# Helper: add log1p-transformed weight edge attribute to a graph
# sensitivity_curve_long() uses the "weight" edge attribute internally.
# Add weight = log1p(sum_trade) to each graph — same transformation used in
# rank selection (log1p), applied here as a continuous weight (no rounding needed).
add_log_weight <- function(g) {
  igraph::E(g)$weight <- log1p(igraph::E(g)$sum_trade)
  g
}


# Helper: split combined df (with "year" column) into a per-year named list,
# and return only the years present in both the trade graphs and the node df.
split_dv_subset <- function(df, trade_graphs) {
  node_list    <- split(df, df$year)
  common_years <- intersect(names(trade_graphs), names(node_list))

  if (length(common_years) < length(names(trade_graphs))) {
    dropped <- setdiff(names(trade_graphs), common_years)
    message("Years with no DV data (dropped from analysis): ",
            paste(dropped, collapse = ", "))
  }

  list(
    trade_graphs = trade_graphs[common_years],
    node_list    = node_list[common_years]
  )
}


# Orchestration: run one sensitivity curve analysis ---------------------------
# One call per model. Data loading and subset alignment are handled in master.
#
# Arguments:
#   formula      : full model formula (DV ~ predictors)
#   trade_graphs : named list of igraph objects per year (raw sum_trade weights)
#   node_data    : named list of node data frames per year
#   output_file  : full path to save the result RDS (directory created if needed)
#   coembedding  : "U" (export side, default) or "V" (import side)


run_sensitivity_curves <- function(formula, trade_graphs, node_data,
                                   output_file, coembedding = "U") {

  trade_graphs_weight <- map(trade_graphs, add_log_weight)

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  curve_result <- imap(
    trade_graphs_weight,
    ~ sensitivity_curve_long(
      graph             = .x,
      formula           = formula,
      node_data         = node_data[[.y]],
      max_rank          = 80,
      ranks_to_consider = 79,
      coembedding       = coembedding,
      clusters          = node_data[[.y]]$name
    )
  )

  saveRDS(curve_result, output_file)
  invisible(curve_result)
}


# Helper: complete-case observation count per year, for a given formula ------
#
# The curve RDS from run_sensitivity_curves() does not store N, so this
# recomputes it from the same node_data: rows with NA on any formula variable
# are dropped (listwise deletion, as in lm()). Trade-embedding columns are not
# checked here.
#
# Arguments:
#   node_data_list : named list of per-year node data frames (same object
#                     passed as node_data to run_sensitivity_curves())
#   formula        : the model formula (e.g. formula_main)
#
# Returns a named-by-year integer vector.
compute_n_obs_by_year <- function(node_data_list, formula) {
  vars <- all.vars(formula)
  purrr::map_int(node_data_list, function(df) {
    df |>
      dplyr::select(dplyr::any_of(vars)) |>
      tidyr::drop_na() |>
      nrow()
  })
}


# Counterfactual trade embedding scenario -------------------------------------
# Fits the outcome and mediator models at a given rank K for one year, then
# predicts y_hat for a "source" country under its own trade embeddings and
# under each "alternative" country's trade embeddings. Returns a tidy data frame of
# predicted values so multiple scenarios can be combined and compared.
#
# Logic (mirrors latentnetmediate internals):
#   1. Compute rank-K SVD embeddings X_K from the weighted adjacency matrix
#   2. Fit outcome model: y ~ W + X_K (W = model matrix from formula)
#   3. Extract betaw_hat (covariate coefs) and betax_hat (embedding coefs)
#   4. For each alternative: y_hat_alternative = betaw_hat %*% W_source + betax_hat %*% X_alternative
#      delta = y_hat_alternative - y_hat_own shows the trade-network-mediated shift
#
# Arguments:
#   trade_graph  : igraph object for the year of interest (raw sum_trade weights)
#                 --> add the "weight" to use log1p(sum_trade) internally
#   node_data    : node data frame for the same year (long by rcid)
#   formula      : model formula used in sensitivity_curve_long()
#   K            : latent rank to use (integer)
#   source_country : name of the country whose W (covariates) are held fixed
#   alternatives   : character vector of countries whose X embeddings are swapped in
#   coembedding    : "U" (export, default) or "V" (import)
#
# Returns a tibble with columns:
#   source_country, alternative, y_actual (mean observed vote), y_hat_own (predicted under
#   own embeddings), y_hat_alternative (predicted under alternative's embeddings),
#   delta (y_hat_alternative - y_hat_own)

predict_counterfactual_trade <- function(trade_graph,
                                         node_data,
                                         formula,
                                         K,
                                         source_country,
                                         alternatives,
                                         coembedding = "U") {

  # ── Build adjacency matrix and compute rank-K embeddings ──────────────────
  g_w <- add_log_weight(trade_graph)
  A   <- igraph::as_adjacency_matrix(g_w, attr = "weight")

  X_K <- if (coembedding == "U") {
    latentnetmediate::US(A, rank = K)
  } else {
    latentnetmediate::VS(A, rank = K)
  }

  node_names <- rownames(A)   # rownames from adjacency matrix, not X_K (US() drops them)
  embed_cols <- paste0("V", seq_len(K))
  X_K_df     <- as.data.frame(X_K, stringsAsFactors = FALSE)
  colnames(X_K_df) <- embed_cols
  X_K_df$name <- node_names

  # ── Join embeddings to node data ──────────────────────────────────────────
  node_joined <- node_data |>
    dplyr::left_join(X_K_df, by = "name")
  # Warn if any country in node_data has no trade embedding
  missing_embed <- node_joined |>
    dplyr::filter(is.na(.data[[embed_cols[1]]])) |>
    dplyr::distinct(name)
  if (nrow(missing_embed) > 0)
    warning("Countries in node_data with no trade embedding (zero-variance column dropped): ",
            paste(missing_embed$name, collapse = ", "))

  # ── Fit outcome model: y ~ W + X_K ───────────────────────────────────────
  # Drop rows with NAs in formula variables OR embeddings before building
  # matrices. lm.fit() takes matrices directly — no model.frame involved,
  # so there is no risk of internal length mismatches.
  formula_vars  <- all.vars(formula)
  complete_rows <- stats::complete.cases(node_joined[, c(formula_vars, embed_cols)])
  # Report how many rows are dropped as incomplete cases
  n_dropped <- sum(!complete_rows)
  if (n_dropped > 0)
    message(sprintf("complete.cases dropped %d rows (%.1f%%) — check for systematic missingness by country type",
                    n_dropped, 100 * n_dropped / nrow(node_joined)))
  node_complete <- node_joined[complete_rows, ]

  mf    <- stats::model.frame(formula, data = node_complete, na.action = na.pass)
  y     <- stats::model.response(mf)
  W     <- stats::model.matrix(formula, mf)
  X_mat <- as.matrix(node_complete[, embed_cols])

  # lm.fit bypasses model.frame entirely — only point estimates needed here
  fit         <- lm.fit(cbind(W, X_mat), y)
  num_w_coefs <- ncol(W)
  betaw_hat   <- fit$coefficients[seq_len(num_w_coefs)]
  betax_hat   <- fit$coefficients[-seq_len(num_w_coefs)]

  # ── Helper: W vector for one country (one covariate row from node_complete) ─
  get_W_vec <- function(country) {
    # Build W for a single country via model.matrix on one row of data.
    # Cannot use colnames(W) directly — model.matrix adds "(Intercept)" and
    # rcid dummies that are not columns in node_complete.
    one_row <- node_complete |>
      dplyr::filter(name == country) |>
      dplyr::slice(1)
    mf_row <- stats::model.frame(formula, data = one_row, na.action = na.pass)
    as.numeric(stats::model.matrix(formula, mf_row)[1, ])
  }

  get_X_vec <- function(country) {
    X_K_df |>
      dplyr::filter(name == country) |>
      dplyr::select(dplyr::all_of(embed_cols)) |>
      as.numeric()
  }

  get_vdem <- function(country) {
    node_complete |>
      dplyr::filter(name == country) |>
      dplyr::pull(v2x_polyarchy) |>
      dplyr::first()
  }

  # ── Source country baseline ───────────────────────────────────────────────
  y_actual    <- node_complete |>
    dplyr::filter(name == source_country) |>
    dplyr::pull(vote_numeric) |>
    mean(na.rm = TRUE)

  vdem_source <- get_vdem(source_country)
  W_source    <- get_W_vec(source_country)
  X_source    <- get_X_vec(source_country)
  y_hat_own   <- as.numeric(betaw_hat %*% W_source + betax_hat %*% X_source)

  # ── Alternative scenarios ───────────────────────────────────────────────────────
  # NIE (trade swap): source keeps its own democracy score throughout.
  # vdem_source and vdem_alternative are both included for reference — neither
  # is swapped, but seeing both helps contextualize the trade position comparison.
  purrr::map_dfr(alternatives, function(alternative) {
    X_alternative     <- get_X_vec(alternative)
    y_hat_alternative <- as.numeric(betaw_hat %*% W_source + betax_hat %*% X_alternative)

    tibble::tibble(
      source_country    = source_country,
      alternative       = alternative,
      vdem_source       = vdem_source,
      vdem_alternative  = get_vdem(alternative),
      y_actual          = y_actual,
      y_hat_own         = y_hat_own,
      y_hat_alternative = y_hat_alternative,
      delta             = y_hat_alternative - y_hat_own
    )
  })
}


# Run counterfactual scenarios across multiple source-alternative pairs and years ---
# Wraps predict_counterfactual_trade() to iterate over a list of scenarios and
# optionally multiple years.
#

# Arguments:
#   scenarios : named list of lists, each with elements:
#                    source (string) and alternatives (character vector)
#                  e.g. list(
#                    Cyprus = list(source = "Cyprus", alternatives = c("Canada", "Germany")),
#                    Moldova = list(source = "Moldova", alternatives = c("Japan", "Belgium"))
#                  )
#   trade_graphs : named list of igraph objects per year
#   node_data    : named list of node data frames per year
#   formula      : model formula
#   K_results    : data frame with columns `year` and the K column
#   K_column     : name of the K column to use from K_results (unquoted)
#   years        : character vector of years to run (default: all in trade_graphs)
#   coembedding  : "U" or "V"
#
# Returns a tibble with columns from predict_counterfactual_trade() plus `year`.

run_counterfactual_scenarios <- function(scenarios,
                                          trade_graphs,
                                          node_data,
                                          formula,
                                          K_results,
                                          K_column,
                                          years       = names(trade_graphs),
                                          coembedding = "U") {

  purrr::map_dfr(years, function(yr) {
    K <- K_results |>
      dplyr::filter(year == yr) |>
      dplyr::pull({{ K_column }})

    purrr::map_dfr(scenarios, function(sc) {
      predict_counterfactual_trade(
        trade_graph    = trade_graphs[[yr]],
        node_data      = node_data[[yr]],
        formula        = formula,
        K              = K,
        source_country = sc$source,
        alternatives   = sc$alternatives,
        coembedding    = coembedding
      )
    }) |>
      dplyr::mutate(trade_year = yr)
  })
}

# Counterfactual democracy (NDE) scenario -------------------------------------
# Holds the source country's trade embeddings (X) fixed and swaps only its
# v2x_polyarchy value in W with the alternative country's value. This isolates
# the direct effect of democracy (NDE path: T → Y not through the network).
#
# Arguments:
#   trade_graph  : igraph object for the year of interest (raw sum_trade weights)
#   node_data    : node data frame for the same year (long by rcid)
#   formula      : model formula
#   K            : latent rank to use (integer)
#   source_country : country whose W and X are held fixed (except democracy)
#   alternatives : countries whose v2x_polyarchy value is swapped into W_source
#   coembedding  : "U" (export, default) or "V" (import)
#
# Returns a tibble with columns:
#   source_country, alternative, y_actual, y_hat_own, y_hat_vdem_swap,
#   delta (y_hat_vdem_swap - y_hat_own)

predict_counterfactual_democracy <- function(trade_graph,
                                              node_data,
                                              formula,
                                              K,
                                              source_country,
                                              alternatives,
                                              coembedding = "U") {

  # ── Build embeddings and fit models (same as trade scenario) ─────────────
  g_w <- add_log_weight(trade_graph)
  A   <- igraph::as_adjacency_matrix(g_w, attr = "weight")

  X_K <- if (coembedding == "U") {
    latentnetmediate::US(A, rank = K)
  } else {
    latentnetmediate::VS(A, rank = K)
  }

  node_names <- rownames(A)   # rownames from adjacency matrix, not X_K (US() drops them)
  embed_cols <- paste0("V", seq_len(K))
  X_K_df     <- as.data.frame(X_K, stringsAsFactors = FALSE)
  colnames(X_K_df) <- embed_cols
  X_K_df$name <- node_names

  node_joined <- node_data |>
    dplyr::left_join(X_K_df, by = "name")

  formula_vars  <- all.vars(formula)
  complete_rows <- stats::complete.cases(node_joined[, c(formula_vars, embed_cols)])
  node_complete <- node_joined[complete_rows, ]

  mf    <- stats::model.frame(formula, data = node_complete, na.action = na.pass)
  y     <- stats::model.response(mf)
  W     <- stats::model.matrix(formula, mf)
  X_mat <- as.matrix(node_complete[, embed_cols])

  fit         <- lm.fit(cbind(W, X_mat), y)
  num_w_coefs <- ncol(W)
  betaw_hat   <- fit$coefficients[seq_len(num_w_coefs)]
  betax_hat   <- fit$coefficients[-seq_len(num_w_coefs)]

  get_W_vec <- function(country) {
    # Build W for a single country via model.matrix on one row of data.
    # Cannot use colnames(W) directly — model.matrix adds "(Intercept)" and
    # rcid dummies that are not columns in node_complete.
    one_row <- node_complete |>
      dplyr::filter(name == country) |>
      dplyr::slice(1)
    mf_row <- stats::model.frame(formula, data = one_row, na.action = na.pass)
    as.numeric(stats::model.matrix(formula, mf_row)[1, ])
  }

  get_X_vec <- function(country) {
    X_K_df |>
      dplyr::filter(name == country) |>
      dplyr::select(dplyr::all_of(embed_cols)) |>
      as.numeric()
  }

  get_vdem <- function(country) {
    node_complete |>
      dplyr::filter(name == country) |>
      dplyr::pull(v2x_polyarchy) |>
      dplyr::first()
  }

  # ── Source baseline ───────────────────────────────────────────────────────
  y_actual    <- node_complete |>
    dplyr::filter(name == source_country) |>
    dplyr::pull(vote_numeric) |>
    mean(na.rm = TRUE)

  vdem_source <- get_vdem(source_country)
  W_source    <- get_W_vec(source_country)
  X_source    <- get_X_vec(source_country)
  y_hat_own   <- as.numeric(betaw_hat %*% W_source + betax_hat %*% X_source)

  # Index of v2x_polyarchy in the model matrix W (matched by column name)
  vdem_idx <- which(grepl("v2x_polyarchy", names(betaw_hat)))
  if (length(vdem_idx) == 0)
    stop("v2x_polyarchy not found in model matrix — check formula")

  # ── Alternative democracy swap: change only W[vdem_idx], keep X_source ──────
  # NDE swap: both the source and alternative vdem scores are included so the
  # output records what value was swapped in vs. what the source started with.
  purrr::map_dfr(alternatives, function(alternative) {
    W_swapped           <- W_source
    W_swapped[vdem_idx] <- get_W_vec(alternative)[vdem_idx]

    y_hat_vdem_swap <- as.numeric(betaw_hat %*% W_swapped + betax_hat %*% X_source)

    tibble::tibble(
      source_country  = source_country,
      alternative     = alternative,
      vdem_source     = vdem_source,
      vdem_alternative = get_vdem(alternative),
      y_actual        = y_actual,
      y_hat_own       = y_hat_own,
      y_hat_vdem_swap = y_hat_vdem_swap,
      delta           = y_hat_vdem_swap - y_hat_own
    )
  })
}


# Run democracy counterfactual scenarios across multiple pairs and years ---
# Same structure as run_counterfactual_scenarios() but calls
# predict_counterfactual_democracy() — swaps democracy in W, not trade in X.
#
# Arguments: identical to run_counterfactual_scenarios()

run_counterfactual_democracy_scenarios <- function(scenarios,
                                                    trade_graphs,
                                                    node_data,
                                                    formula,
                                                    K_results,
                                                    K_column,
                                                    years       = names(trade_graphs),
                                                    coembedding = "U") {

  purrr::map_dfr(years, function(yr) {
    K <- K_results |>
      dplyr::filter(year == yr) |>
      dplyr::pull({{ K_column }})

    purrr::map_dfr(scenarios, function(sc) {
      predict_counterfactual_democracy(
        trade_graph    = trade_graphs[[yr]],
        node_data      = node_data[[yr]],
        formula        = formula,
        K              = K,
        source_country = sc$source,
        alternatives   = sc$alternatives,
        coembedding    = coembedding
      )
    }) |>
      dplyr::mutate(trade_year = yr)
  })
}
