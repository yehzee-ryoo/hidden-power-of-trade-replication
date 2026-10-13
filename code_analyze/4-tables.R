# Regression table functions --------------------------------------------------
# All actual data loading and table building happens in master-run-all.R.
# This file defines three regression-table functions:
#   load_curve_at_K(): load one model's curve RDS, filter to selected years at best K
#   build_estimates_tabular_lines(): helper — the v2x_polyarchy NDE/NIE tabular for one
#                                    set of models, used by build_main_estimates_table()
#   build_main_estimates_table(): compact table: v2x_polyarchy NDE/NIE only, with model
#                                 descriptor rows (✓/✗) for specifications and controls,
#                                 and/or extra per-year rows (year_rows)


# Helper: load a curve RDS, filter to selected years at best K, tag with model label
# Arguments:
#   curve_rds  : path to curve RDS produced by run_sensitivity_curves()
#   model_name : string label added as a "model" column
#   K_column   : column in K_results.rds to use for best-K selection
#   years      : integer vector of years to keep (default: 5 representative years)
#   K_path     : path to K_results.rds
load_curve_at_K <- function(curve_rds,
                             model_name,
                             K_column,
                             years  = c(2000, 2005, 2010, 2015, 2018),
                             K_path = "../data/rank_selection/K_results.rds") {
  K_selected_df <- data.frame(
    year = as.numeric(readRDS(K_path) |> filter(year %in% years) |> pull(year)),
    K    = readRDS(K_path) |> filter(year %in% years) |> pull({{ K_column }})
  )

  bind_rows(readRDS(curve_rds), .id = "year") |>
    mutate(year = as.numeric(year)) |>
    left_join(K_selected_df, by = "year") |>
    filter(rank == K) |>
    mutate(model = model_name)
}


# Helper: build the v2x_polyarchy NDE/NIE (+ optional per-year rows and/or
# descriptor rows) tabular for one set of models, as raw .tex lines
# (\begin{tabular}...\end{tabular}, with \midrule inserted before any
# descriptor section). No caption/label/table wrapper — shared by both the
# single-panel and multi-panel table builders below, since a panel IS just
# this same tabular restricted to a subset of models.
#
# Arguments: same as model_list/col_labels/descriptors/year_rows in
#   build_main_estimates_table() below, restricted to one panel's models.
build_estimates_tabular_lines <- function(model_list, col_labels,
                                          descriptors = NULL, year_rows = NULL) {

  model_names <- names(model_list)

  # ── v2x_polyarchy NDE and NIE, one row per year × estimand ─────────────────
  nde_nie <- bind_rows(model_list) |>
    filter(term == "v2x_polyarchy") |>
    mutate(
      value     = sprintf(
        "%.3f%s [%.3f, %.3f]",
        estimate,
        ifelse(conf.low > 0 | conf.high < 0, "***", ""),
        conf.low, conf.high
      ),
      row_label = ifelse(estimand == "nde", "NDE", "NIE"),
      row_order = ifelse(estimand == "nde", 1L, 2L)
    ) |>
    select(year, model, row_label, row_order, value)

  # ── Optional extra rows (year_rows), shown under NDE/NIE in each year ───
  extra_rows <- NULL
  if (!is.null(year_rows)) {
    displayed_years <- unique(nde_nie$year)

    extra_rows <- purrr::imap_dfr(year_rows, function(by_model, row_label) {
      purrr::imap_dfr(by_model, function(by_year, model_name) {
        tibble::tibble(
          year  = as.numeric(names(by_year)),
          model = model_name,
          value = unname(as.character(by_year))
        )
      }) |>
        dplyr::mutate(row_label = row_label)
    }) |>
      # Keep only the years shown in the table
      dplyr::filter(year %in% displayed_years) |>
      dplyr::mutate(row_order = 2L + match(row_label, names(year_rows)))
  }

  estimates <- bind_rows(nde_nie, extra_rows) |>
    select(year, row_label, row_order, model, value) |>
    tidyr::pivot_wider(names_from = model, values_from = value) |>
    arrange(year, row_order) |>
    mutate(year = ifelse(row_order == 1, as.character(year), "")) |>
    select(-row_order) |>
    rename(Year = year, ` ` = row_label)

  # Rename model columns to display labels
  names(estimates) <- dplyr::recode(names(estimates), !!!col_labels)

  full_table <- estimates
  n_est      <- nrow(estimates)

  # ── Optional descriptor rows (one value per model), shown at the bottom ──
  if (!is.null(descriptors)) {
    # Use tibble() with !!! splicing to preserve special column names like
    # "(1) Main" — cbind/as.data.frame would sanitize them to "X.1..Main".
    desc_rows <- imap_dfr(descriptors, function(values, desc_name) {
      model_vals        <- as.list(values[model_names])
      names(model_vals) <- col_labels[names(model_vals)]
      tibble::tibble(Year = "", ` ` = desc_name, !!!model_vals)
    })

    sep_row <- tibble::tibble(
      !!!setNames(as.list(rep("", ncol(estimates))), names(estimates))
    )

    full_table <- bind_rows(estimates, sep_row, desc_rows)
  }

  # ── Build LaTeX lines manually to insert \midrule between sections ─────────
  n_cols <- ncol(full_table)

  tabular <- knitr::kable(
    full_table,
    format   = "latex",
    booktabs = TRUE,
    align    = c("l", "l", rep("c", n_cols - 2)),
    linesep  = ""          # suppress booktabs default inter-group spacing
  )

  tex_lines <- strsplit(as.character(tabular), "\n")[[1]]

  if (!is.null(descriptors)) {
    # Insert \midrule before the descriptor section (after estimates + blank row)
    data_start <- which(grepl("^2", tex_lines))[1]   # first data row
    midrule_at <- data_start + n_est                  # after last estimate row
    tex_lines  <- append(tex_lines, "\\midrule", after = midrule_at)
  }

  tex_lines
}


# Compact main-results table --------------------------------------------------
# Shows only v2x_polyarchy NDE and NIE. Rows = years. Columns = one per model.
# NDE and NIE are stacked within each year block. Bottom panel shows model
# descriptors (trade network, DV, controls included) as ✓/✗ rows.
#
# Arguments:
#   model_list    : named list of data frames from load_curve_at_K().
#                   Names become column headers.
#   col_labels    : named character vector mapping model names to display labels
#                   (e.g. c(main_complete = "Main", import_complete = "Import"))
#   descriptors   : named list of named character vectors — one entry per
#                   descriptor row, mapping model names to display values
#                   (e.g. list("Trade network" = c(main_complete = "Complete", ...),
#                              "Importantvote" = c(main_complete = "No", ...)))
#   output_path   : full path for the saved .tex file
#   year_rows     : optional extra rows that differ by year, shown under
#                   NDE/NIE in each year's block. A named list (row label) of
#                   named lists (model) of named-by-year values, e.g.
#                     list("N (Excl.)" = list(main_complete = c(`2000` = "2754 (0)", ...), ...))
build_main_estimates_table <- function(model_list,
                                       col_labels,
                                       descriptors  = NULL,
                                       output_path,
                                       caption      = NULL,
                                       label        = NULL,
                                       year_rows    = NULL) {

  note <- "\\par\\smallskip\\footnotesize $^{***}$~95\\% CI excludes zero ($p < 0.05$, two-sided)."

  tex_lines <- build_estimates_tabular_lines(model_list, col_labels, descriptors, year_rows)

  if (!is.null(caption)) {
    cap_line   <- sprintf("\\caption{%s}", caption)
    label_line <- if (!is.null(label)) sprintf("\\label{%s}", label) else NULL
    header     <- c("\\begin{table}[htbp]\\centering",
                    cap_line, label_line)
    footer     <- "\\end{table}"
    tex_lines  <- c(header, tex_lines, note, footer)
    note       <- NULL   # already included above
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(tex_lines, note), output_path)
  message("Table saved to: ", output_path)

  invisible(NULL)
}



# Appendix tables ---------------------------------------------------------

# build_resolution_example_table()
#
# Builds a sample of UNGA resolution rows for the appendix. Randomly samples
# n_per_year rows per year from the full resolution pool, then appends n_must
# rows from the CD/RD subset (6_robustness_check.R) to guarantee representation.
# Duplicates are removed before sorting.
#
# Arguments:
#   n_per_year  : number of random rows sampled per year from full pool (default 1)
#   n_must      : number of mandatory CD/RD rows to include (default 3)
#   seed        : RNG seed for reproducibility
#   output_path : full path for the saved .tex file (NULL = no file saved)
#   caption     : LaTeX caption string (NULL = tabular body only)
#   label       : LaTeX label for \ref{} (NULL = omitted)
build_resolution_example_table <- function(n_per_year  = 1L,
                                           n_must      = 3L,
                                           seed        = NULL,
                                           output_path = NULL,
                                           caption     = NULL,
                                           label       = NULL) {

  # Full pool: all resolutions across all years
  all_nodes <- readRDS("../data/node-trade-aligned-complete.rds") |>
    map(\(x) x$nodes) |>
    bind_rows() |>
    select(rcid, year_DV, short, descr) |>
    distinct()

  # CD/RD mandatory pool
  cd_rd_nodes <- readRDS("../data/dv_subset/node_only_CD_RD.rds") |>
    select(rcid, year_DV, short, descr) |>
    distinct()

  # ── Random sample: one row per year from full pool ───────────────────────────
  set.seed(seed)
  random_rows <- all_nodes |>
    filter(!is.na(short) & short != "") |>
    select(RCID = rcid, Year = year_DV, Short = short, Description = descr) |>
    group_by(Year) |>
    slice_sample(n = n_per_year) |>
    ungroup()

  # ── Mandatory: sample n_must rows from CD/RD pool ───────────────────────────
  set.seed(seed)
  must_rows <- cd_rd_nodes |>
    filter(!is.na(short) & short != "") |>
    select(RCID = rcid, Year = year_DV, Short = short, Description = descr) |>
    slice_sample(n = min(n_must, nrow(cd_rd_nodes)))

  # ── Combine, deduplicate, sort ───────────────────────────────────────────────
  example_table <- bind_rows(random_rows, must_rows) |>
    distinct() |>
    arrange(Year, RCID)

  # ── Render data rows: escape LaTeX special characters ───────────────────────
  escape_latex <- function(x) {
    x |>
      gsub("\\\\", "\\\\textbackslash{}", x = _, fixed = TRUE) |>
      gsub("&",  "\\&",  x = _, fixed = TRUE) |>
      gsub("%",  "\\%",  x = _, fixed = TRUE) |>
      gsub("\\$", "\\$", x = _, fixed = TRUE) |>
      gsub("#",  "\\#",  x = _, fixed = TRUE) |>
      gsub("_",  "\\_",  x = _, fixed = TRUE) |>
      gsub("\\{", "\\{", x = _, fixed = TRUE) |>
      gsub("\\}", "\\}", x = _, fixed = TRUE) |>
      gsub("~",  "\\textasciitilde{}", x = _, fixed = TRUE) |>
      gsub("\\^", "\\textasciicircum{}", x = _, fixed = TRUE)
  }

  data_rows <- example_table |>
    mutate(across(where(is.character), escape_latex)) |>
    glue::glue_data("{RCID} & {Year} & {Short} & {Description}\\\\")

  # Insert \addlinespace every 5 rows (matching the example style)
  row_idx   <- seq_along(data_rows)
  add_space <- which(row_idx %% 5 == 0 & row_idx != length(data_rows))
  for (i in rev(add_space)) {
    data_rows <- append(data_rows, "\\addlinespace", after = i)
  }

  # ── Assemble longtable with p{} column specs and continuation headers ────────
  cap_str   <- if (!is.null(caption)) sprintf("\\caption{%s}", caption) else "\\caption{}"
  label_str <- if (!is.null(label))   sprintf("\\label{%s}", label)     else ""

  tex_lines <- c(
    "\\begin{longtable}[t]{",
    "  >{\\raggedright\\arraybackslash}p{0.8cm}",
    "  >{\\raggedright\\arraybackslash}p{0.8cm}",
    "  >{\\raggedright\\arraybackslash}p{4cm}",
    "  >{\\raggedright\\arraybackslash}p{7.5cm}",
    "}",
    sprintf("%s%s\\\\", cap_str, label_str),
    "\\toprule",
    "RCID & Year & Short & Description\\\\",
    "\\midrule",
    "\\endfirsthead",
    "",
    "\\multicolumn{4}{c}{{\\tablename\\ \\thetable{} -- continued from previous page}}\\\\",
    "\\toprule",
    "RCID & Year & Short & Description\\\\",
    "\\midrule",
    "\\endhead",
    "",
    "\\midrule",
    "\\multicolumn{4}{r}{{Continued on next page}}\\\\",
    "\\endfoot",
    "",
    "\\bottomrule",
    "\\endlastfoot",
    "",
    data_rows,
    "\\end{longtable}"
  )

  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(tex_lines, output_path)
    message("Table saved to: ", output_path)
  }

  invisible(example_table)
}


# -----Summary statistics table

# build_summary_stats_table()
#
# Builds a summary statistics table (min, mean, max, N) for the main
# explanatory variables and covariates, across selected years.
#
# Arguments:
#   node_trade_path : path to node-trade-aligned-complete.rds
#   years           : integer vector of years to include (default: all 2000-2018)
#   output_path     : full path for the saved .tex file (NULL = no file saved)
#   caption         : LaTeX caption string (NULL = omitted)
#   label           : LaTeX label for \ref{} (NULL = omitted)

build_summary_stats_table <- function(node_trade_path = "../data/node-trade-aligned-complete.rds",
                                      years           = 2000:2018,
                                      output_path     = NULL,
                                      caption         = NULL,
                                      label           = NULL) {

  # Load and bind node data across selected years
  aligned <- readRDS(node_trade_path)
  node_df <- map(aligned[as.character(years)], \(x) x$nodes) |>
    bind_rows() |>
    group_by(name, year_IV) |>
    slice(1) |>       # one row per country per year (node data is long by rcid)
    ungroup() |>
    mutate(year_IV = as.character(year_IV)) |>   # treat as character so N = unique years
    dplyr::select(
      year_IV,
      name,
      v2x_polyarchy,
      l.gdp_s,
      l.pop_s,
      HR_index_s,
      interstate_conflicts_lag0,
      intrastate_conflicts_lag0,
      interstate_conflicts_lag1,
      intrastate_conflicts_lag1
    )

  # Summarise each variable
  summarize_variable <- function(x) {
    if (is.numeric(x)) {
      tibble(
        Min  = round(min(x,  na.rm = TRUE), 2),
        Mean = round(mean(x, na.rm = TRUE), 2),
        Max  = round(max(x,  na.rm = TRUE), 2),
        N    = sum(!is.na(x))
      )
    } else {
      tibble(Min = NA_real_, Mean = NA_real_, Max = NA_real_, N = n_distinct(x))
    }
  }

  options(scipen = 999)

  summary_stats <- node_df |>
    map_dfr(summarize_variable, .id = "Variable") |>
    mutate(across(c(Min, Mean, Max), ~ ifelse(is.na(.), "-", as.character(.))))

  # Rename variables to display labels
  label_map <- c(
    year_IV                    = "Year IV (democracy measured, year t)",
    name                       = "Country",
    v2x_polyarchy              = "V-Dem Polyarchy Score",
    l.gdp_s                    = "Log GDP (scaled)",
    l.pop_s                    = "Log Population (scaled)",
    HR_index_s                 = "Human Rights Index (scaled)",
    interstate_conflicts_lag0  = "Interstate Conflict Count (year t-1)",
    intrastate_conflicts_lag0  = "Intrastate Conflict Count (year t-1)",
    interstate_conflicts_lag1  = "Interstate Conflict Count (year t-2)",
    intrastate_conflicts_lag1  = "Intrastate Conflict Count (year t-2)"
  )

  year_iv_vals <- sort(unique(as.integer(node_df$year_IV)))
  summary_stats <- summary_stats |>
    mutate(
      Variable = dplyr::recode(Variable, !!!label_map),
      Min      = ifelse(Variable == label_map[["year_IV"]], as.character(min(year_iv_vals)), Min),
      Max      = ifelse(Variable == label_map[["year_IV"]], as.character(max(year_iv_vals)), Max)
    )

  # Build LaTeX table
  n_stat_cols <- ncol(summary_stats) - 1L

  # Pass caption and label directly to kable so kableExtra produces a single
  # \begin{table} float. Adding an outer wrapper manually creates a nested
  # \begin{table} inside kableExtra's own float, which is invalid LaTeX.
  tab <- knitr::kable(
    summary_stats,
    format   = "latex",
    booktabs = TRUE,
    align    = c("l", rep("c", n_stat_cols)),
    linesep  = "",
    caption  = caption,
    label    = label
  ) |>
    kableExtra::kable_styling(
      latex_options = c("striped", "scale_down", "hold_position")
    ) |>
    kableExtra::add_header_above(c(" " = 1, "Summary Statistics" = n_stat_cols)) |>
    kableExtra::row_spec(0, bold = TRUE)

  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(as.character(tab), output_path)
    message("Table saved to: ", output_path)
  }

  invisible(summary_stats)
}


# build_pc2_trade_regression_table()
#
# Regresses PC2 scores on two log-trade predictors for a single year and saves
# the result as a stargazer .tex file.
#
# Predictors:
#   l_mix = log(imports) - log(exports)  — import orientation (+ = import-heavy)
#   l_tot = log(imports + exports)       — total trade volume (log scale)
#
# Arguments:
#   scores_df   : data frame from build_pca_scores(), must have columns
#                 `name`, `year`, and the column named by pc2_col
#   trade_graph : igraph object for the chosen year, with edge attribute `sum_trade`
#   year        : year to subset from scores_df (must match trade_graph)
#   pc2_col     : name of the PC2 column to use as outcome (default "PC2_flipped")
#   output_path : full path for the saved .tex file
#   caption     : LaTeX table caption string
#   label       : LaTeX table label for \ref{}

build_pc2_trade_regression_table <- function(
    scores_df,
    trade_graph,
    year,
    pc2_col     = "PC2_flipped",
    output_path,
    caption     = "Regression of PC2 on Trade Orientation and Volume",
    label       = "tab:pc2_trade_regression"
) {

  year <- as.character(year)

  # ── Trade strength from igraph ───────────────────────────────────────────────
  exports <- igraph::strength(trade_graph, mode = "out",
                              weights = igraph::E(trade_graph)$sum_trade, loops = FALSE)
  imports <- igraph::strength(trade_graph, mode = "in",
                              weights = igraph::E(trade_graph)$sum_trade, loops = FALSE)

  trade_df <- data.frame(
    name    = names(exports),
    exports = as.numeric(exports),
    imports = as.numeric(imports),
    row.names = NULL
  )

  # ── Build model data frame ───────────────────────────────────────────────────
  df <- scores_df |>
    dplyr::filter(as.character(.data$year) == .env$year, !is.na(name)) |>
    dplyr::left_join(trade_df, by = "name") |>
    dplyr::mutate(
      l_imports = log1p(imports),
      l_exports = log1p(exports),
      imp_share = imports / (imports + exports)
    )

  # Model 1: log imports alone
  model_imports <- lm(paste(pc2_col, "~ l_imports"), data = df)

  # Model 2: log exports alone
  model_exports <- lm(paste(pc2_col, "~ l_exports"), data = df)

  # Model 3: import share (net importer vs exporter direction)
  model_share   <- lm(paste(pc2_col, "~ imp_share"), data = df)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  stargazer::stargazer(
    model_imports, model_exports, model_share,
    type                   = "latex",
    title                  = caption,
    label                  = label,
    digits                 = 3,
    out                    = output_path,
    covariate.labels       = c("Log imports", "Log exports", "Import share"),
    dep.var.caption        = "Dependent variable: PC2 (sign-aligned)",
    dep.var.labels.include = FALSE,
    header                 = FALSE
  )

  # Wrap in \begingroup...\endgroup and explicitly set all rows to white,
  # overriding cup-journal's alternating row shading from xcolor.
  # The group ensures the reset does not bleed into the rest of the document.
  tex_lines <- readLines(output_path)
  writeLines(
    c("\\begingroup", "\\rowcolors{1}{white}{white}", tex_lines, "\\endgroup"),
    output_path
  )

  message("Table saved to: ", output_path)
  invisible(list(imports = model_imports, exports = model_exports, import_share = model_share))
}


# ---- Lagged years summary table
# build_lagged_years_table()
#
# Builds a table showing the staggered year structure used in the analysis:
# covariates (t-1), democracy/IV (t), trade network (t+1), UNGA votes (t+2),
# and the number of observations per year combination.
#
# Arguments:
#   node_trade_path : path to node-trade-aligned-complete.rds
#   years           : integer vector of trade years to include (default: 2000-2018)
#   output_path     : full path for the saved .tex file (NULL = no file saved)
#   caption         : LaTeX caption string (NULL = omitted)
#   label           : LaTeX label string, without "tab:" prefix (NULL = omitted)

build_lagged_years_table <- function(node_trade_path = "../data/node-trade-aligned-complete.rds",
                                      years           = 2000:2018,
                                      output_path     = NULL,
                                      caption         = NULL,
                                      label           = NULL) {

  year_summary <- readRDS(node_trade_path) |>
    purrr::map(~ .x$nodes) |>
    dplyr::bind_rows() |>
    dplyr::filter((as.integer(year_IV) + 1L) %in% years) |>
    dplyr::mutate(
      year_cov   = as.integer(year_IV) - 1L,
      trade_year = as.integer(year_IV) + 1L,
      year_DV    = as.integer(year_IV) + 2L
    ) |>
    dplyr::distinct(name, year_cov, year_IV, trade_year, year_DV) |>
    dplyr::group_by(year_cov, year_IV, trade_year, year_DV) |>
    dplyr::summarise(n_countries = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(trade_year)

  n_cols <- ncol(year_summary)

  tab <- knitr::kable(
    year_summary,
    format    = "latex",
    booktabs  = TRUE,
    col.names = c("Covariates (t-1)", "Democracy (t)",
                  "Trade Network (t+1)", "UNGA Votes (t+2)",
                  "N Countries"),
    align     = c(rep("c", n_cols)),
    linesep   = "",
    caption   = caption,
    label     = label
  ) |>
    kableExtra::kable_styling(
      latex_options = c("striped", "scale_down", "hold_position")
    ) |>
    kableExtra::add_header_above(
      c("Summary of Year Structure Used in Analysis" = n_cols)
    ) |>
    kableExtra::row_spec(0, bold = TRUE)

  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(as.character(tab), output_path)
    message("Table saved to: ", output_path)
  }

  invisible(year_summary)
}


