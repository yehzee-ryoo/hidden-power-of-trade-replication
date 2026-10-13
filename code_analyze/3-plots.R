# NDE and NIE time trend — all years in one panel -------------------------
# Extracts estimates at each year's best K and plots them together so you
# can see how direct and indirect effects evolve over the full time period.

plot_nde_nie_time_trend <- function(
    curve_results,
    years      = 2000:2018,
    K_list     = NULL,
    K_path     = "../data/rank_selection/K_results.rds",
    K_column,
    term,
    subtitle   = NULL,
    output_dir,
    filename   = "time_trend_NDE_NIE.png",
    ylim_range = c(-1, 1)
) {

  # Load K_list if not supplied
  if (is.null(K_list)) {
    K_selected <- readRDS(K_path)
    K_list <- K_selected %>%
      filter(year %in% years) %>%
      pull({{ K_column }})
  }

  stopifnot(length(K_list) == length(years))

  # Extract best-K estimates for each year and bind into one data frame
  plot_data <- map2_dfr(years, K_list, function(yr, K) {
    curve_results[[as.character(yr)]] %>%
      filter(rank == K, term == .env$term) %>%
      mutate(year = yr)
  })

  p <- ggplot(plot_data, aes(x = factor(year), y = estimate, color = estimand)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width    = 0.3,
                  size     = 0.8,
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    labs(
      title    = "Direct and Indirect Effects of Democracy over Time",
      subtitle = subtitle,
      x        = "Year",
      y        = "Estimate",
      color    = "Estimand"
    ) +
    scale_color_manual(
      values = c("nde" = "#007f33", "nie" = "#F8766D"),
      labels = c("nde" = "NDE (Direct)", "nie" = "NIE (Indirect)")
    ) +
    theme_classic() +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    ylim(ylim_range[1], ylim_range[2])

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = file.path(output_dir, filename),
    plot     = p,
    width    = 7,
    height   = 5
  )

  invisible(p)
}



# Sensitivity all rank K --------------------------------------------------
# Produces one PNG per year showing NDE and NIE across all ranks (2–80).
# If K_results and K_column are supplied, a dark gray dotted vertical line is
# drawn at the optimal K for each year.
#
# Arguments:
#   curve_results : named list of rank_sensitivity_curve objects (one per year)
#   term          : predictor term to plot
#   output_dir    : directory to save PNGs
#   K_results     : (optional) data frame with columns `year` and a K column
#   K_column      : (optional) string name of the K column in K_results

latent_estimates_plot <- function(curve_results,
                                  term,
                                  output_dir,
                                  K_results = NULL,
                                  K_column  = NULL) {
  if (!is.null(output_dir))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  imap(curve_results, function(curve_obj, year_label) {
    # dplyr::filter strips the rank_sensitivity_curve class; restore it so
    # plot() dispatches to plot.rank_sensitivity_curve (returns a ggplot).
    curve_term <- dplyr::filter(curve_obj, .data$term == .env$term)
    class(curve_term) <- c("rank_sensitivity_curve", class(curve_term))

    p <- plot(curve_term) +
      theme_classic() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = NULL)

    # Add dark gray dotted vertical line at optimal K if K_results supplied
    if (!is.null(K_results) && !is.null(K_column)) {
      best_k <- K_results |>
        dplyr::filter(as.character(year) == as.character(year_label)) |>
        dplyr::pull(.data[[K_column]])
      if (length(best_k) == 1 && !is.na(best_k))
        p <- p + geom_vline(xintercept = best_k, linetype = "dotted",
                            color = "gray30", linewidth = 0.8)
    }

    if (!is.null(output_dir))
      ggsave(
        filename = file.path(output_dir, paste0("sensitivity_", year_label, ".png")),
        plot     = p,
        width    = 7,
        height   = 5
      )

    invisible(p)
  })
}


# Wrapper: plots for one model ----------------------------------------------
# Loads the curve RDS, then runs:
#   (1) plot_nde_nie_time_trend   → all-years trend panel
#   (2) latent_estimates_plot     → sensitivity across all K ranks, only for
#                                   the years in sensitivity_years (if any)
# Sensitivity plots go into sensitivity_dir (default: output_dir/sensitivity/).
#
# Required arguments (vary per model):
#   curve_rds            : path to the .rds file produced by run_sensitivity_curves()
#   K_column             : column in K_results.rds to use for best-K selection
#                          (e.g. "K_complete_lp1" or "K_incomplete_lp1")
#   output_dir           : base directory for this model's plots
#   time_trend_subtitle  : subtitle shown on the all-years time-trend panel only
#   time_trend_filename  : unique PNG filename for the time-trend panel
#                          (must differ across models to avoid overwriting)
#
# Optional:
#   sensitivity_years    : years to draw sensitivity plots for (NULL = none)
#   sensitivity_dir      : directory for the sensitivity plots
#
# Static defaults (shared across all models):
#   term, ylim_range, K_path

plot_model_results <- function(
    curve_rds,
    K_column,
    output_dir,
    time_trend_subtitle,
    time_trend_filename,
    sensitivity_years = NULL,
    sensitivity_dir   = file.path(output_dir, "sensitivity"),
    term       = "v2x_polyarchy",
    ylim_range = c(-1, 1),
    K_path     = "../data/rank_selection/K_results.rds"
) {
  curve_results <- readRDS(curve_rds)
  years         <- as.integer(names(curve_results))

  plot_nde_nie_time_trend(
    curve_results = curve_results,
    years         = years,
    term          = term,
    K_column      = K_column,
    subtitle      = time_trend_subtitle,
    output_dir    = output_dir,
    filename      = time_trend_filename,
    ylim_range    = ylim_range,
    K_path        = K_path
  )

  if (!is.null(sensitivity_years)) {
    K_results <- readRDS(K_path)
    latent_estimates_plot(
      curve_results = curve_results[as.character(sensitivity_years)],
      term          = term,
      output_dir    = sensitivity_dir,
      K_results     = K_results,
      K_column      = K_column
    )
  }

  invisible(NULL)
}



# Trade network plot ------------------------------------------------------
# Produces two plots for a single year's trade graph:
#   (1) Full network (all edges), node size = total exports
#   (2) Top-k edges per source node, node fill = v2x_polyarchy score,
#       with optional highlight rings around target countries
#
# Arguments:
#   trade_graph : igraph object with edge attribute `sum_trade` and node
#                 attribute `v2x_polyarchy`
#   year        : integer year label used in titles and filenames
#   targets     : character vector of node names to highlight with bold
#                 labels and outline rings (default: none)
#   top_k       : edges to keep per source node in the top-k plot (default 5)
#   seed        : RNG seed for reproducible layouts (default 50)
#   output_dir  : directory for saved PDF files
plot_trade_network <- function(
    trade_graph,
    year,
    node_data        = NULL,   # optional data frame with columns `name` and `v2x_polyarchy`
    targets          = character(0),
    top_k            = 5L,
    seed             = NULL,
    reference_layout = NULL,   # data frame with `name`, `x`, `y` from a prior call
    output_dir
) {

  year <- as.integer(year)

  # ── Attach v2x_polyarchy from node_data if provided ─────────────────────────
  if (!is.null(node_data)) {
    polyarchy_lookup <- node_data |>
      dplyr::select(name, v2x_polyarchy) |>
      dplyr::distinct()
    node_order <- igraph::V(trade_graph)$name
    polyarchy_vals <- polyarchy_lookup$v2x_polyarchy[
      match(node_order, polyarchy_lookup$name)
    ]
    trade_graph <- igraph::set_vertex_attr(trade_graph, "v2x_polyarchy", value = polyarchy_vals)
  }

  # ── Compute and attach export / import strength ──────────────────────────────
  exports <- igraph::strength(trade_graph, mode = "out",
                              weights = igraph::E(trade_graph)$sum_trade, loops = FALSE)
  trade_graph <- igraph::set_vertex_attr(trade_graph, "exports", value = exports)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # ── Top-k edges per source node ──────────────────────────────────────────────
  g_topk <- trade_graph |>
    tidygraph::as_tbl_graph() |>
    tidygraph::activate(edges) |>
    dplyr::group_by(from) |>
    dplyr::slice_max(order_by = sum_trade, n = top_k, with_ties = FALSE) |>
    dplyr::ungroup() |>
    tidygraph::activate(nodes) |>
    dplyr::filter(!tidygraph::node_is_isolated())

  set.seed(seed)
  layout_coords <- ggraph::create_layout(g_topk, layout = "drl")

  if (!is.null(reference_layout)) {
    matched  <- match(layout_coords$name, reference_layout$name)
    has_ref  <- !is.na(matched)
    # Pin shared nodes to reference positions
    layout_coords$x[has_ref] <- reference_layout$x[matched[has_ref]]
    layout_coords$y[has_ref] <- reference_layout$y[matched[has_ref]]
    # Place new nodes at centroid so they don't distort the plot
    layout_coords$x[!has_ref] <- mean(reference_layout$x, na.rm = TRUE)
    layout_coords$y[!has_ref] <- mean(reference_layout$y, na.rm = TRUE)
  }

  lab_df <- layout_coords |>
    dplyr::filter(is.finite(x), is.finite(y), !is.na(name), name != "") |>
    dplyr::mutate(font_size = ifelse(name %in% targets, 4, 3))

  ring_df <- dplyr::filter(lab_df, name %in% targets)

  p_topk <- ggraph(layout_coords) +
    geom_edge_link0(aes(width = sum_trade), alpha = 0.4, colour = "darkgrey", show.legend = FALSE) +
    geom_node_point(aes(size = exports, fill = v2x_polyarchy),
                    shape = 21, colour = "darkgrey", stroke = 0.3, alpha = 0.8) +
    geom_point(
      data = ring_df,
      aes(x = x, y = y, size = exports),
      shape = 21, fill = NA, colour = "black", stroke = 1.2,
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    ggrepel::geom_text_repel(
      data = lab_df,
      aes(x = x, y = y, label = name,
          fontface = ifelse(name %in% targets, "bold", "plain")),
      size = lab_df$font_size,
      max.overlaps = Inf,
      box.padding = 0.5, point.padding = 0.5, force = 0.8,
      max.iter = 1000, max.time = 0.3, min.segment.length = 0,
      segment.color = "black", segment.size = 0.3,
      show.legend = FALSE
    ) +
    scale_size_continuous(name = "Total export size", range = c(1, 10), trans = "log10") +
    scale_fill_distiller(palette = "RdYlBu", direction = 1, na.value = "white") +
    theme_void() +
    labs(title = sprintf("Trade Network in %d (Top-%d edges per node)", year, top_k))

  ggsave(
    filename = file.path(output_dir, sprintf("trade_%d_top%d_exp.pdf", year, top_k)),
    plot = p_topk, width = 12, height = 10, units = "in", dpi = 300
  )

  invisible(list(
    topk   = p_topk,
    layout = dplyr::select(layout_coords, name, x, y)
  ))
}

# PCA plots ---------------------------------------------------------------

# build_pca_scores() : computes PCA from A_list, joins node covariates, returns a tidy data frame (one row per node per year)
# plot_pca_trade_network() : faceted PC1/PC2 scatter coloured by v2x_polyarchy, with optional label countries and per-year axis flips

# Build tidy PCA scores from a named list of sparse adjacency matrices.
#
# Arguments:
#   A_list : named list of sparse matrices (one per year), as built by igraph::as_adjacency_matrix() + log transform
#   node_list : named list of node data frames (one per year), each with  columns `name` and `polyarchy_col`
#   n_pcs : number of principal components to retain (default 2)
#   polyarchy_col : name of the democracy covariate column in node_list
#
# Returns a data frame with columns: name, year, PC1, PC2 (…PCn), node_index, and the covariate column renamed to "polyarchy"

# Helper used by build_pca_scores when ref_country is supplied.
# Returns +1 (no flip) or -1 (flip) based on the sign of ref_country's PC2
# loading. Falls back to +1 if the country is absent from the matrix.
pc2_flip_sign <- function(pca_obj, node_names, ref_country) {
  if (!ref_country %in% node_names) return(1L)
  sign_val <- sign(pca_obj$rotation[ref_country, "PC2"])
  if (sign_val == 0L) 1L else as.integer(sign_val)
}

# Per-year PCA worker: runs prcomp on one adjacency matrix and returns a tidy
# data frame of scores joined to node covariates.
pca_scores_one_year <- function(A, yr, node_list, n_pcs, polyarchy_col, ref_country) {

  A@x     <- as.numeric(A@x)
  mat     <- as.matrix(A)
  sds     <- apply(mat, 2, sd)
  keep    <- which(sds > 0)
  pca_obj <- prcomp(mat[keep, keep, drop = FALSE], scale. = TRUE, center = TRUE)

  pc_cols           <- pca_obj$x[, seq_len(n_pcs), drop = FALSE]
  colnames(pc_cols) <- paste0("PC", seq_len(n_pcs))

  nm        <- colnames(mat)
  node_name <- if (!is.null(nm)) nm[keep] else paste0("node_", keep)

  pcs <- as.data.frame(pc_cols) |>
    dplyr::mutate(year = yr, node_index = keep, name = node_name)

  node_cov <- node_list[[yr]] |>
    dplyr::select(name, polyarchy = dplyr::all_of(polyarchy_col)) |>
    dplyr::distinct()

  result <- dplyr::left_join(pcs, node_cov, by = "name")

  # Attach PC2_flipped when a reference country is supplied for sign alignment
  if (!is.null(ref_country)) {
    flip      <- pc2_flip_sign(pca_obj, node_name, ref_country)
    result$PC2_flipped <- result$PC2 * flip
  }

  result
}

build_pca_scores <- function(A_list,
                             node_list,
                             n_pcs         = 2L,
                             polyarchy_col = "v2x_polyarchy",
                             ref_country   = NULL) {
  # ref_country : optional country name used to align PC2 sign across years.
  #   Each year's PC2 loading for this country is checked; if negative the
  #   whole axis is flipped, so PC2_flipped always points in the same canonical
  #   direction. Grenada is used for the complete trade graph (its PC2 loading
  #   is among the largest and most stable across years).
  #   When NULL (default), no sign alignment is done and PC2_flipped is not
  #   added.

  purrr::imap(A_list, \(A, yr)
    pca_scores_one_year(A, yr, node_list, n_pcs, polyarchy_col, ref_country)
  ) |> dplyr::bind_rows()
}

# Plot PCA trade network: faceted scatter of PC1 vs PC2, coloured by polyarchy.
#
# Arguments:
#   scores_df      : data frame from build_pca_scores()
#   to_label       : character vector of country names to annotate
#   flip_pc2_years : character vector of years where PC2 should be sign-flipped
#                    (PCA axes are sign-arbitrary; this aligns panels visually)
#   flip_pc1_years : same for PC1
#   output_dir     : directory for saved PDF
#   filename       : output filename (default "PCA_allyears.pdf")
#   width, height  : ggsave dimensions in inches

plot_pca_trade_network <- function(
    scores_df,
    to_label = character(0), 
    flip_pc2_years = character(0),
    flip_pc1_years = character(0),
    output_dir,
    filename       = "PCA_allyears.pdf",
    width          = 10,
    height         = 8) {

  plot_df <- scores_df |>
    dplyr::filter(!is.na(polyarchy)) |>
    mutate(
      PC1_fixed = ifelse(as.character(year) %in% as.character(flip_pc1_years), -PC1, PC1),
      PC2_fixed = ifelse(as.character(year) %in% as.character(flip_pc2_years), -PC2, PC2)
    )

  lab_df <- plot_df |>
    dplyr::filter(name %in% to_label) |>
    dplyr::select(name, year, PC1_fixed, PC2_fixed)

  xlim_r <- range(plot_df$PC1_fixed, na.rm = TRUE)
  ylim_r <- range(plot_df$PC2_fixed, na.rm = TRUE)

  p <- ggplot(plot_df, aes(PC1_fixed, PC2_fixed, color = polyarchy)) +
    geom_point() +
    ggrepel::geom_text_repel(
      data           = lab_df,
      aes(label      = name),
      size           = 3,
      seed           = 42,
      na.rm          = TRUE,
      color          = "black",
      segment.color  = "black",
      segment.size   = 0.4,
      segment.linetype = "dotted",
      point.padding  = 0,
      box.padding    = 0.6,
      max.overlaps   = Inf,
      force          = 5
    ) +
    facet_wrap(~ year, scales = "fixed") +
    scale_color_distiller(palette = "RdYlBu", direction = 1, na.value = "grey80",
                          name = "Democracy\n(v2x_polyarchy)") +
    coord_cartesian(xlim = xlim_r, ylim = ylim_r, clip = "off") +
    labs(x = "PC1", y = "PC2") +
    theme_bw()

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = file.path(output_dir, filename),
    plot     = p,
    width    = width,
    height   = height,
    units    = "in",
    dpi      = 300
  )

  invisible(p)
}

# Appendix plots --------------------------------------------------------------

# plot_vdem_distribution()
#
# Plots a histogram + kernel density + rug of v2x_polyarchy across all
# country-years in the node data. Saved as a PDF to output_dir.
#
# Arguments:
#   node_trade_path : path to node-trade-aligned-complete.rds
#   years           : integer vector of trade years to include
#   output_dir      : directory for saved PDF
#   filename        : output filename (default "vdem_polyarchy_distribution.pdf")
#   width, height   : PDF dimensions in inches

plot_vdem_distribution <- function(node_trade_path = "../data/node-trade-aligned-complete.rds",
                                   years           = 2000:2018,
                                   output_dir,
                                   filename        = "vdem_polyarchy_distribution.pdf",
                                   width           = 6,
                                   height          = 4) {
  
  x <- readRDS(node_trade_path) |>
    purrr::map(~ .x$nodes) |>
    dplyr::bind_rows() |>
    dplyr::filter((as.integer(year_IV) + 1L) %in% years) |>  # filter by trade year
    dplyr::distinct(name, year_IV, v2x_polyarchy) |>
    dplyr::pull(v2x_polyarchy)
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  pdf(file.path(output_dir, filename), width = width, height = height)
  hist(x, prob = TRUE, col = "grey85",
       xlab = "V-Dem Polyarchy Score",
       ylab = "Probability density",
       main = "Distribution of V-Dem Polyarchy Score")
  lines(density(x, na.rm = TRUE), lwd = 2)
  rug(x)
  dev.off()

  invisible(x)
}


# plot_pc2_trade_correlation()
#
# Produces two scatter plots correlating PC2 scores with a country's total
# trade volume for a single year: one for exports, one for imports.
# Points are labelled with ISO2 country codes and a Pearson correlation + OLS
# line is overlaid in red.
#
# Arguments:
#   scores_df      : data frame from build_pca_scores(), must contain columns
#                    `name`, `year`, `PC2`
#   trade_graph    : igraph object for the chosen year, with edge attribute
#                    `sum_trade`
#   year           : year to subset from scores_df (must match trade_graph)
#   pc2_col        : name of the column to use as the PC2 axis. When scores_df
#                    already has a pre-flipped column (e.g. "PC2_flipped" from
#                    scores_df_all), pass that name here and omit flip_pc2_years.
#                    Defaults to "PC2".
#   flip_pc2_years  : character/integer vector of years whose PC2 should be
#                     sign-flipped before plotting. Ignored when pc2_col is not
#                     "PC2" (i.e. a pre-flipped column is supplied).
#   output_dir_export : directory for the export plot PDF
#   output_dir_import : directory for the import plot PDF
#   log_transform     : if TRUE, applies log1p to import/export values before
#                       plotting, consistent with the regression table.
#                       Default FALSE (raw trade values).
#   filename_export   : output filename (default "pc2_export_<year>.pdf", or
#                       "pc2_export_log_<year>.pdf" when log_transform = TRUE)
#   filename_import   : output filename (default "pc2_import_<year>.pdf", or
#                       "pc2_import_log_<year>.pdf" when log_transform = TRUE)
#   width, height     : ggsave dimensions in inches

plot_pc2_trade_correlation <- function(
    scores_df,
    trade_graph,
    year,
    pc2_col           = "PC2",
    flip_pc2_years    = character(0),
    log_transform     = FALSE,
    output_dir_export,
    output_dir_import,
    filename_export   = NULL,
    filename_import   = NULL,
    width             = 6,
    height            = 5
) {

  year <- as.character(year)

  # ── Compute nodal export / import strength from the trade graph ──────────────
  exports <- igraph::strength(trade_graph, mode = "out",
                              weights = igraph::E(trade_graph)$sum_trade,
                              loops   = FALSE)
  imports <- igraph::strength(trade_graph, mode = "in",
                              weights = igraph::E(trade_graph)$sum_trade,
                              loops   = FALSE)

  trade_df <- data.frame(
    name    = names(exports),
    exports = if (log_transform) log1p(as.numeric(exports)) else as.numeric(exports),
    imports = if (log_transform) log1p(as.numeric(imports)) else as.numeric(imports),
    row.names = NULL
  )

  # ── Build analysis data frame ────────────────────────────────────────────────
  plot_df <- scores_df |>
    dplyr::filter(as.character(.data$year) == .env$year, !is.na(name)) |>
    dplyr::mutate(
      PC2_fixed = if (pc2_col == "PC2") {
        ifelse(as.character(.data$year) %in% as.character(flip_pc2_years), -PC2, PC2)
      } else {
        .data[[pc2_col]]
      },
      iso2c = countrycode::countrycode(name,
                                       origin      = "country.name.en",
                                       destination = "iso2c")
    ) |>
    dplyr::left_join(trade_df, by = "name")

  # ── Default filenames ────────────────────────────────────────────────────────
  if (is.null(filename_export)) filename_export <- paste0("pc2_export_", if (log_transform) "log_" else "", year, ".pdf")
  if (is.null(filename_import)) filename_import <- paste0("pc2_import_", if (log_transform) "log_" else "", year, ".pdf")

  dir.create(output_dir_export, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir_import, recursive = TRUE, showWarnings = FALSE)

  # ── Export plot ──────────────────────────────────────────────────────────────
  p_export <- ggplot(plot_df, aes(x = PC2_fixed, y = exports)) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = iso2c), size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    ggpubr::stat_cor(method = "pearson", color = "red") +
    labs(x = "PC2", y = if (log_transform) "Total Exports (log1p)" else "Total Exports") +
    theme_bw()

  ggsave(
    filename = file.path(output_dir_export, filename_export),
    plot     = p_export,
    width    = width,
    height   = height,
    units    = "in",
    dpi      = 300
  )

  # ── Import plot ──────────────────────────────────────────────────────────────
  p_import <- ggplot(plot_df, aes(x = PC2_fixed, y = imports)) +
    geom_point() +
    ggrepel::geom_text_repel(aes(label = iso2c), size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    ggpubr::stat_cor(method = "pearson", color = "red") +
    labs(x = "PC2", y = if (log_transform) "Total Imports (log1p)" else "Total Imports") +
    theme_bw()

  ggsave(
    filename = file.path(output_dir_import, filename_import),
    plot     = p_import,
    width    = width,
    height   = height,
    units    = "in",
    dpi      = 300
  )

  invisible(list(export = p_export, import = p_import))
}
