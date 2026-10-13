# Rank Selection via EigenCV --------------------------------------------------
# Goal: Find the optimal latent rank (K) of the trade network.
#
# Inputs:
#   trade_graph: igraph object with raw "sum_trade" edge weights.
#   log_fn:      Transformation (e.g., log1p or log_fn8) to reduce skewness.
#
# Logic:
#   1. Extracts "sum_trade" into a sparse adjacency matrix (A).
#   2. Applies log_fn() and round() to sum_trade values to create "count-like" doubles.
#
# Outputs (List):
#   $K:            Selected rank (from gdim::eigcv estimated_dimension).
#   $eigcv_result: Full statistical summary from the gdim package.
#   $A:            (Optional) Transformed sparse matrix — only if keep_matrix = TRUE.
#                  Omitted by default to save RAM; does not change K or eigcv.
# -----------------------------------------------------------------------------

log_fn8 <- function(x) log(x) + 8
# Note: this log_fn8 function ONLY works with incomplete graph. Because complete
# graph has 0 edges where we expanded from only existing i-js to all possible pairs.

# A is used ONLY to select the optimal latent rank K via eigcv. It is NOT the
# matrix passed to sensitivity_curve_long(). That function receives the igraph
# object directly and constructs its own internal adjacency matrix. The
# transformation applied here (log + round) has no bearing on how
# sensitivity_curve_long() decomposes the network.

rank_selection <- function(trade_graph, log_fn, keep_matrix = FALSE) {
  A        <- igraph::as_adjacency_matrix(trade_graph, attr = "sum_trade")
  A_double <- A
  A_double@x <- round(log_fn(A@x))
  stopifnot(is.double(A_double@x))

  eigcv_result <- gdim::eigcv(A_double, k_max = 80, num_bootstraps = 100)

  # use the package's own estimate — avoids off-by-one in manual which() logic
  K <- eigcv_result$estimated_dimension

  out <- list(K = K, eigcv_result = eigcv_result)
  if (isTRUE(keep_matrix)) {
    out$A <- A_double
  }
  out
}


# Run rank selection in parallel for all years and graph types -
# complete graph + log_fn8 does NOT work (log(0) = -Inf); only log1p used there.

run_rank_selection <- function(seed, n_cores = NULL, reserve_cores = 4, keep_matrix = FALSE) {

  node_trade_aligned_inc  <- readRDS("../data/node-trade-aligned-incomplete.rds")
  node_trade_aligned_comp <- readRDS("../data/node-trade-aligned-complete.rds")

  trade_graph_incomplete <- map(node_trade_aligned_inc,  \(x) x$graph)
  trade_graph_complete   <- map(node_trade_aligned_comp, \(x) x$graph)

  detected <- parallel::detectCores()
  if (is.null(n_cores)) {
    n_cores <- max(1L, detected - reserve_cores)
  }
  n_cores <- max(1L, min(as.integer(n_cores), detected))
  message("Rank selection using ", n_cores, " workers (detected: ", detected, ").")

  cl <- makeCluster(n_cores)
  on.exit(stopCluster(cl), add = TRUE)
  clusterSetRNGStream(cl, seed)
  clusterEvalQ(cl, { library(igraph); library(gdim) })
  # Do not export keep_matrix here: it only exists in this function's env, and
  # clusterExport() defaults to envir = .GlobalEnv (would error: object not found).
  # parLapply(..., keep_matrix = keep_matrix) passes the value to workers anyway.
  clusterExport(cl, varlist = c("log_fn8", "rank_selection"))

  all_results <- list(
    inc_lfn8 = parLapply(cl, trade_graph_incomplete, rank_selection, log_fn = log_fn8, keep_matrix = keep_matrix),
    inc_lp1  = parLapply(cl, trade_graph_incomplete, rank_selection, log_fn = log1p, keep_matrix = keep_matrix),
    comp_lp1 = parLapply(cl, trade_graph_complete,   rank_selection, log_fn = log1p, keep_matrix = keep_matrix)
  )

  dir.create("../data/rank_selection", recursive = TRUE, showWarnings = FALSE)
  iwalk(all_results,
        ~ saveRDS(.x, file.path("../data/rank_selection", paste0("results_", .y, ".rds"))))

  # K results table
  K_results <- tibble(
    year              = names(all_results$inc_lp1),
    K_incomplete_lfn8 = map_int(all_results$inc_lfn8, \(x) x$K),
    K_incomplete_lp1  = map_int(all_results$inc_lp1,  \(x) x$K),
    K_complete_lp1    = map_int(all_results$comp_lp1,  \(x) x$K)
  )

  saveRDS(K_results, "../data/rank_selection/K_results.rds")

  invisible(K_results)
}
