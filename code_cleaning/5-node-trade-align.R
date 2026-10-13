# align_nodes_and_graphs() performs YEAR-SPECIFIC alignment on top of the static
# alignment already done in ccode_fixed.name_aligned (script 4-trade-network-graph.R).
# Static alignment keeps only countries that appear in both the CEPII codebook and
# final_countries. But some countries still drop out in specific years because they
# have no covariate data (GDP, population, democracy score etc.) or no trade flows
# that year (e.g. Monaco and Liechtenstein have no recorded CEPII trade flows in
# some years). This function filters BOTH the graph vertices and node data to their
# year-specific intersection, so each year's graph and node data contain exactly
# the same countries.

align_nodes_and_graphs <- function(trade_graphs, lagged_polished) {

  map(names(trade_graphs), function(year) {

    node_df     <- lagged_polished[[year]]
    g           <- trade_graphs[[year]]
    graph_names <- igraph::V(g)$name
    node_names  <- node_df$name

    # countries in graph but missing from node data (no covariates this year)
    dropped_from_graph <- setdiff(graph_names, node_names)
    # countries in node data but missing from graph (no trade flows this year)
    dropped_from_nodes <- setdiff(node_names, graph_names)

    if (length(dropped_from_graph) > 0) {
      message("Year ", year, " — dropped from graph (no covariate data): ",
              paste(dropped_from_graph, collapse = ", "))
    }
    if (length(dropped_from_nodes) > 0) {
      message("Year ", year, " — dropped from nodes (no trade data): ",
              paste(dropped_from_nodes, collapse = ", "))
    }
    if (length(dropped_from_graph) == 0 & length(dropped_from_nodes) == 0) {
      message("Year ", year, " — OK: ", length(graph_names),
              " countries perfectly aligned")
    }

    # filter BOTH directions to year-specific intersection
    intersection    <- intersect(graph_names, node_names)
    g_aligned       <- igraph::induced_subgraph(g, igraph::V(g)$name %in% intersection)
    node_df_aligned <- node_df |> filter(name %in% intersection)

    list(graph = g_aligned, nodes = node_df_aligned)

  }) |>
    set_names(names(trade_graphs))
}


# verify_alignment() confirms that align_nodes_and_graphs() correctly produced
# matching vertex and node sets for each year. Check 1 should always be TRUE
# after alignment — if not, something went wrong in induced_subgraph() or the
# filter step. Check 2 is purely informational — remaining NAs are expected for
# some countries in some years and will be handled by listwise deletion at the
# regression stage.

verify_alignment <- function(graphs_aligned, nodes_aligned) {

  walk(names(graphs_aligned), function(year) {

    g           <- graphs_aligned[[year]]
    node_df     <- nodes_aligned[[year]]
    graph_names <- igraph::V(g)$name
    node_names  <- node_df |> distinct(name) |> pull(name)

    # Check 1: exact vertex match
    exact_match        <- setequal(graph_names, node_names)
    missing_from_nodes <- setdiff(graph_names, node_names)
    missing_from_graph <- setdiff(node_names,  graph_names)

    message("Year ", year, " — Check 1 (exact match): ", exact_match,
            " | graph not in nodes: ", length(missing_from_nodes),
            " | nodes not in graph: ", length(missing_from_graph))

    if (!exact_match) {
      if (length(missing_from_nodes) > 0)
        message("  → in graph but not in nodes: ",
                paste(missing_from_nodes, collapse = ", "))
      if (length(missing_from_graph) > 0)
        message("  → in nodes but not in graph: ",
                paste(missing_from_graph, collapse = ", "))
    }

    # Check 2: NA counts at COUNTRY level (not vote level)
    # node_df is long format (one row per country per UNGA vote) so
    # distinct() first to get one row per country before counting NAs
    na_check <- node_df |>
      group_by(name) |>
      slice(1) |>   # one row per country regardless of covariate values
      ungroup() |>
      summarise(
        n_countries   = n_distinct(name),
        na_l.gdp_s    = sum(is.na(l.gdp_s)),
        na_l.pop_s    = sum(is.na(l.pop_s)),
        na_HR_index_s = sum(is.na(HR_index_s)),
        na_v2x        = sum(is.na(v2x_polyarchy))
      )

    if (any(na_check |> dplyr::select(-n_countries) > 0)) {
      message("  → Year ", year, " — NA covariates (out of ",
              na_check$n_countries, " countries):")
      print(na_check)
    }
  })
}


# Orchestration: align trade graphs with node covariate data ------------------
# Loads lagged_polished and both graph types, runs alignment and verification,
# saves the aligned objects.

build_node_trade_aligned <- function() {

  lagged_polished <- readRDS("../data/lagged_polished_list.rds") |>
    (\(x) setNames(x, gsub("node_", "", names(x))))()

  trade_years <- 2000:2018

  trade_graphs_incomplete <- map(trade_years, ~ readRDS(
    paste0("../data/trade-graphs-incomplete/",
           sprintf("trade-igraph-%s-incomplete.rds", .x)))
  ) |> set_names(as.character(trade_years))

  trade_graphs_complete <- map(trade_years, ~ readRDS(
    paste0("../data/trade-graphs-complete/",
           sprintf("trade-igraph-%s-complete.rds", .x)))
  ) |> set_names(as.character(trade_years))

  aligned_incomplete <- align_nodes_and_graphs(trade_graphs_incomplete, lagged_polished)
  aligned_complete   <- align_nodes_and_graphs(trade_graphs_complete,   lagged_polished)

  # Check 1: node sets should be identical regardless of complete/incomplete
  node_sets_equal <- all(map2_lgl(
    aligned_incomplete, aligned_complete,
    \(inc, comp) setequal(inc$nodes$name, comp$nodes$name)
  ))

  if (node_sets_equal) {
    message("OK: node sets identical across complete and incomplete alignments")
  } else {
    warning("Node sets differ between complete and incomplete alignments — investigate")
  }

  # Check 2: vertex sets should be identical between graph and node data within each alignment
  graph_sets_equal <- all(map2_lgl(
    aligned_incomplete, aligned_complete,
    \(inc, comp) setequal(igraph::V(inc$graph)$name, igraph::V(comp$graph)$name)
  ))

  if (graph_sets_equal) {
    message("OK: graph vertex sets identical across complete and incomplete alignments")
  } else {
    warning("Graph vertex sets differ between complete and incomplete alignments — investigate")
  }

  saveRDS(aligned_incomplete, "../data/node-trade-aligned-incomplete.rds")
  saveRDS(aligned_complete,   "../data/node-trade-aligned-complete.rds")

  # Verify alignment for both
  verify_alignment(map(aligned_incomplete, \(x) x$graph), map(aligned_incomplete, \(x) x$nodes))
  verify_alignment(map(aligned_complete,   \(x) x$graph), map(aligned_complete,   \(x) x$nodes))

  invisible(list(incomplete = aligned_incomplete, complete = aligned_complete))
}
