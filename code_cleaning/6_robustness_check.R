# Load finally aligned node-trade data
load_aligned <- function(path) {
  data <- readRDS(path)
  list(
    graphs = map(data, \(x) x$graph),
    nodes  = map(data, \(x) x$nodes)
  )
}


# Build robust trade network (zero-fill missing traders) ------------------
# Robustness: complete graph expanded to full covariate sample
#
# zero-fill missing traders instead of dropping them.
# Keep all countries that have covariate data in the node data, even if they
# have no trade flows that year. Add them back as isolated vertices (0-weight
# edges to all others) so centrality is computed on the full covariate sample,
# not just the trading sample.

make_robust_trade_graph <- function(year, trade_graphs_complete, lagged_polished) {

  g_comp   <- trade_graphs_complete[[as.character(year)]]
  node_df  <- lagged_polished[[as.character(year)]]

  # Countries in node data but not in complete graph (no trade flows this year)
  missing_traders <- setdiff(node_df$name, igraph::V(g_comp)$name)

  if (length(missing_traders) > 0) {
    message("Year ", year, " — adding back as zero-traders: ",
            paste(missing_traders, collapse = ", "))
  }

  # Extract existing vertices and edges from complete graph
  verts <- igraph::as_data_frame(g_comp, what = "vertices")
  edges <- igraph::as_data_frame(g_comp, what = "edges") |>
    mutate(from = as.character(from), to = as.character(to))

  # Add missing traders as new vertices, pulling iso3c from final_countries
  fc <- readRDS("../data/final_countries.rds")

  verts_expanded <- bind_rows(
    verts,
    tibble(name = missing_traders) |>
      left_join(fc |> distinct(fixed.name, iso3c),
                by = c("name" = "fixed.name")) |>
      mutate(ctry_code = NA_character_)
  )

  # Only need to add edges involving the new vertices — all other pairs
  # already exist in the complete graph
  new_pairs <- expand.grid(
    from = verts_expanded$name,
    to   = verts_expanded$name,
    stringsAsFactors = FALSE
  ) |>
    filter(from != to) |>
    # keep only pairs involving at least one new vertex
    filter(from %in% missing_traders | to %in% missing_traders)

  edges_expanded <- bind_rows(
    edges,
    new_pairs |> mutate(sum_trade = 0)
  )

  # Build graph
  g_robust <- graph_from_data_frame(d        = edges_expanded,
                                    directed = TRUE,
                                    vertices = verts_expanded)

  # Check
  final_names   <- igraph::V(g_robust)$name
  still_missing <- setdiff(node_df$name, final_names)
  if (length(still_missing) > 0) {
    warning("Year ", year, " — still missing after robust expansion: ",
            paste(still_missing, collapse = ", "))
  } else {
    message("Year ", year, " — robust graph OK: ", length(final_names),
            " vertices (all covariate countries present)")
  }

  return(g_robust)
}


# Orchestration: build DV subsets (three resolution-filtered datasets) --------

build_dv_subsets <- function() {

  complete   <- load_aligned("../data/node-trade-aligned-complete.rds")
  incomplete <- load_aligned("../data/node-trade-aligned-incomplete.rds")

  # Node sets must be identical across complete/incomplete (only edges differ)
  stopifnot(
    "Node sets differ between complete and incomplete — investigate" =
      isTRUE(all.equal(complete$nodes, incomplete$nodes))
  )

  final_nodes_df <- bind_rows(complete$nodes, .id = "year")

  dv_data   <- readRDS("../data/dv_data.rds")

  # Resolution filter patterns
  patterns <- list(
    right_to_dev     = regex("\\bright\\s+to\\s+development\\b",           ignore_case = TRUE),
    cultural_div     = regex("cultur\\w*\\s+diversity",                     ignore_case = TRUE),
    israel_palestine = regex("israel|palestin|gaza|jerusalem|west bank",    ignore_case = TRUE)
  )

  # Extract distinct rcids matching each pattern
  get_matching_rcids <- function(df, pattern) {
    df |>
      distinct(rcid, short, descr) |>
      filter(str_detect(paste(short, descr), pattern)) |>
      pull(rcid)
  }

  rcids_RD <- get_matching_rcids(final_nodes_df, patterns$right_to_dev)
  rcids_CD <- get_matching_rcids(final_nodes_df, patterns$cultural_div)
  rcids_IP <- get_matching_rcids(final_nodes_df, patterns$israel_palestine)

  # Subsets
  subsets <- list(
    # 1. All politicized resolutions removed
    node_full_eliminated = final_nodes_df |>
      filter(!rcid %in% c(rcids_RD, rcids_CD, rcids_IP)),

    # 2. Only Cultural Diversity & Right to Development resolutions
    node_only_CD_RD = final_nodes_df |>
      filter(rcid %in% c(rcids_RD, rcids_CD)),

    # 3. Only Israel-Palestine resolutions
    node_only_IsPal = final_nodes_df |>
      filter(rcid %in% rcids_IP)
  )

  dir.create("../data/dv_subset", recursive = TRUE, showWarnings = FALSE)

  iwalk(subsets, \(df, name)
        saveRDS(df, paste0("../data/dv_subset/", name, ".rds"))
  )

  invisible(subsets)
}


# Orchestration: build category-exclusion DV subsets (main minus each category) --
#
# Six recurring resolution types (a-f, see 5-resolution-types.R) pair
# human-rights language with a separate political dispute. This builds one
# subset per category: the full analytic sample with that category's
# resolutions removed, so the main model can be re-estimated without them.
# It also builds a seventh subset, node_excl_all, with all six categories
# removed at once. This differs from node_full_eliminated built by
# build_dv_subsets() above, which drops only three categories (right to
# development, cultural diversity, Israel/Palestine) using its own patterns.
#
# Category membership comes from categorize_resolutions() and
# build_category_patterns() (defined in code_analyze/5-resolution-types.R,
# sourced before this is called).
build_category_exclusion_subsets <- function() {

  # Short filename keys for each category label. Checked against
  # build_category_patterns() below so the two can't silently drift apart.
  category_keys <- c(
    "Globalization and human rights"      = "globalization",
    "Combating glorification of Nazism"   = "nazism",
    "Racial discrimination and Durban"    = "racism_durban",
    "Right to development"                = "right_to_dev",
    "Human rights and cultural diversity" = "cultural_diversity",
    "Israel/Palestine"                    = "israel_palestine"
  )
  stopifnot(
    "category_keys is out of sync with build_category_patterns() -- update both" =
      setequal(names(category_keys), names(build_category_patterns()))
  )

  complete       <- load_aligned("../data/node-trade-aligned-complete.rds")
  final_nodes_df <- bind_rows(complete$nodes, .id = "year")

  categorized <- final_nodes_df |>
    distinct(rcid, short, descr) |>
    categorize_resolutions()

  subsets <- imap(category_keys, \(key, label) {
    excl_rcids <- categorized |> filter(category == label) |> pull(rcid)
    final_nodes_df |> filter(!rcid %in% excl_rcids)
  }) |>
    set_names(paste0("node_excl_", category_keys))

  # Seventh subset: all six categories excluded at once.
  excl_rcids_all <- categorized |> filter(category %in% names(category_keys)) |> pull(rcid)
  subsets$node_excl_all <- final_nodes_df |> filter(!rcid %in% excl_rcids_all)

  dir.create("../data/dv_subset", recursive = TRUE, showWarnings = FALSE)

  iwalk(subsets, \(df, name)
        saveRDS(df, paste0("../data/dv_subset/", name, ".rds"))
  )

  invisible(subsets)
}


# Orchestration: build robust trade graphs (zero-filled for missing traders) --

build_robust_trade_graphs <- function() {

  trade_years <- 2000:2018

  trade_graphs_complete <- map(trade_years, ~ readRDS(
    paste0("../data/trade-graphs-complete/",
           sprintf("trade-igraph-%s-complete.rds", .x)))
  ) |> set_names(as.character(trade_years))

  lagged_polished <- readRDS("../data/lagged_polished_list.rds") |>
    (\(x) setNames(x, gsub("node_", "", names(x))))()

  trade_graphs_robust <- trade_years |>
    set_names() |>
    map(\(y) make_robust_trade_graph(y, trade_graphs_complete, lagged_polished))

  # save_trade_graphs() is defined in 4-trade-network-graph.R (sourced before this)
  save_trade_graphs(trade_graphs_robust,
                    out_dir = "../data/trade-graphs-robust",
                    suffix  = "robust")

  invisible(trade_graphs_robust)
}
