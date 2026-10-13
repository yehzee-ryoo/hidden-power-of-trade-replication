# Save function for created trade graphs (both incomplete and complete) -------
# this function will be used after creating trade graphs using
# make_(in)complete_trade_graph function.
save_trade_graphs <- function(trade_graphs, out_dir, suffix) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE) # ensure dir exists
  iwalk(trade_graphs, function(g, year) {
    out_path <- paste0(out_dir, "/trade-igraph-", year, "-", suffix, ".rds")
    write_rds(g, out_path)
  })
  invisible(trade_graphs)
}



# Build incomplete trade network ------------------------------------------------
# Input:  year        : single integer year (e.g. 2001)
#         cepii_list  : list of raw CEPII trade data frames, named as each year
#         ccode_fixed.name_aligned: country code lookup (pre-cleaned: ANT, SCG, N/A removed, and aligned with final countries list)
# Output: igraph object g : directed, weighted trade network for the given year
#           vertices: all countries present in that year's trade data (after cleaning)
#             - name: standardized country name (from countrycode())
#             - ctry_code: original CEPII numeric country code (i, j)
#           edges: all observed i → j trade flows
#             - sum_trade: sum of trade value 'v' across all HS96 products

# make_incomplete_trade_graph() builds a directed weighted trade network for a
# single year from raw CEPII HS96 data. Only observed i → j trade pairs appear
# as edges — countries that did not trade in that year have no edge between them.
# This "incomplete" network is used as a robustness check against the complete
# network (make_complete_trade_graph()), which is used in the main model.

make_incomplete_trade_graph <- function(year, cepii_list, ccode_fixed.name_aligned) {

  cepii <- cepii_list[[ as.character(year) ]]

  # Summarise edge list: total trade value per country pair
  total_trade_volume <- cepii |>
    group_by(i, j) |>
    summarise(sum_trade = sum(v, na.rm = TRUE), .groups = "drop") |>
    mutate(i = as.character(i), # "character" as in ccode_fixed.name_aligned
           j = as.character(j))

  # Build vertex lookup using ccode_fixed.name_aligned
  # ANT, SCG, N/A already excluded upfront
  verts <- ccode_fixed.name_aligned |>
    filter(name %in% unique(c(total_trade_volume$i, total_trade_volume$j)))

  # Check 1: verify dropped edges are ONLY due to ANT, SCG, or N/A
  dropped_codes <- unique(c(
    total_trade_volume$i[!total_trade_volume$i %in% verts$name],
    total_trade_volume$j[!total_trade_volume$j %in% verts$name]
  ))

  if (length(dropped_codes) > 0) {
    message("Year ", year, " — edge drops due to excluded codes: ",
            paste(dropped_codes, collapse = ", "))
  }

  # Keep only edges whose endpoints are in verts
  edges <- total_trade_volume |>
    filter(i %in% verts$name, j %in% verts$name) |>
    dplyr::select(from = i, to = j, sum_trade)

  # Build igraph
  g <- graph_from_data_frame(d        = edges,
                             directed = TRUE,
                             vertices = verts)

  # Replace name with fixed.name, keep country code separately
  g <- igraph::set_vertex_attr(g, "ctry_code", value = igraph::V(g)$name)
  g <- igraph::set_vertex_attr(g, "name",      value = igraph::V(g)$fixed.name)
  g <- igraph::delete_vertex_attr(g, "fixed.name")


  # ── Check 2: verify country name mapping ──────────────────────────────
  mapping_check <- tibble(
    name      = igraph::V(g)$name,
    ctry_code = igraph::V(g)$ctry_code
  ) |>
    left_join(
      ccode_fixed.name_aligned |> dplyr::select(name, iso3c),
      by = c("ctry_code" = "name")
    ) |>
    mutate( #recomputed ground truth to compare with the assigned name
      expected_name = countrycode(iso3c, origin = "iso3c",
                                  destination = "country.name.en",
                                  custom_match = c("ATG" = "Antigua & Barbuda")) #manual fix
    )

  mismatches <- mapping_check |> filter(name != expected_name)
  if (nrow(mismatches) > 0) {
    message("Year ", year, " — name mismatches found:")
    print(mismatches)
  }

  # ── Check 3: verify no NA names remain ────────────────────────────────
  na_verts <- igraph::as_data_frame(g, what = "vertices") |>
    filter(is.na(name))
  if (nrow(na_verts) > 0) {
    message("Year ", year, " — vertices with NA name: ",
            paste(na_verts$ctry_code, collapse = ", "))
  }#no message if no na_verts

  # Check 4: verify vertex attributes (ctry_code, iso3c) are preserved
  # igraph::as_data_frame() may silently drop non-standard vertex attributes.
  # This check ensures ctry_code and iso3c survive graph construction and will
  # carry through to make_complete_trade_graph() correctly.
  vattr <- igraph::as_data_frame(g, what = "vertices")
  missing_attrs <- setdiff(c("ctry_code", "iso3c"), names(vattr))
  if (length(missing_attrs) > 0) {
    warning("Year ", year, " — vertex attributes lost after graph construction: ",
            paste(missing_attrs, collapse = ", "))
  }

  return(g)
}


# Build complete trade network ------------------------------------------------

# make_complete_trade_graph() expands all pairs of vertices to create a complete
# graph (sum_trade = 0) for unobserved pairs, while keeping the same vertex set as
# the original. This way, we can compare results from the original (incomplete)
# and complete graphs to see if the missing edges are driving our results.

# Input:  g_incomplete : igraph object from make_incomplete_trade_graph()
#           - all vertices already cleaned and aligned to final_countries
#           - existing edges have raw sum_trade values
# Output: igraph object g_complete : same vertices, but ALL possible directed
#           i → j pairs present
#           - sum_trade: raw summed trade values from i to j, 0 for unobserved pairs


make_complete_trade_graph <- function(g_incomplete) {

  # Extract existing vertices and edges
  verts <- igraph::as_data_frame(g_incomplete, what = "vertices")
  edges <- igraph::as_data_frame(g_incomplete, what = "edges")

  # Generate all possible directed i → j pairs (excluding self-loops)
  all_pairs <- expand.grid(
    from = verts$name,
    to   = verts$name,
    stringsAsFactors = FALSE
  ) |>
    filter(from != to)  # remove self-loops

  # Left join observed edges onto all pairs — unobserved pairs get NA → 0
  edges_complete <- all_pairs |>
    left_join(edges, by = c("from", "to")) |>
    mutate(
      sum_trade     = replace_na(sum_trade,     0)
    )

  # Build complete igraph
  g_complete <- graph_from_data_frame(d        = edges_complete,
                                      directed = TRUE,
                                      vertices = verts)

  # Check 1: edge count should be n*(n-1) for n vertices
  n        <- igraph::vcount(g_complete)
  expected <- n * (n - 1)
  actual   <- igraph::ecount(g_complete)

  if (actual != expected) {
    warning("Complete graph edge count wrong: expected ", expected,
            " got ", actual)
  } else {
    message("Complete graph OK: ", n, " vertices, ", actual,
            " edges (", expected, " expected)")
  }

  # Check 2: verify vertex attributes (ctry_code, iso3c) survived graph construction.
  # igraph::as_data_frame() may silently drop non-standard vertex attributes when
  # extracting verts above. If lost, the complete graph has vertices with only
  # 'name' and no country code or iso3c — which breaks any downstream join or lookup.
  vattr_complete <- igraph::as_data_frame(g_complete, what = "vertices")
  missing_attrs  <- setdiff(c("ctry_code", "iso3c"), names(vattr_complete))
  if (length(missing_attrs) > 0) {
    warning("Complete graph lost vertex attributes: ",
            paste(missing_attrs, collapse = ", "))
  }

  return(g_complete)
}


# Compare incomplete and complete trade graphs --------------------------------
# For each year, check that:
#   1. Vertex sets are identical
#   2. Observed edges have identical sum_trade in both graphs
#   3. Added edges in complete graph all have sum_trade = 0
#   4. Edge count in complete graph = n*(n-1)

# Input:  year (a single year)
#      -  trade_graphs_incomplete: list of all incomplete igraph objects, each igraph named as each year
#      -  trade_graphs_complete  : list of all complete igraph objects, each igraph named as each year
# Output: messages about each check (1-4 above) only, no output returned.

compare_trade_graphs <- function(year, trade_graphs_incomplete, trade_graphs_complete) {
  g_inc  <- trade_graphs_incomplete[[as.character(year)]]
  g_comp <- trade_graphs_complete[[as.character(year)]]

  edges_inc  <- igraph::as_data_frame(g_inc,  what = "edges") |>
    mutate(from = as.character(from), to = as.character(to))
  edges_comp <- igraph::as_data_frame(g_comp, what = "edges") |>
    mutate(from = as.character(from), to = as.character(to))

  # Check 1: vertex sets identical
  same_verts <- setequal(igraph::V(g_inc)$name, igraph::V(g_comp)$name)
  message("Year ", year, " — Check 1 (same vertices): ", same_verts)

  # Check 2: observed edge weights identical in both graphs
  sum_trade_check <- edges_inc |>
    rename(sum_trade_inc = sum_trade) |>
    left_join(
      edges_comp |> dplyr::select(from, to, sum_trade_comp = sum_trade),
      by = c("from", "to")
    ) |>
    mutate(sum_trade_match = sum_trade_inc == sum_trade_comp)

  all_match <- all(sum_trade_check$sum_trade_match, na.rm = TRUE)
  message("Year ", year, " — Check 2 (sum_trade match): ", all_match)

  # Check 3: added edges all have sum_trade = 0
  added_edges   <- edges_comp |> anti_join(edges_inc, by = c("from", "to"))
  all_zero      <- all(added_edges$sum_trade == 0)
  message("Year ", year, " — Check 3 (added edges = 0): ", all_zero)

  # Check 4: complete graph has exactly n*(n-1) edges
  n             <- igraph::vcount(g_comp)
  correct_count <- igraph::ecount(g_comp) == n * (n - 1)
  message("Year ", year, " — Check 4 (edge count n*(n-1)): ", correct_count)
}


# Orchestration: build both incomplete and complete trade graphs --------------
# Loads CEPII codebook and raw data, aligns to final country list, builds and
# saves both graph types, runs validation checks.

build_trade_network_graphs <- function() {

  # Load CEPII codebook
  ccode <- read_csv("../raw data/Trade-cepii-HS96/WTFC_country_codes_V202104.csv")

  # Load all years of CEPII trade data
  trade_years <- 2000:2018
  paths <- paste0("../raw data/Trade-cepii-HS96/WTFC_HS96_Y", trade_years, "_V202104.csv")
  cepii_list <- paths |>
    set_names(trade_years) |>
    map(vroom)

  # Create country code lookup
  # NOTE: The raw CEPII codebook contains a data entry error: Antigua & Barbuda
  # (iso2 = "AG") is assigned iso3_digit_alpha = "ATA" (Antarctica). We manually
  # recode ATA → ATG before passing to countrycode().
  ccode_fixed.name <- ccode |>
    transmute(
      name       = as.character(country_code),
      iso3c      = case_when(
        iso3_digit_alpha == "ATA" ~ "ATG", # manual fix for Antigua & Barbuda
        TRUE  ~ iso3_digit_alpha
      ),
      fixed.name = countrycode(iso3c,
                               origin      = "iso3c",
                               destination = "country.name.en",
                               custom_match = c("ATG" = "Antigua & Barbuda")),
      country_name_full = country_name_full
    ) |>
    # Dropping NA fixed.name removes: ANT (Netherlands Antilles — dissolved 2010),
    # SCG (Serbia & Montenegro — dissolved 2006), and non-country aggregates.
    filter(!is.na(fixed.name))

  # Load final countries and align trade nodes to final country list
  dv_data         <- readRDS("../data/dv_data.rds")
  final_countries <- readRDS("../data/final_countries.rds")

  overlap_ctries           <- intersect(ccode_fixed.name$fixed.name, final_countries$fixed.name)
  only_trade_ctries        <- setdiff(ccode_fixed.name$fixed.name, final_countries$fixed.name)
  only_dv_ctries           <- setdiff(final_countries$fixed.name, ccode_fixed.name$fixed.name)
  expected_drops           <- union(only_trade_ctries, only_dv_ctries)
  ccode_fixed.name_aligned <- ccode_fixed.name |> filter(fixed.name %in% overlap_ctries)

  # Build incomplete graphs over all years
  trade_graphs_incomplete <- trade_years |>
    set_names() |>
    map(\(y) make_incomplete_trade_graph(y, cepii_list, ccode_fixed.name_aligned))

  # Validate drops: erased edges should only be due to ANT, SCG, or N/A
  actual_drops <- setdiff(
    ccode_fixed.name$fixed.name,
    map(trade_graphs_incomplete, \(g) igraph::V(g)$name) |> unlist() |> unique()
  )

  actual_drops_df <- ccode_fixed.name |>
    filter(fixed.name %in% actual_drops) |>
    dplyr::select(name, iso3c, fixed.name) |>
    mutate(
      name = as.numeric(name),
      drop_reason = case_when(
        fixed.name %in% only_trade_ctries ~ "alignment drop: in trade data but not in DV data",
        fixed.name %in% ccode_fixed.name_aligned$fixed.name ~ "no trade activity in any year",
        TRUE ~ "unexpected: inspect manually"
      )
    )

  unexpected <- setdiff(actual_drops_df$fixed.name, expected_drops)
  if (length(unexpected) > 0) {
    warning("Unexpected drops: ", paste(unexpected, collapse = ", "))
  }
  message("Drop reasons:"); print(actual_drops_df |> count(drop_reason))

  # Save incomplete graphs
  save_trade_graphs(trade_graphs_incomplete,
                    out_dir = "../data/trade-graphs-incomplete",
                    suffix  = "incomplete")

  # Build complete graphs
  trade_graphs_complete <- trade_graphs_incomplete |>
    map(make_complete_trade_graph)

  # Save complete graphs
  save_trade_graphs(trade_graphs_complete,
                    out_dir = "../data/trade-graphs-complete",
                    suffix  = "complete")

  # Compare incomplete and complete graphs across all years
  for (y in trade_years) {
    compare_trade_graphs(y, trade_graphs_incomplete, trade_graphs_complete)
  }

  # Expansion summary: how many edges were added per year
  expansion_summary <- map_dfr(as.character(trade_years), function(y) {
    tibble(
      year        = as.integer(y),
      edges_added = igraph::ecount(trade_graphs_complete[[y]]) -
        igraph::ecount(trade_graphs_incomplete[[y]])
    )
  })
  message("Expansion summary:"); print(expansion_summary)

  invisible(list(incomplete = trade_graphs_incomplete, complete = trade_graphs_complete))
}
