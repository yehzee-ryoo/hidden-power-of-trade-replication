# Resolution-type table ---------------------------------------------------
# Classifies UNGA "Human rights"-issue resolutions into six recurring types
# that pair rights language with a separate political dispute:
#   a. globalization and the enjoyment of human rights
#   b. combating glorification of Nazism
#   c. racial discrimination / Durban Declaration follow-up
#   d. right to development
#   e. human rights and cultural diversity
#   f. Israel-Palestine and occupied territories
#
# Every resolution in the analytic sample is assigned to exactly one of these
# six categories, or "Others" if it matches none. The table reports counts,
# recurrence, and shares.
#
# All categories are defined by SUBJECT MATTER. The table makes no claim
# about how contested a resolution is or how any bloc votes on it.
#
# SCOPE: these six categories are not a partition of the human-rights agenda.
# Country-specific situations, sovereignty and non-intervention resolutions,
# UN human rights machinery, and rights-as-such resolutions (death penalty,
# torture, right to food, rights of children, women, migrants and persons
# with disabilities) match no named category and are reported as "Others",
# so the "Others" share is expected to be large.
#
# Data source: ../data/node-trade-aligned-complete.rds, using its $nodes.
# In master-run-all.R this is node_hr_list — the literal node_data argument
# passed to run_sensitivity_curves() for the main model, so it is by
# construction the sample the reported estimates come from.
# Built by build_node_trade_aligned() in code_cleaning/5-node-trade-align.R.
#
# Cross-check: lagged_polished_list.rds (the same panel one step upstream,
# before trade alignment) yields an identical 430 resolutions over 2001-2019.
# Trade alignment drops countries, never whole resolutions.
#
# This file defines five functions:
#   build_category_patterns():        the six named category definitions (a-f)
#   categorize_resolutions():         tags each resolution with one category
#   count_category_by_year():         per-year count of resolutions in one
#                                     category, for per-year exclusion reporting
#   build_resolution_type_table():    summarizes categories into a table,
#                                     optionally saved as a .tex file
#   build_regression_category_exclusion_table(): end-to-end — builds the six "main minus
#                                     category X" subsets, re-estimates the
#                                     main model on each, and writes the
#                                     comparison table


# build_category_patterns()
#
# Returns the ORDERED list of the six category definitions (see the header
# block). Order is substantive: categorize_resolutions() locks each
# resolution into the FIRST category it matches, so the sequence decides
# which classification wins when a resolution matches more than one.
#
# The only ordering constraint that matters in this sample: "Combating
# glorification of Nazism" resolutions condemn Nazism AS a form of racism, so
# their own titles also contain "racism, racial discrimination, xenophobia" —
# they match the racial-discrimination/Durban pattern too. Nazism (b) is
# tested before racial discrimination/Durban (c) so those resolutions are
# classified as Nazism, not folded into Durban. Checked directly against the
# analytic sample: no other pair of the six patterns overlaps, so the
# remaining order (a, d, e, f) is not load-bearing.
#
# Anything matching none of the six is labelled "Others". That residual is
# EXPECTED TO BE LARGE (see the header block) and is not a defect.
build_category_patterns <- function() {

  list(
    # (a)
    "Globalization and human rights" =
      "global[a-z]*ation",

    # (b): tested before (c) below — see the ordering note above — because
    # its own title text names racism, racial discrimination and xenophobia.
    "Combating glorification of Nazism" =
      "nazi|glorification",

    # (c): racism, racial discrimination, xenophobia, and the Durban
    # Declaration and Programme of Action follow-up.
    "Racial discrimination and Durban" =
      "racis|racial discrimination|xenophobia|durban",

    # (d): both wordings ("right to development" / "right of development")
    # appear in titles across the sample.
    "Right to development" =
      "right(s)? (to|of) development",

    # (e)
    "Human rights and cultural diversity" =
      "cultur[a-z]* diversity",

    # (f): also used elsewhere in the analysis (issue_type == "specific").
    # Covers the Israeli-Palestinian conflict, Israeli occupation, and the
    # occupied Syrian Golan — the Assembly takes Golan items under the same
    # agenda item as the Palestinian occupation resolutions, and some texts
    # cover both territories.
    "Israel/Palestine" =
      "israel|palestin|gaza|jerusalem|west bank|golan"
  )
}


# categorize_resolutions()
#
# Assigns exactly one category per resolution. Categories are NOT naturally
# disjoint — a resolution can match several patterns — so exclusivity is
# imposed procedurally: patterns are tested in the order given by
# build_category_patterns() and the `is.na(category) &` guard below means the
# FIRST match wins and cannot be overwritten by a later pattern. This is what
# makes the categories sum to the sample total and the shares sum to 100.
#
# Arguments:
#   res : data frame with columns `short` and `descr`, one row per
#         resolution (e.g. distinct(rcid, year_DV, short, descr))
#
# Returns res with added columns:
#   category  : assigned category (character)
#   n_matched : how many categories the resolution matched in total, so the
#                sensitivity of the result to priority order can be reported
categorize_resolutions <- function(res) {

  patterns <- build_category_patterns()
  text     <- tolower(paste(res$short, res$descr))

  category  <- rep(NA_character_, length(text))
  n_matched <- rep(0L, length(text))

  for (label in names(patterns)) {
    matches   <- stringr::str_detect(text, patterns[[label]])
    n_matched <- n_matched + as.integer(matches)

    hit <- is.na(category) & matches   # first match wins; cannot be reclaimed
    category[hit] <- label
  }
  category[is.na(category)] <- "Others"

  res$category  <- category
  res$n_matched <- n_matched
  res
}


# count_category_by_year()
#
# For each year in node_data_list, counts how many DISTINCT resolutions that
# year belong to ANY of the given categories. Used to report "N resolutions
# excluded" PER YEAR in the category-exclusion table (regression_category_
# exclusion.tex) — resolutions are year-specific (each rcid belongs to
# exactly one year_DV), so the number a given category removes varies year to
# year; a single whole-panel total (as used for the resolution-type table
# above) isn't the right number to show alongside a per-year NDE/NIE estimate.
#
# Arguments:
#   node_data_list  : named list of per-year node data frames (e.g.
#                      node_hr_list), each with columns rcid, short, descr
#   category_labels : character vector of category label(s) to count (one or
#                      more of build_category_patterns()'s names) — pass more
#                      than one for an "exclude several categories at once"
#                      column, or character(0) to always return 0 (for the
#                      "main"/no-exclusion column, which excludes nothing)
#
# Returns a named-by-year integer vector.
count_category_by_year <- function(node_data_list, category_labels) {
  purrr::map_int(node_data_list, function(df) {
    df |>
      dplyr::distinct(rcid, short, descr) |>
      categorize_resolutions() |>
      dplyr::filter(category %in% category_labels) |>
      nrow()
  })
}


# build_resolution_type_table()
#
# Builds the type / recurrence / proportion table: one row per category,
# counting distinct resolutions (rcids) and the number of vote years in which
# the category appears. Covers the 430 resolutions in the main model's sample
# (vote years 2001-2019), not the raw 1995-2019 "Human rights"-issue pull from
# 1-DV-cleaning.R.
#
# Arguments:
#   node_trade_path : path to node-trade-aligned-complete.rds — a named list
#                      (one element per trade year) of list(graph, nodes);
#                      only $nodes is used here.
#   output_path     : full path for the saved .tex file (NULL = no file saved)
#   caption         : LaTeX caption string. Defaults to a self-documenting
#                      caption defining each column, the counting unit, every
#                      category, and the exclusivity rule.
#   label           : LaTeX label, without "tab:" prefix — kable() prepends it
#                      (same convention as build_summary_stats_table())
#
# Returns (invisibly) the resolution-level data frame with assigned
# categories; prints the summary table to console.
build_resolution_type_table <- function(node_trade_path = "../data/node-trade-aligned-complete.rds",
                                        output_path      = NULL,
                                        caption          = paste(
  "Composition of the 430 UNGA human-rights resolutions underlying the outcome, pooled across vote",
  "years 2001--2019. The counting unit is a distinct roll call (\\texttt{rcid}), not a country-vote:",
  "each resolution is voted on by roughly 190 states but counted once. \\textit{N Years Present} is",
  "the number of the 19 vote years in which a category appears. Categories are assigned by",
  "case-insensitive keyword matching on each resolution's subject label (\\texttt{short}) and",
  "description (\\texttt{descr}). Because they are not naturally disjoint, each resolution is assigned",
  "to the first category it matches in the order listed---Nazism precedes racial discrimination",
  "because its title also names racism, racial discrimination and xenophobia---and is counted exactly",
  "once. Resolutions matching no category are reported as \\textit{Others}."
),
                                        label            = NULL) {

  aligned <- readRDS(node_trade_path)

  res <- purrr::map(aligned, \(x) x$nodes) |>
    dplyr::bind_rows() |>
    dplyr::distinct(rcid, year_DV, short, descr) |>
    categorize_resolutions()

  n_years_total <- dplyr::n_distinct(res$year_DV)
  n_total       <- nrow(res)

  # Display rows in the priority order of build_category_patterns(), so readers
  # can see which classification took precedence over which.
  category_levels <- c(names(build_category_patterns()), "Others")

  summary_tab <- res |>
    dplyr::group_by(category) |>
    dplyr::summarise(
      `N Resolutions`   = dplyr::n(),
      `N Years Present` = dplyr::n_distinct(year_DV),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      `% of Total` = round(100 * `N Resolutions` / n_total, 1),
      category     = factor(category, levels = category_levels)
    ) |>
    dplyr::arrange(category) |>
    dplyr::mutate(category = as.character(category)) |>
    dplyr::rename(Type = category)

  summary_tab <- dplyr::bind_rows(
    summary_tab,
    tibble::tibble(
      Type              = "Total",
      `N Resolutions`   = n_total,
      `N Years Present` = n_years_total,
      `% of Total`      = 100
    )
  )

  print(summary_tab)

  # Sensitivity of the classification to priority order: how many resolutions
  # matched more than one category and so were decided by the ordering rule.
  n_multi <- sum(res$n_matched > 1)
  message(sprintf(
    "Multi-match resolutions (assignment decided by priority order): %d of %d (%.1f%%)",
    n_multi, n_total, 100 * n_multi / n_total
  ))

  if (!is.null(output_path)) {
    n_cols <- ncol(summary_tab) - 1L
    tab <- knitr::kable(
      summary_tab,
      format   = "latex",
      booktabs = TRUE,
      align    = c("l", rep("c", n_cols)),
      linesep  = "",
      caption  = caption,
      label    = label
    ) |>
      kableExtra::kable_styling(latex_options = c("hold_position")) |>
      kableExtra::row_spec(0, bold = TRUE) |>
      kableExtra::row_spec(nrow(summary_tab), bold = TRUE, hline_after = TRUE)

    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(as.character(tab), output_path)
    message("Table saved to: ", output_path)
  }

  invisible(res)
}


# build_regression_category_exclusion_table()
#
# Checks whether the democracy NDE/NIE estimates depend on any one resolution
# category (a-f, see build_category_patterns()). Builds six "main minus
# category X" subsets plus one "main minus all six" subset
# (build_category_exclusion_subsets(), 6_robustness_check.R), re-estimates the
# main model on each, and writes one .tex file with two sidewaystable blocks
# (8 models are too wide for one table): Part A = main model (1) and
# categories (2)-(4); Part B = categories (5)-(7) and all six excluded (8).
#
# Arguments:
#   node_hr_list      : named list of per-year node data frames for the main model
#   trade_graphs_comp : named list of per-year trade igraphs, aligned to node_hr_list
#   formula_main      : the main model formula
#   show_n_row        : if TRUE (default), adds a per-year "N (Excl.)" row
#                       (complete-case N, with that year's excluded resolution
#                       count in parentheses) and its caption sentence. If FALSE,
#                       each year's block shows only NDE/NIE.
#   output_path       : full path for the saved .tex file (both parts)
#   caption_a, caption_b : LaTeX caption strings for each part
#   label_a, label_b  : LaTeX labels for each part
#
# Returns (invisibly) the assembled model_list (all 8 models).
build_regression_category_exclusion_table <- function(
    node_hr_list,
    trade_graphs_comp,
    formula_main,
    show_n_row = TRUE,
    output_path = "../output/tables/appendix/regression_category_exclusion.tex",
    caption_a = paste(
      "Sensitivity of the democracy (v2x\\_polyarchy) NDE/NIE estimates to each recurring and",
      "potentially politicized resolution category (see Table \\ref{tab:resolution-types} for",
      "category definitions; continued in Table \\ref{tab:regression-category-excl-b}), selected",
      "years (2000, 2005, 2010, 2015). Column (1) is the main model on the full 430-resolution",
      "sample. Columns (2)--(4) re-estimate the same model after dropping one category's resolutions",
      "at a time: globalization and human rights, combating glorification of Nazism, and racial",
      "discrimination and Durban follow-up, respectively. All columns include the same covariate",
      "controls and resolution fixed effects as the main model.",
      if (show_n_row) paste(
        "N (Excl.) is the number of complete-case country-votes used in that year's regression,",
        "with the number of that year's resolutions belonging to the category that column excludes",
        "shown in parentheses."
      )
    ),
    caption_b = paste(
      "Continuation of Table \\ref{tab:regression-category-excl-a}"
    ),
    label_a = "tab:regression-category-excl-a",
    label_b = "tab:regression-category-excl-b") {

  col_labels <- c(
    main_complete                 = "(1) Main",
    node_excl_globalization       = "(2) Excl. Globalization",
    node_excl_nazism              = "(3) Excl. Nazism",
    node_excl_racism_durban       = "(4) Excl. Racism/Durban",
    node_excl_right_to_dev        = "(5) Excl. Right to Dev.",
    node_excl_cultural_diversity  = "(6) Excl. Cultural Div.",
    node_excl_israel_palestine    = "(7) Excl. Israel/Palestine",
    node_excl_all                 = "(8) Excl. All"
  )

  # Category label(s) each column's "N resolutions excluded" row should count
  # — empty for the main column (excludes nothing), all six for node_excl_all.
  categories_by_model <- list(
    main_complete                 = character(0),
    node_excl_globalization       = "Globalization and human rights",
    node_excl_nazism              = "Combating glorification of Nazism",
    node_excl_racism_durban       = "Racial discrimination and Durban",
    node_excl_right_to_dev        = "Right to development",
    node_excl_cultural_diversity  = "Human rights and cultural diversity",
    node_excl_israel_palestine    = "Israel/Palestine",
    node_excl_all                 = names(build_category_patterns())
  )

  # 1. Build the six "main minus category X" subsets, plus "main minus all
  #    six", and align each to trade_graphs_comp.
  exclusion_subsets <- build_category_exclusion_subsets()
  exclusion_aligned <- purrr::map(
    exclusion_subsets,
    ~ split_dv_subset(.x, trade_graphs_comp)
  )

  # 2. Re-estimate the main model on each subset.
  purrr::iwalk(exclusion_aligned, function(aligned, name) {
    run_sensitivity_curves(
      formula      = formula_main,
      trade_graphs = aligned$trade_graphs,
      node_data    = aligned$node_list,
      coembedding  = "U",
      output_file  = paste0("../output/estimates/curve_", sub("^node_", "", name), "_complete.rds")
    )
  })

  # 3. Assemble the compact table: main model + one column per excluded category.
  model_list <- c(
    list(main_complete = load_curve_at_K(
      curve_rds  = "../output/estimates/curve_main_complete.rds",
      model_name = "main_complete",
      K_column   = "K_complete_lp1"
    )),
    purrr::imap(exclusion_aligned, function(aligned, name) {
      load_curve_at_K(
        curve_rds  = paste0("../output/estimates/curve_", sub("^node_", "", name), "_complete.rds"),
        model_name = name,
        K_column   = "K_complete_lp1"
      )
    })
  )

  # Per-year "N (Excl.)" values, e.g. "2611 (1)": complete-case N and the
  # number of that year's resolutions the column excludes.
  n_obs <- c(
    list(main_complete = compute_n_obs_by_year(node_hr_list, formula_main)),
    purrr::map(exclusion_aligned, \(a) compute_n_obs_by_year(a$node_list, formula_main))
  )

  n_excluded <- purrr::map(
    categories_by_model,
    \(labs) count_category_by_year(node_hr_list, labs)
  )

  n_obs_and_excluded <- purrr::map2(n_obs, n_excluded, function(obs_vec, excl_vec) {
    yrs <- names(obs_vec)
    set_names(sprintf("%d (%d)", obs_vec[yrs], excl_vec[yrs]), yrs)
  })

  # NULL tells build_estimates_tabular_lines() to skip the extra row.
  year_rows <- if (show_n_row) {
    list("N (Excl.)" = n_obs_and_excluded)
  } else {
    NULL
  }

  # 4. Part A: columns (1)-(4). Part B: columns (5)-(8). Each part is built
  # with build_estimates_tabular_lines() (4-tables.R) and gets its own
  # sidewaystable wrapper.
  part_a_names <- names(model_list)[1:4]
  part_b_names <- names(model_list)[5:8]

  # Keep NULL as NULL (purrr::map(NULL, ...) would return an empty list).
  year_rows_for <- function(model_names) {
    if (is.null(year_rows)) NULL
    else purrr::map(year_rows, \(by_model) by_model[model_names])
  }

  tex_a <- build_estimates_tabular_lines(
    model_list = model_list[part_a_names],
    col_labels = col_labels[part_a_names],
    year_rows  = year_rows_for(part_a_names)
  )
  tex_b <- build_estimates_tabular_lines(
    model_list = model_list[part_b_names],
    col_labels = col_labels[part_b_names],
    year_rows  = year_rows_for(part_b_names)
  )

  note <- "\\par\\smallskip\\footnotesize $^{***}$~95\\% CI excludes zero ($p < 0.05$, two-sided)."

  sidewaystable_block <- function(tabular_lines, caption, label) {
    c(
      "\\begin{sidewaystable}[H]",
      "\\centering",
      sprintf("\\caption{%s}", caption),
      sprintf("\\label{%s}", label),
      tabular_lines,
      note,
      "\\end{sidewaystable}"
    )
  }

  all_lines <- c(
    sidewaystable_block(tex_a, caption_a, label_a),
    "",
    sidewaystable_block(tex_b, caption_b, label_b)
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(all_lines, output_path)
  message("Table saved to: ", output_path)

  invisible(model_list)
}
