# Master script — sources all function definitions then runs the full pipeline
# in order. All actual data loading, cleaning, and saving happens here.
# Delete all files in the data/ and output/ folders before running to start fresh.

rm(list = ls())

pacman::p_load(
  tidyverse, countrycode, haven, dplyr,
  igraph, vroom, ggraph,
  gdim, parallel,
  latentnetmediate
)

# set seed
my_seed <- 50 

# 1. Source all function definitions -----------------------------------------

# Data cleaning code
source("../code_cleaning/sanity_check.R")
source("../code_cleaning/1-DV-cleaning.R")
source("../code_cleaning/2-IV-cleaning.R")
source("../code_cleaning/3-DV-IV-merge-polish.R")
source("../code_cleaning/4-trade-network-graph.R")
source("../code_cleaning/5-node-trade-align.R")
source("../code_cleaning/6_robustness_check.R")

# Analysis code
source("../code_analyze/1-trade-rank-selection.R")
source("../code_analyze/2-estimate-models.R")
source("../code_analyze/3-plots.R")
source("../code_analyze/4-tables.R")
source("../code_analyze/5-resolution-types.R")

# 2. Cleaning pipeline -------------------------------------------------------

# Ensure output directories exist before any saves
dir.create("../data",   recursive = TRUE, showWarnings = FALSE)
dir.create("../output", recursive = TRUE, showWarnings = FALSE)
dir.create("../output/estimates", recursive = TRUE, showWarnings = FALSE)

# Script 1: DV data
dv_data         <- build_dv_table()
final_countries <- build_country_list()
saveRDS(dv_data,         "../data/dv_data.rds")
saveRDS(final_countries, "../data/final_countries.rds")

# Script 2: IV and covariate data
vdem_data    <- build_vdem()
gdp_pop_data <- build_gdp_pop()
igo_data     <- build_igo()
hri_vdem     <- build_hri()
ucdp_data    <- build_ucdp()
saveRDS(vdem_data,    "../data/vdem_data.rds")
saveRDS(gdp_pop_data, "../data/gdp_pop_data.rds")
saveRDS(igo_data,     "../data/igo_data.rds")
saveRDS(hri_vdem,     "../data/hri_data.rds")
saveRDS(ucdp_data,    "../data/ucdp_data.rds")

# Script 3: Merge and polish
# Note: merge_data() may report a Serbia/Yugoslavia mismatch — expected, see
# build_country_list() in 1-DV-cleaning.R for the fix applied there.
merged_data   <- merge_data()
saveRDS(merged_data, "../data/dv_iv_covariates_merged.rds")

polished_data <- build_final_data()
saveRDS(polished_data, "../data/lagged_polished_list.rds")

# Script 4: Trade network graphs (builds and saves incomplete + complete)
build_trade_network_graphs()

# Script 5: Node-trade alignment (builds and saves aligned objects)
build_node_trade_aligned()

# Robustness: DV subsets and robust (zero-filled) trade graphs
build_dv_subsets()
build_robust_trade_graphs()



# 3. Analysis - run mediation models -------------------------------------------------------

# Analysis 1: Rank selection (parallel, saves K_results.rds)
# n_cores sets the number of parallel workers. Reduce it on machines with fewer
# than 8 cores or limited memory.
run_rank_selection(seed = my_seed, 
                   n_cores = 8, 
                   keep_matrix = FALSE)

# Analysis 2: Sensitivity curves (saves all curve RDS files to ../output/estimates/)
# Model formula:
#   vote_numeric, rcid: t+2 | v2x_polyarchy: t
#   l.gdp_s, l.pop_s, HR_index_s, WTO, EU, OIC, conflicts_lag0: t-1 | conflicts_lag1: t-2
formula_main <- vote_numeric ~
  v2x_polyarchy + l.gdp_s + l.pop_s +
  interstate_conflicts_lag0 +
  intrastate_conflicts_lag0 +
  interstate_conflicts_lag1 +
  intrastate_conflicts_lag1 +
  HR_index_s + WTO + EU + OIC +
  rcid

formula_alternative_dv <- vote_binary ~ 
  v2x_polyarchy + l.gdp_s + l.pop_s +
  interstate_conflicts_lag0 +
  intrastate_conflicts_lag0 +
  interstate_conflicts_lag1 +
  intrastate_conflicts_lag1 +
  HR_index_s + WTO + EU + OIC +
  rcid

formula_impvote_control <- vote_numeric ~
  v2x_polyarchy + l.gdp_s + l.pop_s +
  interstate_conflicts_lag0 +
  intrastate_conflicts_lag0 +
  interstate_conflicts_lag1 +
  intrastate_conflicts_lag1 +
  HR_index_s + WTO + EU + OIC +
  importantvote +
  rcid
# Note: importantvote is sourced from the US State Department's "Voting Practices" reports
# and is only coded through vote year 2017. Any models using importantvote must be
# restricted to trade years 2000–2016 (vote year = trade year + 1, so trade year 2016
# → vote year 2017 is the last valid year). Trade years 2017–2018 have all-NA
# importantvote and will cause lm() to error with "0 (non-NA) cases".
# See: impvote_years / trade_graphs_impvote / node_hr_impvote defined in Robustness Check 4 below.

# ── Load aligned data (shared across all model runs) ──────────────────────
node_trade_aligned_comp <- readRDS("../data/node-trade-aligned-complete.rds")

trade_graphs_comp <- map(node_trade_aligned_comp, \(x) x$graph)
node_hr_list      <- map(node_trade_aligned_comp,  \(x) x$nodes)

# ── Pre-split DV subsets (aligns node data and graphs to common years) ─────
node_full_eliminated <- readRDS("../data/dv_subset/node_full_eliminated.rds")

aligned_FE   <- split_dv_subset(node_full_eliminated, trade_graphs_comp)

# ── Main analyses ──────────────────────────────────────────────────────────
run_sensitivity_curves(
  formula      = formula_main,
  trade_graphs = trade_graphs_comp,
  node_data    = node_hr_list,
  coembedding  = "U",
  output_file  = "../output/estimates/curve_main_complete.rds"
)

# ── Robustness Check 2: import trade network (coembedding = "V") ─────────────────────
run_sensitivity_curves(
  formula      = formula_main,
  trade_graphs = trade_graphs_comp,
  node_data    = node_hr_list,
  coembedding  = "V",
  output_file  = "../output/estimates/curve_import_comp.rds"
)

# ── Robustness Check 3: alternative DV (vote_binary) ─────────────────────────────────
run_sensitivity_curves(
  formula      = formula_alternative_dv,
  trade_graphs = trade_graphs_comp,
  node_data    = node_hr_list,
  coembedding  = "U",
  output_file  = "../output/estimates/curve_alt_dv_complete.rds"
)

# ── Robustness Check 4: importantvote control ─────────────────────
# importantvote is only available through vote year 2017.
# Since vote year = trade year + 1, trade years >= 2017 (vote year >= 2018)
# have all-NA importantvote and must be excluded to avoid lm() 0-case errors.
impvote_years        <- as.character(2000:2016)
trade_graphs_impvote <- trade_graphs_comp[impvote_years]
node_hr_impvote      <- node_hr_list[impvote_years]

run_sensitivity_curves(
  formula      = formula_impvote_control,
  trade_graphs = trade_graphs_impvote,
  node_data    = node_hr_impvote,
  coembedding  = "U",
  output_file  = "../output/estimates/curve_impvote_controls_complete.rds"
)


# ── Robustness Check 5: resolution subsets ───────────────────────────────────────────
run_sensitivity_curves(
  formula      = formula_main,
  trade_graphs = aligned_FE$trade_graphs,
  node_data    = aligned_FE$node_list,
  coembedding  = "U",
  output_file  = "../output/estimates/curve_subset_full_eliminated_complete.rds"
)


# 4. Plots - NDE and NIE, latent ranks ------------------------------------


# Analysis 3: Plots
# plot_model_results() loads the curve RDS and draws the all-years time-trend
# panel, plus sensitivity-across-K plots for sensitivity_years (main model only).
# Main-text figures go in ../output/plots/, appendix figures in ../output/plots/appendix/.
# time_trend_filename must be unique across models to avoid overwriting.

# ── Main model: complete trade graph ───────────────────────────────────────
plot_model_results(
  curve_rds           = "../output/estimates/curve_main_complete.rds",
  K_column            = "K_complete_lp1",
  output_dir          = "../output/plots",
  time_trend_subtitle = "Points represent estimates with 95% CIs — complete export trade graph",
  time_trend_filename = "time_trend_NDE_NIE_complete.png",
  sensitivity_years   = c(2000, 2005, 2010, 2015, 2018),
  sensitivity_dir     = "../output/plots/appendix",
  ylim_range          = c(-0.5, 0.5)
)

# ── Robustness Check 2: import trade network (coembedding = "V") ───────────
plot_model_results(
  curve_rds           = "../output/estimates/curve_import_comp.rds",
  K_column            = "K_complete_lp1",
  output_dir          = "../output/plots/appendix",
  time_trend_subtitle = "Points represent estimates with 95% CIs — complete import trade graph (V embedding)",
  time_trend_filename = "time_trend_NDE_NIE_import_complete.png",
  ylim_range          = c(-0.5, 0.5)
)

# ── Robustness Check 3: alternative DV (vote_binary) ───────────────────────
plot_model_results(
  curve_rds           = "../output/estimates/curve_alt_dv_complete.rds",
  K_column            = "K_complete_lp1",
  output_dir          = "../output/plots/appendix",
  time_trend_subtitle = "Points represent estimates with 95% CIs — binary DV",
  time_trend_filename = "time_trend_NDE_NIE_alt_dv_complete.png"
)

# ── Robustness Check 4: importantvote control (trade years 2000–2016 only) ─
# importantvote coverage ends at vote year 2017 (= trade year 2016 + 1).
plot_model_results(
  curve_rds           = "../output/estimates/curve_impvote_controls_complete.rds",
  K_column            = "K_complete_lp1",
  output_dir          = "../output/plots/appendix",
  time_trend_subtitle = "Points represent estimates with 95% CIs — importantvote control (2000–2016)",
  time_trend_filename = "time_trend_NDE_NIE_impvote_controls_complete.png"
)

# ── Robustness Check 5a: resolution subset — full eliminated ───────────────
plot_model_results(
  curve_rds           = "../output/estimates/curve_subset_full_eliminated_complete.rds",
  K_column            = "K_complete_lp1",
  output_dir          = "../output/plots/appendix",
  time_trend_subtitle = "Points represent estimates with 95% CIs — full eliminated resolutions",
  time_trend_filename = "time_trend_NDE_NIE_subset_full_eliminated.png"
)

# 5. Plot - Trade network ----------------------------------------------------

trade_graphs_comp <- readRDS("../data/node-trade-aligned-complete.rds") |>
  map(\(x) x$graph)

node_hr_list <- readRDS("../data/node-trade-aligned-complete.rds") |>
  map(\(x) x$nodes)

trade_targets <- c(
  "United States",
  "China",
  "Canada",
  "Russia",
  "Germany",
  "Afghanistan",
  "Cyprus",
  "Saudi Arabia",
  "Syria",
  "Qatar",
  "Japan",
  "Moldova",
  "Belgium"
)

plot_trade_network(trade_graphs_comp[["2014"]],
                   year      = 2014,
                   node_data = node_hr_list[["2014"]],
                   seed      = my_seed,
                   targets   = trade_targets,
                   output_dir = "../output/plots")


# 6. Plot - PCA plots -----------------------------------------------------
target_years <- c("2000", "2005", "2010", "2015", "2018")

aligned   <- readRDS("../data/node-trade-aligned-complete.rds")
A_list    <- map(aligned[target_years], function(x) {
  A   <- igraph::as_adjacency_matrix(x$graph, attr = "sum_trade")
  A@x <- round(log1p(A@x))
  A
})
node_list <- map(aligned[target_years], \(x) x$nodes)

scores_df <- build_pca_scores(A_list, node_list, polyarchy_col = "v2x_polyarchy")

pca_targets <- c(
  "United States",
  "China",
  "Canada",
  "Russia",
  "Germany",
  "Afghanistan",
  "Cyprus",
  "Saudi Arabia",
  "Syria",
  "Moldova",
  "Senegal",
  "Japan",
  "Nicaragua",
  "Qatar",
  "Belgium",
  "Singapore",
  "New Zealand",
  "Thailand",
  "United Arab Emirates"
)

plot_pca_trade_network(
  scores_df      = scores_df,
  to_label       = pca_targets,
  flip_pc2_years = c("2005", "2010", "2015", "2018"), #flipped these years to match the direction
  output_dir     = "../output/plots",
  filename       = "PCA_allyears_complete_lp1.pdf"
)

scores_df<- scores_df |>
  dplyr::mutate(PC2_flipped = ifelse(as.character(year) %in% c("2005", "2010", "2015", "2018"), -PC2, PC2))

saveRDS(
  scores_df,
  "../output/estimates/pca_scores_complete.rds"
)


# 7. Table - Regression tables -----------------------------------------------
# impvote_complete is estimated on trade years 2000–2016 (importantvote ends at vote year 2017);
# tables show 2000, 2005, 2010, 2015 only (no 2018).

# ── Robustness table (compact): v2x_polyarchy NDE/NIE only, all models ────
# Rows = 5 selected years × {NDE, NIE}. Columns = one per model.
# Main model included as the reference column.
robustness_model_list <- list(
  main_complete = load_curve_at_K(
    curve_rds  = "../output/estimates/curve_main_complete.rds",
    model_name = "main_complete",
    K_column   = "K_complete_lp1"
  ),
  import_complete = load_curve_at_K(
    curve_rds  = "../output/estimates/curve_import_comp.rds",
    model_name = "import_complete",
    K_column   = "K_complete_lp1"
  ),
  alt_dv_complete = load_curve_at_K(
    curve_rds  = "../output/estimates/curve_alt_dv_complete.rds",
    model_name = "alt_dv_complete",
    K_column   = "K_complete_lp1"
  ),
  impvote_complete = load_curve_at_K(
    curve_rds  = "../output/estimates/curve_impvote_controls_complete.rds",
    model_name = "impvote_complete",
    K_column   = "K_complete_lp1",
    years      = c(2000, 2005, 2010, 2015)
  ),
  subset_full_eliminated = load_curve_at_K(
    curve_rds  = "../output/estimates/curve_subset_full_eliminated_complete.rds",
    model_name = "subset_full_eliminated",
    K_column   = "K_complete_lp1"
  )
)

robustness_col_labels <- c(
  main_complete          = "(1) Main",
  import_complete        = "(2) Import Network",
  alt_dv_complete        = "(3) Binary DV",
  impvote_complete       = "(4) Imp. Vote",
  subset_full_eliminated = "(5) Subset FE"
)

robustness_descriptors <- list(
  "Trade network"      = c(
    main_complete          = "Export",
    import_complete        = "Import",
    alt_dv_complete        = "Export",
    impvote_complete       = "Export",
    subset_full_eliminated = "Export"
  ),
  "Dependent variable" = c(
    main_complete          = "Numeric",
    import_complete        = "Numeric",
    alt_dv_complete        = "Binary",
    impvote_complete       = "Numeric",
    subset_full_eliminated = "Numeric"
  ),
  "Importantvote ctrl" = c(
    main_complete          = "No",
    import_complete        = "No",
    alt_dv_complete        = "No",
    impvote_complete       = "Yes",
    subset_full_eliminated = "No"
  ),
  "Resolution subset"  = c(
    main_complete          = "All",
    import_complete        = "All",
    alt_dv_complete        = "All",
    impvote_complete       = "All",
    subset_full_eliminated = "FE only"
  ),
  "Controls"           = c(
    main_complete          = "Yes",
    import_complete        = "Yes",
    alt_dv_complete        = "Yes",
    impvote_complete       = "Yes",
    subset_full_eliminated = "Yes"
  )
)

build_main_estimates_table(
  model_list  = robustness_model_list,
  col_labels  = robustness_col_labels,
  descriptors = robustness_descriptors,
  output_path = "../output/tables/appendix/regression_robustness_compact.tex",
  caption = "Robustness check: NDE/NIE estimates for democracy (v2x\\_polyarchy) at best K across model specifications ($k = 10, 12, 11, 12, 11$, respectively; chosen using the approach in \\citet{Chen.et.al.2021}), selected years (2000, 2005, 2010, 2015, 2018). The first model (1) is the main model (which is estimated with the export trade graph). Robustness check models (2)–(5) estimate on an import trade graph, an alternative binary DV, an \\texttt{importantvote} control, and a restricted sample that excludes all resolutions concerning highly politicized issues (Israel/Palestine, right to development, and cultural diversity), respectively. All models include the set of covariate controls from the main model, measured in the year preceding the democracy variable (V-Dem score): scaled GDP and population, human rights index, WTO/EU/OIC membership status, and the presence of inter-state and intra-state conflicts (measured in the covariate year and the year prior). Resolution fixed effects are also included.",
)


# ── Subset excluding each resolution category ───────────────────────────────
# build_regression_category_exclusion_table() (5-resolution-types.R) builds the six
# "main minus category X" subsets, re-estimates the main model on each, and
# writes the comparison table (main model + one column per excluded
# category).
build_regression_category_exclusion_table(
  node_hr_list      = node_hr_list,
  trade_graphs_comp = trade_graphs_comp,
  formula_main      = formula_main,
  show_n_row        = FALSE
)


# 8. Table - Example resolutions by issue category ---------------------------

# ── Table 1: Example resolutions by issue category. (RD/CD subset from 6_robustness_check.R) ────
build_resolution_example_table(
  n_must      = 2,
  seed = my_seed,
  output_path = "../output/tables/resolution_examples.tex",
  caption     = "Example resolutions in Human Rights (hr) issue category, categorized in the UNGA votes dataset (unvotes package).
Resolutions in the table are randomly sampled from each year of the the UNGA votes data set, with two further resolutions
sampled among cultural diversity and right to development resolutions to illustrate examples of each.",
  label       = "tab:resolution-examples"
)

# ── Table 1b: Resolution types — counts, years recurring, % of total ────────
# Same analytic sample as the example table above (see 5-resolution-types.R).
build_resolution_type_table(
  output_path = "../output/tables/appendix/resolution_types.tex",
  label       = "resolution-types"
)

# 9. Scenarios ---------------------------------------------------------------

# Counterfactual trade embedding scenarios
K_results <- readRDS("../data/rank_selection/K_results.rds")

trade_graphs_comp <- readRDS("../data/node-trade-aligned-complete.rds") |>
  map(\(x) x$graph)
node_hr_list <- readRDS("../data/node-trade-aligned-complete.rds") |>
  map(\(x) x$nodes)

formula_main <- vote_numeric ~
  v2x_polyarchy + l.gdp_s + l.pop_s +
  interstate_conflicts_lag0 +
  intrastate_conflicts_lag0 +
  interstate_conflicts_lag1 +
  intrastate_conflicts_lag1 +
  HR_index_s + WTO + EU + OIC +
  rcid

# ── NIE scenarios: swap trade embeddings (X), hold covariates (W) fixed ──────
# Source keeps its own democracy and all covariates; only trade position changes.
nie_scenarios <- list(

  # 1. Mid democracy, [low trade → high trade] embeddings
  case_moldova_japan = list(source = "Moldova",
                            alternatives = c("Japan")),

  # 2. Low democracy, [low trade → high trade] embeddings
  case_syria_qatar  = list(source = "Syria",
                           alternatives = c("Qatar"))
)

cf_nie_results <- run_counterfactual_scenarios(
  scenarios    = nie_scenarios,
  trade_graphs = trade_graphs_comp,
  node_data    = node_hr_list,
  formula      = formula_main,
  K_results    = K_results,
  K_column     = K_complete_lp1,
  years        = c("2015"),
  coembedding  = "U"
)
print(cf_nie_results)
####  Results  
# 1) Moldova (actual mean vote 0.526, predicted own 0.694) adopting:
# Japan's trade embeddings  → predicted 0.816 (delta +0.123)

# 2) Syria (actual 0.684, predicted own 0.830) adopting:
# Qatar's trade embeddings → predicted 0.975 (delta +0.144)

saveRDS(cf_nie_results, "../output/estimates/counterfactual_nie_scenarios.rds")


# ── NDE scenarios: swap v2x_polyarchy in W, hold trade embeddings (X) fixed ──
# Source keeps its own trade network position; only democracy score changes.
nde_scenarios <- list(
  # Low democracy country adopts high democracy country's v2x_polyarchy
  case_saudiarabia_canada  = list(source = "Saudi Arabia", alternatives = c("Canada")),
  case_nicaragua_cyprus    = list(source = "Nicaragua",    alternatives = c("Cyprus"))
)

cf_nde_results <- run_counterfactual_democracy_scenarios(
  scenarios    = nde_scenarios,
  trade_graphs = trade_graphs_comp,
  node_data    = node_hr_list,
  formula      = formula_main,
  K_results    = K_results,
  K_column     = K_complete_lp1,
  years        = c("2015"),
  coembedding  = "U"
)

print(cf_nde_results)
####  Results  
# Saudi Arabia (actual 0.868, predicted own 0.917) adopting Canada's democracy:
#  predicted 0.865 (delta -0.0520) — HR vote decreases slightly

# Nicaragua (actual 0.789, predicted own 0.950) adopting Cyprus's democracy:
#   predicted 0.915 (delta -0.0343) — HR vote decreases slightly

saveRDS(cf_nde_results, "../output/estimates/counterfactual_nde_scenarios.rds")



# Appendix ---------------------------------------------------------


## Table - summary statistics ------------------------------------------
build_summary_stats_table(
  years       = 2000:2018,
  output_path = "../output/tables/appendix/summary_stats.tex",
  caption     = "Summary statistics of nodal variables. For character variables (country and year), N indicates the number of unique values. The final dataset covers 19 years and 170 countries in total.",
  label       = "summary-stats"
)


## Table - lagged year structure ------------------------------------------
build_lagged_years_table(
  years       = 2000:2018,
  output_path = "../output/tables/appendix/lagged_years_summary.tex",
  caption     = "Year structure used in the analysis. Each row corresponds to one trade year. Covariates are measured one year before the democracy score (t-1); UNGA votes one year after the trade network (t+2).",
  label       = "lagged-years-summary"
)

## Figure - v2xpolyarchy distribution ------------------------------------------
plot_vdem_distribution(
  years      = 2000:2018,
  output_dir = "../output/plots/appendix",
  width           = 5,
  height          = 4
)



## PCA results for all years -----------------------------------------------
# Computes PCA scores for every year (2000–2018) and adds a PC2_flipped column with a consistent sign convention across all years.
# PCA axes are sign-arbitrary: the same underlying dimension can appear as +PC2 or -PC2 in different years. Rather than hardcoding the flip for only the 5 plot years, we auto-detect the flip direction each year by checking the sign of a reference country's PC2 loading.

# Reference country: Grenada, whose PC2 loading is among the largest and most stable across 2000–2018.
#   When Grenada's loading is negative in a given year, the whole axis is flipped so that PC2_flipped always points in the same direction.

aligned   <- readRDS("../data/node-trade-aligned-complete.rds")
all_years <- names(aligned)

A_list_all <- map(aligned[all_years], function(x) {
  A   <- igraph::as_adjacency_matrix(x$graph, attr = "sum_trade")
  A@x <- round(log1p(as.numeric(A@x)))
  A
})
node_list_all <- map(aligned[all_years], \(x) x$nodes)

scores_df_all <- build_pca_scores(
  A_list        = A_list_all,
  node_list     = node_list_all,
  polyarchy_col = "v2x_polyarchy",
  ref_country   = "Grenada"
)

saveRDS(scores_df_all, "../output/estimates/pca_scores_complete_allyears.rds")


## PC2 vs trade correlation — log1p version (2014) ----------------------------
# Uses scores_df_all (built above), with log1p-transformed trade values,
# consistent with the regression table.
plot_pc2_trade_correlation(
  scores_df         = scores_df_all,
  trade_graph       = aligned[["2014"]]$graph,
  year              = 2014,
  pc2_col           = "PC2_flipped",
  log_transform     = TRUE,
  output_dir_export = "../output/plots/appendix",
  output_dir_import = "../output/plots/appendix"
)

## PC2 trade regression table --------------------------------------
# Uses scores_df_all and aligned from above (both still in scope).
build_pc2_trade_regression_table(
  scores_df   = scores_df_all,
  trade_graph = aligned[["2014"]]$graph,
  year        = 2014,
  output_path = "../output/tables/appendix/pc2_trade_regression.tex",
  caption     = "Regression of PC2 (sign-aligned) on trade volume and composition using 2014 trade network data.
Models (1) and (2) use log-transformed import and export flows respectively, as raw trade values are heavily right-skewed. Model (3) uses import share $= \\text{imports} / (\\text{imports} + \\text{exports})$
untransformed, as it is already a bounded ratio ranging from 0 (pure exporter) to 1 (pure importer). Similar to Figure \\ref{fig:pca_allyrs}, PC2 scores are sign-normalized across years to ensure a consistent direction.",
  label       = "tab:pc2_trade_regression"
)
