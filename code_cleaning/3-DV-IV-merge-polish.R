# Merge all clean data ----------------------------------------------------

merge_data <- function() {
  
  dv_data      <- readRDS("../data/dv_data.rds")
  vdem_data    <- readRDS("../data/vdem_data.rds")
  gdp_pop_data <- readRDS("../data/gdp_pop_data.rds")
  igo_data     <- readRDS("../data/igo_data.rds")
  hri_data     <- readRDS("../data/hri_data.rds")
  ucdp_data    <- readRDS("../data/ucdp_data.rds")
  
  merged_data <- dv_data |>
    # 2) IV: vdem democracy index
    left_join(vdem_data,    by = c("fixed.name", "cown", "year")) |>
    # 3) GDP and population controls
    left_join(gdp_pop_data, by = c("fixed.name", "cown", "year")) |>
    # 4) IGO memberships
    left_join(igo_data,     by = c("fixed.name", "cown", "year")) |>
    # 5) Human rights index
    left_join(hri_data,     by = c("fixed.name", "cown", "iso3c", "year")) |>
    # 6) Conflict
    left_join(ucdp_data,    by = c("fixed.name", "cown", "year"))
  
  mismatches <- merged_data |>
    filter(fixed.name != country) |>
    distinct(fixed.name, country)
  
  if (nrow(mismatches) > 0) {
    message("Warning: the following countries have mismatched fixed.name and country:")
    print(mismatches)
  }
  
  return(merged_data)
}



# Final polishing for analysis --------------------------------------------
# build_final_data(): MASTER function — orchestrates the full pipeline.
#   Calls make_node_data() for each year, then applies conflict binning and
#   scaling. Returns a named list of polished node data frames, one per
#   trade year (2000–2018), ready for linking with trade graphs.

build_final_data <- function() {
  merged_data <- readRDS("../data/dv_iv_covariates_merged.rds")
  
  # ── Variable lists ─────────────────────────────────────────────────────
  dv_vars  <- c("rcid", "short", "descr", "importantvote", "vote_numeric", "vote_binary")
  iv_vars  <- c("v2x_polyarchy")
  cov_vars <- c("gdp", "pop",
                "intrastate_conflicts", "interstate_conflicts",
                "WTO", "EU", "OIC", "HR_index")
  
  # ── Year setup ─────────────────────────────────────────────────────────
  # democracy (t), trade (t+1), UNGA votes (t+2), other covs (t-1), conflict (t-2 to t-1)
  iv_years  <- 1999:2017
  dv_years  <- iv_years + 2
  cov_years <- iv_years - 1
  
  # ── Build per-year node data ───────────────────────────────────────────
  node_list <- pmap(
    list(iv_yr = iv_years, dv_yr = dv_years, cov_yr = cov_years),
    function(iv_yr, dv_yr, cov_yr) 
      make_node_data(iv_yr, dv_yr, cov_yr, merged_data, dv_vars, iv_vars)
  ) |>
    set_names(paste0("node_", iv_years + 1)) #trade year will be iv_year + 1
  
  # ── Polish: conflict bins, scaling, HR filter ──────────────────────────
  node_list_hr <- node_list |>
    map(make_conflict_bins) |>
    map(scale_node_vars) 
  
  return(node_list_hr)
  
}


## included in the build_final_data(): 

# make_node_data(): WORKER function — builds one single year's node data frame.
#   Takes three staggered years (iv_yr, dv_yr, cov_yr) and joins DV, IV, and
#   covariates from the merged data. Called repeatedly by build_final_data()
#   via pmap(), once per year combination.

make_node_data <- function(iv_yr, dv_yr, cov_yr, data, dv_vars, iv_vars) {
  
  DVs <- data |>
    filter(year == dv_yr) |>
    dplyr::select(fixed.name, cown, year, all_of(dv_vars)) |>
    rename(year_DV = year) |>
    mutate(year_IV = iv_yr)
  
  IV_regime <- data |>
    filter(year == iv_yr) |>
    dplyr::select(fixed.name, cown, year, all_of(iv_vars)) |>
    rename(year_IV = year) |>
    distinct()
  
  base_covs <- data |>
    filter(year == cov_yr) |>
    dplyr::select(fixed.name, cown, year,  
                  gdp, pop, #all covariates except for conflict vars
                  WTO, EU, OIC, HR_index) |>
    rename(year_cov = year) |>
    distinct()
  
  conflict_lags <- data |>
    filter(year >= cov_yr - 1, year <= cov_yr) |> #get conflict counts in cov_year and cov_year -1 
    dplyr::select(fixed.name, cown, year,
                  intrastate_conflicts, interstate_conflicts) |>
    distinct() |>
    mutate(lag = cov_yr - year) |>
    pivot_wider(
      id_cols     = c(fixed.name, cown),
      names_from  = lag,
      values_from = c(intrastate_conflicts, interstate_conflicts),
      names_glue  = "{.value}_lag{lag}"
    )
  
  IVs <- IV_regime |>
    left_join(base_covs,     by = c("fixed.name", "cown")) |>
    left_join(conflict_lags, by = c("fixed.name", "cown")) |>
    mutate(year_IV = iv_yr)
  
  DVs |>
    left_join(IVs, by = c("fixed.name", "cown", "year_IV")) |>
    dplyr::select(fixed.name, cown, year_IV, year_DV, everything())
}


# make_conflict_bins(): HELPER function — converts raw conflict count variables
#   (lag0, lag1) into binary indicators (1 = conflict present, 0 = none).
#   Applied after make_node_data() as a post-processing step.

make_conflict_bins <- function(df) {
  df |>
    mutate(across(
      matches("^(inter|intra)state_conflicts_lag[0-1]$"),
      ~ as.integer(.x > 0),
      .names = "{.col}_bin"
    ))
}


# scale_node_vars(): HELPER function — log-transforms and standardizes gdp, pop,
#   and HR_index for use in regression. No rows are dropped — NA handling is done
#   at the variable level, not the row level, so only the specific variable is NA
#   for that country-year while all other variables remain intact.
#
#   Two issues addressed:
#   1. Non-positive gdp/pop: log(0) = -Inf and log(negative) = NaN, which would
#      silently corrupt the scaled variable. if_else(gdp > 0, gdp, NA_real_)
#      converts non-positive values to NA before log(), avoiding this.
#   2. NA values in scale(): scale() computes mean and sd excluding NAs by default,
#      then returns NA for those positions. So countries with missing gdp/pop/HR_index
#      get NA for that scaled variable but remain in the dataset for other variables.

scale_node_vars <- function(df) {
  
  result <- df |>
    mutate(
      l.gdp_s    = as.numeric(scale(log(if_else(gdp > 0,  gdp, NA_real_)))),
      gdp_is_na  = ifelse(is.na(gdp), 1, 0), #binary indicator for missing gdp: e.g., North Korea (full years), Afghanistan (early years), South Sudan (late years), Venezuela (late years) - 1% of total gdp data
      l.pop_s    = as.numeric(scale(log(if_else(pop > 0,  pop, NA_real_)))),
      HR_index_s = as.numeric(scale(HR_index)),
      name       = fixed.name
    )
  
  # Check: how many NAs were produced in each scaled variable
  na_check <- result |>
    summarise(
      n_total       = n(),
      na_l.gdp_s    = sum(is.na(l.gdp_s)),
      na_l.pop_s    = sum(is.na(l.pop_s)),
      na_HR_index_s = sum(is.na(HR_index_s))
    )
  
  if (any(na_check > 0)) {
    message("scale_node_vars — NAs in scaled variables:")
    print(na_check)
  }
  
  return(result)
}




