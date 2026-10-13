# IV: democracy score - Vdem ----------------------------------------------
# taken from `vdemdata` R package pulled on March 12, 2026, and saved in raw data folder 
build_vdem <- function() {
  vdem <- readRDS("../raw data/vdem.rds")
  final_countries <- readRDS("../data/final_countries.rds")
  
  vdem.fixed <- final_countries |>
    left_join(vdem |> rename(cown = COWcode), by = "cown") |> 
    dplyr::select(year, fixed.name, cown, v2x_polyarchy, v2x_libdem)
  
  return(vdem.fixed)
}


# Covariate 1: GDP and Population - WDI  ----------------------------------
# taken from `WDI` R package pulled on March 12, 2026, and saved in raw data folder 
build_gdp_pop <- function() {
  final_countries <- readRDS("../data/final_countries.rds")
  gdp.pop <- readRDS("../raw data/gdp.pop.rds")
  
  gdp.pop.fixed <- final_countries |>
    left_join(gdp.pop, by = "iso3c") |>
    mutate(gdp = NY.GDP.MKTP.CD,
           pop = SP.POP.TOTL) |>
    filter(!is.na(fixed.name)) |>
    dplyr::select(year, fixed.name, cown, iso2c, iso3c, gdp, pop)
  
  return(gdp.pop.fixed)
}



# Covariate 2: IGO membership (WTO/EU/OIC) - COW v4 -----------------------
# COW IGO membership codebook (v3): https://correlatesofwar.org/wp-content/uploads/IGO-Codebook_v3_short-copy.pdf
# The v4 codebook is not yet published; check for an updated v4 codebook.
# No Membership 0
# Full Membership 1
# Associate Membership 2
# Observer 3: watching/non-voting presence, not a member
# Missing data -9
# State Not System Member -1: country not recognized as a state that year

# NOTE: data correction 1) UK/EU status after 2020 (should be 0 from 2021 onward), but this is outside our data coverage.

# NOTE: data correction 2) Syria's OIC suspension (2012 to March 2025) is not reflected in the v4 data. We code Syria as a non-member (0) from 2012 to 2024.


build_igo <- function() {
  
  igo <- haven::read_dta("../raw data/state_year_v4_extract.dta")
  final_countries <- readRDS("../data/final_countries.rds")
  
  IGO <- final_countries |>
    left_join(igo |> rename(cown = ccode), by = "cown") |>
    dplyr::select(fixed.name, cown, year, WTO, EU, OIC) |> 
    mutate(
      WTO = case_when(
        WTO == 3 ~ 0,  # treat "observer" as non-member
        TRUE ~ WTO
        ),
      EU = case_when( #fix UK's EU status from 2021 onward
        fixed.name == "United Kingdom" & year >= 2021 ~ 0,
        TRUE ~ EU
        ),
      OIC = case_when( #fix Syria's OIC status from 2012 onward
        fixed.name == "Syria" & year >= 2012 & year <= 2024 ~ 0,
        TRUE ~ OIC
        )
    )
  
  bad <- IGO |> filter((WTO == -1 | EU == -1 | OIC == -1) & year >= 2000)
  if (nrow(bad) > 0) {
    warning(
      "Found ", nrow(bad),
      " countries with -1 (not recognized as a state) in wto/eu/oic (year >= 2000): ",
      paste(unique(bad$fixed.name), collapse = ", ")
    )
  }
  
  return(IGO)
}



# Covariate 3: Human rights index - Vdem ----------------------------------
# Civil Liberties Index by the V-dem project

build_hri <- function() {
  vdem <- readRDS("../raw data/vdem.rds")
  final_countries <- readRDS("../data/final_countries.rds")
  
  vdem_hri <- vdem |>
    rename(iso3c = country_text_id) |>
    dplyr::select(year, iso3c, v2x_civlib)
  
  hri <- final_countries |>
    left_join(vdem_hri, by = "iso3c") |> 
    rename(HR_index = v2x_civlib) 
  
  return(hri)
}


# Covariate 4: Inter-state and Intra-state Conflict - UCDP/PRIO -----------

# ** Type of Conflict: 
# 1. Extrasystemic armed conflict occurs between a state and a non-state group outside its own territory. (In the COW project, extrasystemic war is subdivided into colonial war and imperial war, but this distinction is not used here.) These conflicts are by definition territorial, since the government side is fighting to retain control of a territory outside the state system.
# 2. Interstate armed conflict occurs between two or more states.
# 3. Internal armed conflict occurs between the government of a state and one or more internal opposition group(s) without intervention from other states.
# 4. Internationalized internal armed conflict occurs between the government of a state and one or more internal opposition group(s) with intervention from other states (secondary parties) on one or both sides.


# Build UCDP country-year conflict dataset 
# Input: a) final_countries.rds — final country list with cown and fixed.name
#        b) ucdp_dyadic_v25_1.csv  — UCDP Dyadic Dataset v25.1 (dyad-year level)
# Output: ucdp_final — country-year panel with conflict counts
#           fixed.name         : standardized country name
#           cown               : Correlates of War country code
#           year               : calendar year
#           intrastate_conflicts: number of distinct intrastate conflicts (type 1, 3, and
#                                 type 4 for the host government)
#           interstate_conflicts: number of distinct interstate conflicts (type 2, and
#                                 type 4 for foreign interveners)
#           total_conflicts    : total number of distinct conflicts
#           (years with no conflict are filled with 0)


# Type 4 (internationalized intrastate) conflict coding:
#   The codebook defines four roles in type 4 conflicts:
#     gwno_a     — primary party Side A: always the HOST GOVERNMENT whose
#                  territory/government is disputed. Coded as INTRASTATE,
#                  since the conflict is fundamentally domestic.
#     gwno_a_2nd — secondary party supporting Side A: a FOREIGN STATE
#                  intervening with troops on the government side.
#                  Coded as INTERSTATE from that country's perspective.
#     gwno_b     — primary party Side B: always a REBEL/OPPOSITION GROUP
#                  (non-state actor). Has no GW country code, so filtered
#                  out automatically by is.na(gwn_num).
#     gwno_b_2nd — secondary party supporting Side B: a FOREIGN STATE
#                  intervening with troops on the rebel side.
#                  Coded as INTERSTATE from that country's perspective.


# Unit of analysis and deduplication:
#   Raw data is dyad-year level, and one conflict (conflict_id) can involve
#   several dyads in the same year (e.g. a government fighting several rebel
#   groups, or several states on one side of an interstate war). We count
#   conflicts, not dyads: after each country-role is expanded into its own row,
#   distinct() on (fixed.name, cown, year, conflict_id, conflict_class) keeps
#   each conflict once per country-year before counting.

build_ucdp <- function() {
  
  fc <- readRDS("../data/final_countries.rds")
  
  ucdp_dyad <- read_csv("../raw data/ucdp_dyadic_v25_1.csv",
                        col_types = cols(
                          dyad_id          = col_double(),
                          conflict_id      = col_double(),
                          year             = col_double(),
                          type_of_conflict = col_double(),
                          gwno_a           = col_character(),
                          gwno_a_2nd       = col_character(),
                          gwno_b           = col_character(),
                          gwno_b_2nd       = col_character(),
                          .default         = col_skip()
                        ))
  
  # ── 1. Classify conflict types ─────────────────────────────────────────
  # type4 = "internationalized intrastate": a civil war with foreign interveners.
  # UCDP coding convention:
  #   gwno_a     = host state (always, gwno_b is always empty for type4)
  #   gwno_a_2nd = external states supporting the host govt  -> interstate
  #   gwno_b_2nd = external states supporting the rebels     -> interstate
  #   gwno_b     = always empty (rebel side has no GWN code) -> dropped by filter
  #
  # Exception: conflict_id 418 (US vs al-Qaeda) is coded as type4 with US as gwno_a
  # because 9/11 occurred on US soil, but the conflict is transnational in nature.
  # The US is reclassified to interstate to reflect its role as the primary
  # external military actor rather than a domestic counterinsurgent.
  
  dy <- ucdp_dyad |>
    mutate(conflict_class = case_when(
      type_of_conflict %in% c(1, 3) ~ "intrastate",
      type_of_conflict == 2          ~ "interstate",
      type_of_conflict == 4          ~ "type4"
    ))
  
  # ── 2. Pivot and separate actor roles ──────────────────────────────────
  actor_year <- dy |>
    mutate(across(c(gwno_a, gwno_a_2nd, gwno_b, gwno_b_2nd), as.character)) |>
    dplyr::select(dyad_id, conflict_id, year, conflict_class,
                  gwno_a, gwno_a_2nd, gwno_b, gwno_b_2nd) |>
    pivot_longer(
      cols      = c(gwno_a, gwno_a_2nd, gwno_b, gwno_b_2nd),
      names_to  = "role",
      values_to = "gwn_list"
    ) |>
    separate_rows(gwn_list, sep = ",") |>
    mutate(gwn_num = as.numeric(str_trim(gwn_list))) |>
    filter(!is.na(gwn_num)) |>
    mutate(
      conflict_class = case_when(
        # Exception: conflict 418 (US-alQaeda) - US in gwno_a is interstate
        conflict_id == 418 & conflict_class == "type4" & role == "gwno_a" ~ "interstate",
        # Standard type4 rules
        conflict_class == "type4" & role == "gwno_a"                      ~ "intrastate",
        conflict_class == "type4" & role %in% c("gwno_a_2nd", "gwno_b_2nd") ~ "interstate",
        # gwno_b is always empty for type4 so this never fires, but kept for safety
        conflict_class == "type4" & role == "gwno_b"                      ~ "intrastate",
        TRUE ~ conflict_class
      )
    ) |>
    dplyr::select(-role, -gwn_list)
  
  # ── 3. Join to final countries ─────────────────────────────────────────
  actor_year_fixed <- fc |>
    left_join(actor_year |> rename(cown = gwn_num), by = "cown")
  
  # ── 4. Deduplicate by CONFLICT_ID ──────────────────────────────────────
  country_year <- actor_year_fixed |>
    distinct(fixed.name, cown, year, conflict_id, conflict_class) |>
    filter(!is.na(year)) |>
    group_by(fixed.name, year) |>
    summarise(
      intrastate_conflicts = sum(conflict_class == "intrastate"),
      interstate_conflicts = sum(conflict_class == "interstate"),
      total_conflicts      = n(),
      .groups = "drop"
    ) |>
    left_join(fc |> distinct(fixed.name, cown), by = "fixed.name")
  
  # ── 5. Expand to full country-year grid ────────────────────────────────
  all_years <- seq(min(country_year$year, na.rm = TRUE),
                   max(country_year$year, na.rm = TRUE))
  
  base_grid <- fc |>
    distinct(fixed.name, cown) |>
    expand_grid(year = all_years)
  
  # ── 6. Join and fill zeros ─────────────────────────────────────────────
  ucdp_final <- base_grid |>
    left_join(country_year, by = c("fixed.name", "cown", "year")) |>
    mutate(across(c(intrastate_conflicts,
                    interstate_conflicts,
                    total_conflicts),
                  ~ replace_na(.x, 0L))) |>
    dplyr::select(fixed.name, cown, year,
                  intrastate_conflicts, interstate_conflicts,
                  total_conflicts)
  
  return(ucdp_final)
}

