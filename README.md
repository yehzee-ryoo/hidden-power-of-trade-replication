# Replication Materials: The Hidden Power of Trade

Replication code and data for:

> Ryoo, Yehzee, Adeline Lo, and Keith Levin. "The Hidden Power of Trade: Trade Networks as Mediators for Democracy on Human Rights Voting." *Network Science*.

- Dataverse: [TODO: DOI]
- GitHub: [TODO: URL]
- Contact: Yehzee Ryoo (yehzee.ryoo@wisc.edu)

Running `code_analyze/master-run-all.R` rebuilds every figure and table in the paper from the raw data. The one exception is Figure 2 (the DAG), which was drawn by hand and is not produced by code.

---

## 1. Folder structure

```
.
├── README.md
├── code_cleaning/              # Builds the analysis data from raw data
│   ├── sanity_check.R
│   ├── 1-DV-cleaning.R             # UNGA human-rights votes (DV), country list
│   ├── 2-IV-cleaning.R             # V-Dem, WDI, IGO, human rights index, UCDP
│   ├── 3-DV-IV-merge-polish.R      # Merge and apply time lags
│   ├── 4-trade-network-graph.R     # CEPII WTFC -> yearly trade graphs
│   ├── 5-node-trade-align.R        # Align node data to trade graphs
│   └── 6_robustness_check.R        # Resolution subsets, zero-filled graphs
├── code_analyze/               # Estimation, figures, tables
│   ├── master-run-all.R            # <- RUN THIS FILE
│   ├── 1-trade-rank-selection.R    # Latent rank (K) selection
│   ├── 2-estimate-models.R         # Network mediation models (NDE/NIE)
│   ├── 3-plots.R
│   ├── 4-tables.R
│   └── 5-rnr-resolution-types.R    # Resolution-type table, category exclusion
├── raw data/                   # Raw inputs (see Section 2)
├── data/                       # Created by the code (intermediate data)
└── output/                     # Created by the code (figures, tables, estimates)
```

The `code_cleaning/` and `code_analyze/` scripts only define functions. `master-run-all.R` loads them and runs the pipeline in order. Do not rename the folders: the scripts use relative paths such as `../raw data/`.

---

## 2. Data

Put all of these files in `raw data/` under the file names shown.

| File | Content | Source | Version / date obtained |
|---|---|---|---|
| `un_votes.rds`, `un_roll_calls.rds`, `un_roll_call_issues.rds` | UNGA roll-call votes, resolution information (including `importantvote`), and issue codes | R package `unvotes` (https://github.com/dgrtwo/unvotes), built on Voeten's UNGA Voting Data (https://doi.org/10.7910/DVN/LEJUQZ) | `unvotes` 0.3.0; saved March 12, 2026 |
| `vdem.rds` | V-Dem country-year data (`v2x_polyarchy`, `v2x_civlib`) | R package `vdemdata` (https://github.com/vdeminstitute/vdemdata) | V-Dem v15; saved March 12, 2026 |
| `gdp.pop.rds` | GDP (`NY.GDP.MKTP.CD`) and population (`SP.POP.TOTL`), 1995–2024 | World Bank WDI, via R package `WDI` | `WDI` 2.7.9; saved March 12, 2026 |
| `state_year_v4_CORRECTED.dta` | IGO membership, country-year format (WTO, EU, OIC used), 1816–2023 | Correlates of War IGO data, version 4 (corrected). Provided directly by Jon Pevehouse | Received January 9, 2026 |
| `ucdp_dyadic_v25_1.csv` | Armed conflict dyads | UCDP Dyadic Dataset v25.1 (https://ucdp.uu.se/downloads/#dyadic) | v25.1; downloaded March 12, 2026 |
| `Trade-cepii-HS96/WTFC_HS96_Y{2000..2018}_V202104.csv` (19 files, ~7.5 GB) | Bilateral trade flows, HS96 product level | CEPII World Trade Flows Characterization (WTFC) | V202104 |
| `Trade-cepii-HS96/WTFC_country_codes_V202104.csv` | CEPII country code list | CEPII WTFC | V202104 |

**Where to get the files.**

- All files except the 19 CEPII trade files are in this GitHub repository and on Dataverse.
- The CEPII trade files are too large for GitHub. Download them from the Dataverse record above and put them in `raw data/Trade-cepii-HS96/`.

[TODO: IGO redistribution — waiting for permission from Jon Pevehouse. If permission is not granted, remove the file from the package and replace this note with instructions for requesting it.]

**Changes made in code (raw files are never edited).**

- *UN votes:* Yugoslavia after 1995 is recoded as Serbia (COW code 345). Votes are coded `vote_numeric` (yes = 1, abstain = 0.5, no = 0) and `vote_binary` (yes = 1, otherwise 0).
- *IGO:* WTO observer status (3) is coded as non-member (0). The UK's EU membership is set to 0 from 2021 on. Syria's OIC membership is set to 0 for 2012–2024 because of its suspension.
- *UCDP:* Type 1/3 conflicts count as intrastate and type 2 as interstate. Type 1 (extrasystemic) has no cases after 1974, so it does not affect the 2000–2018 analysis. For type 4 conflicts, the host government counts as intrastate and foreign interveners as interstate. US vs. al-Qaeda (conflict 418) counts as interstate for the US. Conflicts are counted once per `conflict_id` per country-year.
- *CEPII:* Antigua & Barbuda's ISO3 code is corrected from `ATA` to `ATG` in the country code file.

**Data citations.**

- Voeten, Erik, Anton Strezhnev, and Michael Bailey. United Nations General Assembly Voting Data. Harvard Dataverse. https://doi.org/10.7910/DVN/LEJUQZ
- Coppedge, Michael, et al. 2025. "V-Dem Dataset v15." Varieties of Democracy (V-Dem) Project.
- World Bank. World Development Indicators.
- Pevehouse, Jon C. W., et al. Correlates of War Intergovernmental Organizations data, version 4 (corrected). Provided by Jon Pevehouse, January 9, 2026.
- UCDP Dyadic Dataset version 25.1. Uppsala Conflict Data Program.
- CEPII. World Trade Flows Characterization (WTFC) database, version 202104.

[TODO: confirm full citations and the redistribution terms for each dataset, especially CEPII WTFC and the IGO file.]

---

## 3. Software

- R 4.5.2. Results were produced on macOS.
- R packages (versions used):

| Package | Version | Package | Version |
|---|---|---|---|
| pacman | 0.5.1 | igraph | 2.2.2 |
| tidyverse | 2.0.0 | tidygraph | 1.3.1 |
| dplyr | 1.2.1 | ggraph | 2.2.2 |
| tidyr | 1.3.2 | ggplot2 | 4.0.3 |
| purrr | 1.2.2 | ggpubr | 0.6.3 |
| readr | 2.2.0 | ggrepel | 0.9.7 |
| stringr | 1.6.0 | kableExtra | 1.4.0 |
| tibble | 3.3.1 | knitr | 1.51 |
| glue | 1.8.0 | stargazer | 5.2.3 |
| countrycode | 1.7.0 | gdim | 0.1.1 |
| haven | 2.5.5 | latentnetmediate | 0.0.0.9006 |
| vroom | 1.7.0 | parallel | (base R) |

`latentnetmediate` is not on CRAN. Install the exact version used in the paper with:

```r
install.packages("remotes")
remotes::install_github("alexpghayes/latentnetmediate",
                        ref = "1c9713f117c84a12490f92f64cfe679cd99f3e43")
```

The other packages are on CRAN. `pacman::p_load()` at the top of `master-run-all.R` installs any that are missing.

---

## 4. How to replicate

1. Download this repository and put all raw data in `raw data/` (Section 2).
2. Install `latentnetmediate` (Section 3).
3. In R, set the working directory to `code_analyze/` and run the master script:

```r
setwd("path/to/repo/code_analyze")
source("master-run-all.R")
```

The script creates `data/` and `output/` if they don't exist. To start from scratch, empty both folders first. The random seed is set in the master script (`my_seed <- 50`).

**Computing requirements.**

- Rank selection runs in parallel on 8 cores (`n_cores = 8` in `master-run-all.R`) and takes about 20 minutes. Lower `n_cores` if your machine has fewer cores or runs out of memory.
- Building the trade graphs loads all 19 CEPII files (~7.5 GB) at once, so it needs a lot of memory.
- Total run time: [TODO: fill in after the full run, with machine specs (CPU, RAM)].

**Numbers reported in the text.** The counterfactual results (Moldova adopting Japan's or Belgium's trade embeddings, Syria adopting Qatar's; Saudi Arabia and Nicaragua adopting Canada's democracy score) are printed to the console and saved in `output/counterfactual_nie_scenarios.rds` and `output/counterfactual_nde_scenarios.rds`.

---

## 5. Figures and tables in the paper

All paths are relative to `output/`.

### Figures

| Paper | Description | Output file |
|---|---|---|
| Figure 1 | Trade network, 2014 | `plots/trade_network/trade_2014_top5_exp.pdf` |
| Figure 2 | DAG | Drawn by hand; not produced by code |
| Figure 3 | NDE/NIE over time, main model | `plots/main_complete/time_trend_NDE_NIE_complete.png` |
| Figure 4 | PCA of trade networks | `plots/pca/PCA_allyears_complete_lp1.pdf` |
| Figure 5 | Distribution of `v2x_polyarchy` | `plots/appendix/vdem_polyarchy_distribution.pdf` |
| Figure 6 | NDE/NIE, import network | `plots/import_complete/time_trend_NDE_NIE_import_complete.png` |
| Figure 7 | NDE/NIE, binary DV | `plots/alt_dv_complete/time_trend_NDE_NIE_alt_dv_complete.png` |
| Figure 8 | NDE/NIE, important-vote control | `plots/impvote_controls_complete/time_trend_NDE_NIE_impvote_controls_complete.png` |
| Figure 9 | NDE/NIE, politicized resolutions excluded | `plots/subset_full_eliminated/time_trend_NDE_NIE_subset_full_eliminated.png` |
| Figure 10 | Sensitivity to rank (a–e) | `plots/main_complete/sensitivity/sensitivity_{2000,2005,2010,2015,2018}.png` |
| Figure 11 | PC2 vs. export / import, 2014 | `plots/appendix/pc2_corr_export/pc2_export_log_2014.pdf`, `plots/appendix/pc2_corr_import/pc2_import_log_2014.pdf` |

### Tables

| Paper | Description | Output file |
|---|---|---|
| Table 1 | Example resolutions | `tables/resolution_examples.tex` |
| Table 2 | Summary statistics | `tables/summary_stats.tex` |
| Table 3 | Year structure | `tables/lagged_years_summary.tex` |
| Table 4 | Resolution types | `tables/resolution_types.tex` |
| Table 5 | Robustness checks | `tables/regression_robustness_compact.tex` |
| Tables 6–7 | Excluding each resolution category | `tables/regression_category_exclusion.tex` |
| Table 8 | PC2 regressed on trade, 2014 | `tables/pc2_trade_regression.tex` |

The caption of Table 5 was edited in the manuscript, so its wording differs from the caption written to the `.tex` file. The estimates are the same.

---

## 6. License

[TODO: e.g., MIT for code; CC BY 4.0 for data we created. Third-party raw data remain under their original terms.]
