# Replication Materials: The Hidden Power of Trade

Replication code and data for:

> Ryoo, Yehzee, Adeline Lo, and Keith Levin. "The Hidden Power of Trade: Trade Networks as Mediators for Democracy on Human Rights Voting." *Network Science*.

- Dataverse: https://doi.org/10.7910/DVN/K5SIZI
- GitHub: https://github.com/yehzee-ryoo/hidden-power-of-trade-replication
- Contact: Yehzee Ryoo (yehzee.ryoo@wisc.edu)

Running `code_analyze/master-run-all.R` rebuilds every figure, table, and counterfactual number reported in the paper (main text and appendix) from the raw data, and only those. The one exception is Figure 2 (the DAG), which was drawn by hand and is not produced by code. Intermediate files (cleaned data in `data/`, model estimates as `.rds` files in `output/estimates/`) are also saved, because the figures and tables are built from them.

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
│   └── 5-resolution-types.R    # Resolution-type table, category exclusion
├── raw data/                   # Raw inputs (see Section 2)
├── data/                       # Created by the code (intermediate data)
└── output/                     # Created by the code
    ├── plots/                      # Main-text figures
    │   └── appendix/               # Appendix figures
    ├── tables/                     # Main-text tables
    │   └── appendix/               # Appendix tables
    └── estimates/                  # Model estimates (.rds) the figures and tables are built from
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
| `state_year_v4_extract.dta` | IGO membership (WTO, EU, OIC), country-year format, 1998–2016 | Correlates of War IGO data, version 4 (corrected), which we received from the dataset's authors. We include only the part we use: WTO, EU, and OIC membership for 1998–2016 (with `ccode`, `state`, `year`), copied as-is from their full file | Received January 9, 2026 |
| `ucdp_dyadic_v25_1.csv` | Armed conflict dyads | UCDP Dyadic Dataset v25.1 (https://ucdp.uu.se/downloads/#dyadic) | v25.1; downloaded March 12, 2026 |
| `Trade-cepii-HS96/WTFC_HS96_Y{2000..2018}_V202104.csv` (19 files, ~7.5 GB) | Bilateral trade flows, HS96 product level | CEPII World Trade Flows Characterization (WTFC) | V202104 |
| `Trade-cepii-HS96/WTFC_country_codes_V202104.csv` | CEPII country code list | CEPII WTFC | V202104 |

**Where to get the files.**

- All files except the 19 CEPII trade files are in this GitHub repository and on Dataverse.
- The CEPII trade files are too large for GitHub. Download them from the Dataverse record above and put them in `raw data/Trade-cepii-HS96/`.
- We use an advance, limited extract graciously provided by the authors of the Correlates of War (COW) project, covering only the three IGOs used in this analysis (WTO, EU, and OIC). Users interested in these data should refer to the official and most up-to-date releases from the COW project and use the full citation provided in the table below.

**Data citations and terms of use.** If you use these data, please cite the original sources below. Each dataset is redistributed here under its own license.

| Data | Please cite | License / terms |
|---|---|---|
| UN votes | Robinson, David. 2021. *unvotes: United Nations General Assembly Voting Data*. R package version 0.3.0. https://doi.org/10.32614/CRAN.package.unvotes. Underlying data: Voeten, Erik. *United Nations General Assembly Voting Data* (Harvard Dataverse record currently titled "United Nations General Assembly Ideal Points"). https://doi.org/10.7910/DVN/LEJUQZ | Package: MIT. Underlying data: CC0 1.0 |
| V-Dem | Coppedge, Michael, et al. 2025. "V-Dem Dataset v15." Varieties of Democracy (V-Dem) Project. https://doi.org/10.23696/VDEMDS25. Accessed via Maerz, Seraphine, et al. 2025. *vdemdata* R package. https://github.com/vdeminstitute/vdemdata | CC BY-SA 4.0 |
| GDP, population | World Bank. *World Development Indicators*. Accessed via Arel-Bundock, Vincent. *WDI: World Development Indicators and Other World Bank Data*. R package version 2.7.9. https://doi.org/10.32614/CRAN.package.WDI | CC BY 4.0 |
| IGO membership | Fairley, Andrew, Jon Pevehouse, Jonas Tallberg, and Carl Vikberg. 2026. "The Correlates of War Intergovernmental Organizations Data, version 4.0." Manuscript, University of Wisconsin–Madison. | See the IGO note above |
| Armed conflict | Harbom, Lotta, Erik Melander, and Peter Wallensteen. 2008. "Dyadic Dimensions of Armed Conflict, 1946–2007." *Journal of Peace Research* 45(5). Davies, Shawn, Therése Pettersson, Margareta Sollenberg, and Magnus Öberg. 2025. "Organized Violence 1989–2024." *Journal of Peace Research* 62(4). Gleditsch, Nils Petter, et al. 2002. "Armed Conflict 1946–2001: A New Dataset." *Journal of Peace Research* 39(5): 615–637. UCDP Dyadic Dataset version 25.1 (https://ucdp.uu.se/downloads/). | CC BY 4.0 |
| Trade flows | Emlinger, Céline, and Sophie Piton. 2014. "World Trade Flows Characterization: Unit Values, Trade Types and Price Ranges." CEPII Working Paper 2014-26. Data: CEPII, World Trade Flows Characterization (WTFC), version 202104. | Etalab Open Licence 2.0 (reuse and redistribution allowed with attribution to CEPII and the date of the data version) |

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
**Numbers reported in the text.** The counterfactual results (Moldova adopting Japan's trade embeddings and Syria adopting Qatar's; Saudi Arabia adopting Canada's democracy score and Nicaragua adopting Cyprus's) are printed to the console and saved in `output/estimates/counterfactual_nie_scenarios.rds` and `output/estimates/counterfactual_nde_scenarios.rds`. They use trade year 2015, i.e., democracy measured in 2014.

---

## 5. Figures and tables in the paper

All paths are relative to `output/`.

### Figures

| Paper | Section | Description | Output file |
|---|---|---|---|
| Figure 1 | Main text | Trade network, 2014 | `plots/trade_2014_top5_exp.pdf` |
| Figure 2 | Main text | DAG | Drawn by hand; not produced by code |
| Figure 3 | Main text | NDE/NIE over time, main model | `plots/time_trend_NDE_NIE_complete.png` |
| Figure 4 | Main text | PCA of trade networks | `plots/PCA_allyears_complete_lp1.pdf` |
| Figure 5 | Appendix | Distribution of `v2x_polyarchy` | `plots/appendix/vdem_polyarchy_distribution.pdf` |
| Figure 6 | Appendix | NDE/NIE, import network | `plots/appendix/time_trend_NDE_NIE_import_complete.png` |
| Figure 7 | Appendix | NDE/NIE, binary DV | `plots/appendix/time_trend_NDE_NIE_alt_dv_complete.png` |
| Figure 8 | Appendix | NDE/NIE, important-vote control | `plots/appendix/time_trend_NDE_NIE_impvote_controls_complete.png` |
| Figure 9 | Appendix | NDE/NIE, politicized resolutions excluded | `plots/appendix/time_trend_NDE_NIE_subset_full_eliminated.png` |
| Figure 10 | Appendix | Sensitivity to rank (a–e) | `plots/appendix/sensitivity_{2000,2005,2010,2015,2018}.png` |
| Figure 11 | Appendix | PC2 vs. export / import, 2014 | `plots/appendix/pc2_export_log_2014.pdf`, `plots/appendix/pc2_import_log_2014.pdf` |

### Tables

| Paper | Section | Description | Output file |
|---|---|---|---|
| Table 1 | Main text | Example resolutions | `tables/resolution_examples.tex` |
| Table 2 | Appendix | Summary statistics | `tables/appendix/summary_stats.tex` |
| Table 3 | Appendix | Year structure | `tables/appendix/lagged_years_summary.tex` |
| Table 4 | Appendix | Resolution types | `tables/appendix/resolution_types.tex` |
| Table 5 | Appendix | Robustness checks | `tables/appendix/regression_robustness_compact.tex` |
| Tables 6–7 | Appendix | Excluding each resolution category | `tables/appendix/regression_category_exclusion.tex` |
| Table 8 | Appendix | PC2 regressed on trade, 2014 | `tables/appendix/pc2_trade_regression.tex` |

**Formatting differs from the published version.** All estimates and numbers produced by the code are identical to those in the paper. Some captions and table formatting were edited by hand for publication, so the `.tex` files written by the code may differ from the published tables in wording and layout (but not in numbers). In the figures, the positions of country labels, and the node layout of the trade-network plot (Figure 1), are not fully reproducible and may differ from the published figures; the underlying data are the same.

---

## 6. Variables

How each variable in the models is built from the raw data. Timing is relative to the trade year *t*.

| Variable in model | Role | Source | How it's coded |
|---|---|---|---|
| `vote_numeric` | Outcome | `unvotes` (resolutions tagged "Human rights") | yes = 1, abstain = 0.5, no = 0. Yugoslavia after 1995 is treated as Serbia. Measured at *t*+1. |
| `vote_binary` | Outcome (robustness) | `unvotes` | yes = 1, otherwise 0 |
| `importantvote` | Control (robustness) | `unvotes` roll-call data | As provided; available through vote year 2017 |
| `rcid` | Fixed effect | `unvotes` | Resolution identifier |
| `v2x_polyarchy` | Treatment (democracy) | V-Dem v15 | As provided; measured at *t*−1 |
| `l.gdp_s`, `l.pop_s` | Controls | World Bank WDI | Logged, then standardized within each year; *t*−2 |
| `HR_index_s` | Control | V-Dem `v2x_civlib` | Standardized within each year; *t*−2 |
| `WTO`, `EU`, `OIC` | Controls | COW IGO v4 | 1 = full member, 0 = not a member. WTO observers count as non-members. Syria is coded as out of the OIC from 2012 (suspension). *t*−2 |
| `interstate_/intrastate_conflicts_lag0/1` | Controls | UCDP Dyadic v25.1 | Count of distinct conflicts per country-year at *t*−2 (lag0) and *t*−3 (lag1). Types 1/3 count as intrastate and type 2 as interstate. In type 4, the host government counts as intrastate and foreign interveners as interstate, except US vs al-Qaeda, which counts as interstate. |
| Trade network | Mediator | CEPII WTFC | Total bilateral export value summed over HS96 products, weighted by log(1 + value), at year *t*. CEPII countries are matched to our country list through their ISO 3166-1 three-letter codes (converted to country names with the `countrycode` R package). CEPII's country file gives Antigua & Barbuda the code ATA, which is Antarctica's ISO code; we change it to Antigua's ISO code, ATG, so that its trade flows are matched rather than dropped. |

---

## 7. License

[TODO: e.g., MIT for code; CC BY 4.0 for data we created. Third-party raw data remain under their original terms.]
