# KI-lep-epi-paper

Reproducible analysis for a descriptive time-series study of leprosy notifications in South Tarawa, Kiribati, 2018-2025. It compares modes of case detection (passive, active, house-to-house screening) and Betio with the rest of South Tarawa, and produces the manuscript tables and figures.

## Pipeline

| Script | Purpose | Output |
|---|---|---|
| `R/01_load_data.R` | Read raw files as text, type each column | `data-processed/*_typed.qs2` |
| `R/02_data_summary.R` | Data-check tables to assess cleaning | `outputs/data_checks/*.docx` |
| `R/03_clean_data.R` | Cleaning rules and derived columns | `data-processed/linelist_clean.qs2` |
| `R/04_output_functions.R` | Library of `out_tab_*` and `out_plot_*` functions | (sourced by 05) |
| `R/05_run_outputs.R` | Build all tables and figures | `outputs/` |

## Outputs

`outputs/manuscript/`

- Table 1: public health activities
- Table 2: profile of notifications by mode of detection
- Figure 2: notifications pyramid by age group and sex (count and rate)
- Figure 3: stacked notification rate by mode of detection, Betio vs rest of South Tarawa
- Figure 4: time series of notification rate and case mix

Figure 1 (orientation map) is not produced by the pipeline. `outputs/tables` and `outputs/figures` hold supplementary outputs and replications of the draft manuscript's tables and figures (`draft_` prefix).

## Structure

```
R/                     pipeline scripts
data-raw/              input data (not in git)
data-processed/        .qs2 objects (not in git)
outputs/
  manuscript/          manuscript tables and figures
  tables/, figures/    supplementary and draft replications
  data_checks/         data summary tables
  data_cleaning/       cleaning rules and data queries, per raw file
context/               reference material (not in git)
renv/, renv.lock       package environment
```

## Usage

1. Install R 4.6.1 and open the project root (renv bootstraps through `.Rprofile`), then run `renv::restore()`.
2. Put the input data in `data-raw/`:
   - notification linelist: an `.xlsx` named `... rec YYMMDD.xlsx` (date received; the latest is used)
   - `annual_population_estimates.csv`
   - `2020_betio_agegp_MF.csv` and `2020_st_agegp_MF.csv`
3. Run the scripts in order:

```
Rscript R/01_load_data.R
Rscript R/02_data_summary.R
Rscript R/03_clean_data.R
Rscript R/05_run_outputs.R
```

The data are unpublished and are not included in this repository.

## Licence

See `LICENSE`.
