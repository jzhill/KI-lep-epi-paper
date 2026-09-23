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

Tables are in `outputs/tables/` and figures (with their data as CSV) in `outputs/figures/`:

- `public_health_activities`: public health activities
- `case_mix_by_mode_2022_2025`, `case_mix_by_mode_2018_2025`: profile of notifications by mode of detection
- `rates_per_10000`: case notification rate by area
- `pyramid_south_tarawa`, `supp_pyramid_betio`: notifications pyramid by age group and sex (count and rate)
- `age_group_count_and_rate`: notifications by age group, sexes combined (count and rate)
- `rate_by_mode_stacked_betio_vs_rest`: stacked notification rate by mode of detection, rest of South Tarawa vs Betio
- `rate_and_casemix_over_time`: time series of notification rate and case mix

An orientation map is not produced by the pipeline. The same folders also hold supplementary outputs (`supp_` prefix) and replications of a draft manuscript's own tables and figures, numbered as that document numbers them (`draft_` prefix).

## Structure

```
R/                     pipeline scripts
data-raw/              input data (not in git)
data-processed/        .qs2 objects (not in git)
outputs/
  tables/, figures/    manuscript, supplementary and draft outputs
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
   - `table1.csv`: the text of Table 1 (see below)
3. Run the scripts in order:

```
Rscript R/01_load_data.R
Rscript R/02_data_summary.R
Rscript R/03_clean_data.R
Rscript R/05_run_outputs.R
```

### `data-raw/table1.csv`

Table 1 (public health activities) is text, not data, so it is read from a CSV instead of being written in the code. `R/01_load_data.R` reads it as text and `out_tab_interventions()` in `R/04` builds the table from it.

- UTF-8 CSV (Excel's "CSV UTF-8" is fine; a byte-order mark is handled).
- One header row, then one row per activity, in the order they should appear.
- The header cells become the table's column headings, in the same order. All columns are text. Any number of columns works; the current layout has four:

| Column | Content |
|---|---|
| `Activity` | Name of the activity |
| `Description` | What was done |
| `Years implemented` | For example `2022-present` |
| `Where implemented` | For example `Betio and Nanikai` |

- Put a value in double quotes if it contains a comma. Do not leave blank rows in the middle.

The data are unpublished and are not included in this repository.

## Licence

See `LICENSE`.
