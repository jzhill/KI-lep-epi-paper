# KI-lep-epi-paper

Reproducible analysis for a descriptive time-series study of leprosy notifications in South Tarawa, Kiribati, 2018-2025. It compares modes of case detection (passive, active, house-to-house screening) and Betio with the rest of South Tarawa, and produces the manuscript tables and figures.

## Pipeline

| Script | Purpose | Output |
|---|---|---|
| `R/01_load_data.R` | Read raw files as text, type each column | `data-processed/*_typed.qs2` |
| `R/02_data_summary.R` | Data-check tables to assess cleaning | `outputs/data_checks/*.docx` |
| `R/03_clean_data.R` | Cleaning rules and derived columns | `data-processed/linelist_clean.qs2` |
| `R/04_output_functions.R` | The `out_tab_*` and `out_plot_*` functions for the manuscript's tables and figures, plus shared themes and helpers | (sourced by 05) |
| `R/05_run_outputs.R` | Build the manuscript tables and figures, named by manuscript item | `outputs/paper/` |
| `R/exploratory/` | Exploratory and superseded output functions and their runner (`run_exploratory.R`); not needed for the manuscript | `outputs/exploratory/` |

## Outputs

`05_run_outputs.R` writes tables to `outputs/paper/tables/` and figures (with a `.txt` of the title and caption, and for some the plotted data as CSV) to `outputs/paper/figures/`, reproducing the submitted manuscript (v6):

| Manuscript item | Output file | Function |
|---|---|---|
| Table 1 | not produced by the pipeline (text table, written directly in the manuscript) | |
| Table 2 | `Table2_case_mix_by_mode_2018_2025.docx` | `out_tab_casemix()` |
| Fig 1 | not produced here (map, made in a separate repo) | |
| Fig 2 | `Fig2_age_group_count_and_rate.png` | `out_plot_age_rate()` |
| Fig 3 | `Fig3_count_by_mode_betio_vs_rest.png` | `out_plot_rate_mode_stacked(use_counts = TRUE)` |
| Fig 4 | `Fig4_rate_and_casemix_over_time.png` | `out_plot_casemix_time()` |
| Supp Table 1 | `SuppTable1_age_group_denominators_by_year.docx` | `out_tab_denominators()` |
| Supp Table 2 | `SuppTable2_population_denominators_by_area_year.docx` | `out_tab_population()` |
| Supp Table 3 | `SuppTable3_case_mix_by_mode_betio_2023_2025.docx` | `out_tab_casemix(area = "Betio")` |
| Supp Table 4 | `SuppTable4_case_mix_by_mode_rest_south_tarawa_2018_2025.docx` | `out_tab_casemix(area = "Rest of South Tarawa")` |
| Supp Table 5 | `SuppTable5_rates_by_period.docx` | `out_tab_rates_period()` |

`R/exploratory/run_exploratory.R` (optional) writes every other table and figure built during the analysis to `outputs/exploratory/`.

## Structure

```
R/                     pipeline scripts (manuscript outputs)
R/exploratory/         exploratory and superseded output functions and runner
data-raw/              input data (not in git)
data-processed/        .qs2 objects (not in git)
outputs/
  paper/               manuscript tables/ and figures/, named by manuscript item
  exploratory/         exploratory tables/ and figures/
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
   - `table1.csv`: the text of Table 1 (read by `01`; used only by the exploratory outputs, see below)
3. Run the scripts in order:

```
Rscript R/01_load_data.R
Rscript R/02_data_summary.R
Rscript R/03_clean_data.R
Rscript R/05_run_outputs.R
Rscript R/exploratory/run_exploratory.R   # optional
```

### `data-raw/table1.csv`

Table 1 (public health activities) is text, not data. The manuscript's Table 1 is written directly in the manuscript, not produced by the pipeline. The CSV is used only by the exploratory outputs: `R/01_load_data.R` reads it as text (so the file must be present) and `out_tab_interventions()` in `R/exploratory/` builds a formatted table from it.

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
