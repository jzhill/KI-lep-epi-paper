# AGENTS.md

R pipeline for a descriptive time-series analysis of leprosy notifications in South Tarawa, Kiribati (2018-2025). Produces the tables and figures for a journal manuscript.

## Commands

Run from the project root (renv activates via `.Rprofile`), in order:

```
Rscript R/01_load_data.R
Rscript R/02_data_summary.R
Rscript R/03_clean_data.R
Rscript R/05_run_outputs.R
Rscript R/exploratory/run_exploratory.R   # optional
```

- Packages: `renv::restore()`; after adding one, `renv::snapshot()`; verify with `renv::status()` (must be consistent).
- `R/04_output_functions.R` is sourced by `05`, not run directly.

## Structure

- `R/`: numbered pipeline scripts. `04` holds only the `out_tab_*` / `out_plot_*` functions used in the manuscript, plus shared themes and helpers; `05` only calls them. `04` and `05` reproduce the submitted v6 manuscript tables and figures: change them only deliberately.
- `R/exploratory/`: exploratory and superseded output functions and their runner (`run_exploratory.R`). Not needed for the manuscript.
- `data-raw/`: inputs (linelist `.xlsx` named `... rec YYMMDD.xlsx`, census and age-sex population `.csv`, `table1.csv` = the text of Table 1, used only by exploratory outputs, layout in the README). Not in git.
- `data-processed/`: `.qs2` objects. Not in git.
- `outputs/paper/tables`, `outputs/paper/figures`: the manuscript's tables and figures, named by manuscript item (`Table2_`, `Fig3_`, `SuppTable1_`...). Figures have a `.txt` of the title and caption (left out of the PNG unless `embed_text = TRUE`) and, for some, a data CSV. Table 1 is written in the manuscript and Fig 1 comes from a separate repo, so neither is produced here. `outputs/exploratory/`: everything from the exploratory runner.
- `outputs/data_cleaning/<raw filename> - notes.md`: every cleaning rule and data query, by raw file.
- `context/`: reference material only. Not in git.

## Conventions

- tidyverse with the `%>%` pipe (never `|>`); `here()` for paths; explicit code over clever code; minimal defensive code.
- Read raw data as text and type each column explicitly; never let a reader guess types.
- Cleaning rules live only in `03_clean_data.R`. Do not add or change one without asking, and record it in the notes file.
- Output logic goes in functions: in `04` if the output is in the manuscript, otherwise in `R/exploratory/`. Give figures `return_data = TRUE` so the plotted numbers can be exported.
- Match existing style. No numbered subheadings in code. Change only what the task needs; no unrequested functions, refactors or options.
- Joins must be idempotent. Analyses are descriptive: no hypothesis tests beyond those already in the manuscript tables (Fisher's exact, house-to-house v passive); ask before adding more.

## Data

- Data are unpublished and identifiable: never commit them, and never print row-level data or IDs (use Excel row numbers).
- The census file has overlapping rows at several geographic levels: pick one level per question.
- After regenerating a figure, check its file modification time. If an output will not overwrite, ask the user to close it in their editor.
