# Title and description --------------------------------------------

# Data summary tables to assess the typed leprosy linelist and decide what cleaning is needed
# Outputs .docx of tables in outputs/data_checks/

# Author:           Jeremy Hill
# Date commenced:   19 Sep 2026

# Packages -----------------------------------------

library(here)
library(tidyverse)
library(lubridate)
library(qs2)
library(skimr)
library(flextable)
library(officer)

# Load ---------------------------------------------------

linelist <- qs_read(here("data-processed", "linelist_typed.qs2")) %>%
  # Row number in the source spreadsheet (row 1 = header), to find records in Excel
  mutate(excel_row = row_number() + 1L)

# Categorical columns to tabulate
cat_cols <- c(
  "pat_sex",
  "pat_class",
  "pat_source",
  "pat_village",
  "pat_village_clean",
  "pat_st_by_village",
  "pat_present_island",
  "pat_home_island"
)

checks <- list()

# Overview -----------------------------------------------

checks[["Overview"]] <- tibble(
  item = c(
    "Rows",
    "Columns",
    "Distinct patients_id",
    "Distinct pat_reg_no",
    "First diagnosis date",
    "Last diagnosis date"
  ),
  value = c(
    nrow(linelist),
    ncol(linelist) - 1L, # excludes the excel_row helper column
    n_distinct(linelist$patients_id),
    n_distinct(linelist$pat_reg_no),
    format(min(linelist$pat_date_of_diagnosis, na.rm = TRUE)),
    format(max(linelist$pat_date_of_diagnosis, na.rm = TRUE))
  ) %>%
    as.character()
)

# Completeness and value uniqueness per column -----------------------

# n_space_issues: values with leading/trailing/double spaces
# n_distinct_raw vs n_distinct_normalised: normalised = lower case, spaces trimmed. A difference means the same value is written more than one way.

checks[["Completeness by column"]] <- map_dfr(setdiff(names(linelist), "excel_row"), function(col) {
  x <- linelist[[col]]
  is_chr <- is.character(x)
  tibble(
    column = col,
    type = class(x)[1],
    n_missing = sum(is.na(x)),
    pct_missing = round(100 * sum(is.na(x)) / length(x), 1),
    n_blank_string = if (is_chr) sum(x == "", na.rm = TRUE) else NA_integer_,
    n_space_issues = if (is_chr) sum(x != str_squish(x), na.rm = TRUE) else NA_integer_,
    n_distinct_raw = n_distinct(x, na.rm = TRUE),
    n_distinct_normalised = if (is_chr) {
      n_distinct(str_to_lower(str_squish(x)), na.rm = TRUE)
    } else {
      NA_integer_
    }
  )
})

# skimr overview, as a table
checks[["skimr overview"]] <- skim(select(linelist, -excel_row)) %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# Frequency tables, raw values ------------------------------------

# value_quoted shows leading/trailing spaces that are otherwise invisible

for (col in cat_cols) {
  checks[[paste("Frequencies:", col)]] <- linelist %>%
    count(value = .data[[col]], name = "n") %>%
    mutate(
      value_quoted = if_else(is.na(value), "<NA>", paste0("'", value, "'")),
      pct = round(100 * n / sum(n), 1)
    ) %>%
    select(value_quoted, n, pct) %>%
    arrange(desc(n))
}

checks[["Frequencies: pat_disability"]] <- linelist %>%
  count(pat_disability, name = "n") %>%
  mutate(pct = round(100 * n / sum(n), 1))

# Same value written more than one way -----------------------------

checks[["Spelling variants (case/spaces only)"]] <- map_dfr(
  cat_cols,
  function(col) {
    linelist %>%
      count(value = .data[[col]], name = "n") %>%
      filter(!is.na(value)) %>%
      mutate(column = col, key = str_to_lower(str_squish(value))) %>%
      group_by(column, key) %>%
      filter(n_distinct(value) > 1) %>%
      ungroup() %>%
      mutate(value_quoted = paste0("'", value, "'")) %>%
      select(column, key, value_quoted, n) %>%
      arrange(column, key, desc(n))
  }
)

# Time --------------------------------------------------

checks[["Cases by diagnosis year"]] <- linelist %>%
  count(year = year(pat_date_of_diagnosis), name = "n")

checks[["Year by pat_class (raw)"]] <- linelist %>%
  count(year = year(pat_date_of_diagnosis), pat_class) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0)

checks[["Year by pat_source (raw)"]] <- linelist %>%
  count(year = year(pat_date_of_diagnosis), pat_source) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0)

checks[["Year by pat_village_clean (raw)"]] <- linelist %>%
  count(year = year(pat_date_of_diagnosis), pat_village_clean) %>%
  pivot_wider(names_from = year, values_from = n, values_fill = 0)

# Age and class consistency -------------------------------------

# pat_class says Child/Adult, pat_age gives the age; child = under 15
# should agree

class_age <- linelist %>%
  mutate(
    class_says = case_when(
      str_detect(str_to_lower(pat_class), "child") ~ "class: child",
      str_detect(str_to_lower(pat_class), "adult") ~ "class: adult",
      TRUE ~ "class: other/missing"
    ),
    age_says = case_when(
      is.na(pat_age) ~ "age: missing",
      pat_age < 15 ~ "age: <15",
      TRUE ~ "age: 15+"
    )
  )

checks[["pat_class vs pat_age (child = under 15)"]] <- class_age %>%
  count(class_says, age_says) %>%
  pivot_wider(names_from = age_says, values_from = n, values_fill = 0)

checks[["Age range by pat_class (raw)"]] <- linelist %>%
  group_by(pat_class) %>%
  summarise(
    n = n(),
    n_age_missing = sum(is.na(pat_age)),
    min_age = min(pat_age, na.rm = TRUE),
    max_age = max(pat_age, na.rm = TRUE),
    .groups = "drop"
  )

checks[["Age distribution"]] <- linelist %>%
  count(pat_age, name = "n") %>%
  arrange(pat_age)

# Missing data -----------------------------------------------

# Every row with any missing value, and which columns are missing
# Contains register IDs - ensure .docx is in .gitignore

checks[["Rows with any missing value"]] <- linelist %>%
  mutate(
    year = year(pat_date_of_diagnosis),
    missing_columns = pmap_chr(
      across(-excel_row),
      function(...) {
        x <- list(...)
        paste(names(x)[map_lgl(x, is.na)], collapse = ", ")
      }
    )
  ) %>%
  filter(missing_columns != "") %>%
  select(excel_row, patients_id, year, pat_class, pat_source, pat_village_clean, missing_columns)

# across() goes first so n_rows is not counted as one of the columns
checks[["Missing values by column and year"]] <- linelist %>%
  mutate(year = year(pat_date_of_diagnosis)) %>%
  group_by(year) %>%
  summarise(
    across(-c(excel_row, patients_id, pat_date_of_diagnosis), ~ sum(is.na(.))),
    n_rows = n()
  ) %>%
  relocate(n_rows, .after = year)

# Duplicates ---------------------------------------------------

checks[["Possible duplicates (same age, sex, date, village)"]] <- linelist %>%
  group_by(pat_age, pat_sex, pat_date_of_diagnosis, pat_village) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(pat_date_of_diagnosis, pat_village) %>%
  select(excel_row, patients_id, pat_reg_no, pat_age, pat_sex, pat_date_of_diagnosis, pat_village)

# Geography ----------------------------------------------------

# Which raw village maps to which raw area label
checks[["pat_village by pat_village_clean (raw pairs)"]] <- linelist %>%
  count(pat_village_clean, pat_village, name = "n") %>%
  arrange(pat_village_clean, desc(n))

# pat_village_clean values that are not a single place
checks[["pat_village_clean: ambiguous labels (contain '/' or '?')"]] <- linelist %>%
  filter(str_detect(pat_village_clean, "[/?]")) %>%
  count(pat_village_clean, pat_village, name = "n")

# Residence: present island is not South Tarawa or inconsistent
checks[["Residence: present island vs South Tarawa flag"]] <- linelist %>%
  count(pat_present_island, pat_st_by_village, name = "n") %>%
  arrange(desc(n))

# "S Trawa" is a typo for "S Tarawa" here, for listing purposes
checks[["Rows where present island is not South Tarawa"]] <- linelist %>%
  filter(!(pat_present_island %in% c("S Tarawa", "S Trawa"))) %>%
  mutate(year = year(pat_date_of_diagnosis)) %>%
  select(
    excel_row,
    patients_id,
    year,
    pat_present_island,
    pat_st_by_village,
    pat_village_clean,
    pat_village,
    pat_home_island,
    pat_source
  )

# Write DOCX ------------------------------------------------

dir.create(here("outputs", "data_checks"), showWarnings = FALSE, recursive = TRUE)

doc <- read_docx() %>%
  body_add_par("Leprosy linelist - data checks", style = "heading 1") %>%
  body_add_par(paste("Generated", format(Sys.Date())), style = "Normal")

for (nm in names(checks)) {
  ft <- checks[[nm]] %>%
    mutate(across(everything(), ~ replace_na(as.character(.), "<NA>"))) %>%
    flextable() %>%
    fontsize(size = 8, part = "all") %>%
    autofit()

  doc <- doc %>%
    body_add_par(nm, style = "heading 2") %>%
    body_add_flextable(ft) %>%
    body_add_par("", style = "Normal")
}

print(doc, target = here("outputs", "data_checks", "linelist_data_checks.docx"))
message("Saved outputs/data_checks/linelist_data_checks.docx")
