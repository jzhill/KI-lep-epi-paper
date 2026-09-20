# Title and description --------------------------------------------

# Loading the South Tarawa leprosy notification linelist (and the census
# population file) from data-raw, reading every column as character, then
# typing each column explicitly.
# No recoding or cleaning of values happens here - see 02_data_summary.R
# to assess, and 03_clean_data.R to act on decisions.
# Source: NLP notification register (ACCESS), extract supplied by PLF data officers

# Author:           Jeremy Hill
# Date commenced:   19 Sep 2026

# Packages -----------------------------------------

library(here)
library(tidyverse)
library(lubridate)
library(readxl)
library(qs2)

# Load latest raw file ----------------------------------

# Raw filenames end in "rec yymmdd" = date the extract was received
# e.g. "data for Jeremy rec 260918.xlsx". Latest = most recently received.
# "^[^~]" skips Excel's temporary lock files.

raw_files <- dir(
  here("data-raw"),
  pattern = "^[^~].*rec [0-9]{6}\\.xlsx$",
  full.names = TRUE
)

raw_dates <- basename(raw_files) %>%
  str_extract("(?<=rec )[0-9]{6}") %>%
  ymd()

latest_file <- raw_files[which.max(raw_dates)]
message("Loading: ", latest_file)

# col_types = "text": every column is read as character, nothing is guessed.
# .name_repair = "minimal": keep the blank header of column J as it is.
linelist_raw <- read_excel(
  latest_file,
  col_types = "text",
  .name_repair = "minimal"
)

# Name and type each column ---------------------------------------

# Column J has a blank header in the source. Its contents look like a
# cleaned area/village grouping of pat_village, so it is named pat_area here.
# Column K's header is a free-text note, renamed to a valid column name.
# Both renamed by position because one of them has no name to match on.

linelist_typed <- linelist_raw %>%
  rename(
    pat_area = 10,
    pat_sth_tarawa_by_village = 11
  ) %>%
  mutate(
    patients_id = as.character(patients_id),
    pat_reg_no = as.character(pat_reg_no),
    pat_age = as.integer(pat_age),
    pat_sex = as.character(pat_sex),
    pat_class = as.character(pat_class),
    pat_disability = as.integer(pat_disability),
    # Excel stores dates as days since 1899-12-30
    pat_date_of_diagnosis = as.Date(
      as.numeric(pat_date_of_diagnosis),
      origin = "1899-12-30"
    ),
    pat_source = as.character(pat_source),
    pat_village = as.character(pat_village),
    pat_area = as.character(pat_area),
    pat_sth_tarawa_by_village = as.character(pat_sth_tarawa_by_village),
    pat_present_island = as.character(pat_present_island),
    pat_home_island = as.character(pat_home_island)
  )

# Census population estimates -------------------------------------

# Long file: each row is a population for one place, year and sex. Rows are NOT
# mutually exclusive - the same people appear at several levels of aggregation
# (geo = village / council / island / division / oi_st / national), so pick
# one level per question when using it (see 05_run_outputs.R).
# data_type_total: "census" (1990, 1995, ... 2020) or annual "estimate".
# No datestamp in the filename, so this one is read by name.

census_typed <- read_csv(
  here("data-raw", "annual_population_estimates.csv"),
  col_types = cols(.default = col_character())
) %>%
  mutate(
    year = as.integer(year),
    population = as.integer(population)
  )

# 2020 population by age group and sex ------------------------------

# One file each for Betio and all of South Tarawa (5-year age groups, 65+ open).
# Rows are not in age order in the South Tarawa file - the order is set when used.

pop_age_typed <- bind_rows(
  read_csv(
    here("data-raw", "2020_betio_agegp_MF.csv"),
    col_types = cols(.default = col_character())
  ) %>%
    mutate(area = "Betio"),
  read_csv(
    here("data-raw", "2020_st_agegp_MF.csv"),
    col_types = cols(.default = col_character())
  ) %>%
    mutate(area = "South Tarawa")
) %>%
  mutate(
    male = as.integer(male),
    female = as.integer(female),
    population_2020 = as.integer(population_2020)
  )

# Save ---------------------------------------------------

qs_save(linelist_typed, here("data-processed", "linelist_typed.qs2"))
message("Saved ", nrow(linelist_typed), " rows to data-processed/linelist_typed.qs2")

qs_save(census_typed, here("data-processed", "census_typed.qs2"))
message("Saved ", nrow(census_typed), " rows to data-processed/census_typed.qs2")

qs_save(pop_age_typed, here("data-processed", "pop_age_2020_typed.qs2"))
message("Saved ", nrow(pop_age_typed), " rows to data-processed/pop_age_2020_typed.qs2")
