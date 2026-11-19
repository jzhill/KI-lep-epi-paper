# Title and description --------------------------------------------

# Runs the output functions in 04_output_functions.R for the manuscript's tables and figures
# Files are named by manuscript item (Table 2, Fig 3, SuppTable 1...)
# Writes tables (DOCX) to outputs/paper/tables/, figures (PNG, description as TXT, data as CSV) to outputs/paper/figures/
# Not produced here: Table 1 (text table, written directly in the manuscript) and Fig 1 (map, made in a separate repo)

# Author:           Jeremy Hill
# Date commenced:   19 Sep 2026

# Packages -----------------------------------------

library(here)
library(tidyverse)
library(qs2)
library(flextable)
library(officer)

source(here("R", "04_output_functions.R"))

# Load ---------------------------------------------------

linelist_clean <- qs_read(here("data-processed", "linelist_clean.qs2"))
census_typed <- qs_read(here("data-processed", "census_typed.qs2"))

# Census -------------------------------------------------

# The census file has overlapping rows at several levels (village, council,
# island, division...). Take one level per area: council for Betio (btc) and
# the rest of South Tarawa (tuc); the South Tarawa island row for the total.
# One population per area per year, 2018-2025: the 2020 census value for 2020
# and annual intercensal estimates (data_type_total == "estimate") otherwise.

census_pop <- census_typed %>%
  filter(year >= 2018, year <= 2025, sex == "total") %>%
  filter(
    (geo == "council" & name %in% c("btc", "tuc")) |
      (geo == "island" & name == "south tarawa")
  ) %>%
  transmute(
    area = case_when(
      name == "btc" ~ "Betio",
      name == "tuc" ~ "Rest of South Tarawa",
      name == "south tarawa" ~ "South Tarawa (all)"
    ),
    year,
    population
  )

# 2020 population by age group and sex, long format. The age groups are put in
# order explicitly (the South Tarawa file's rows are alphabetical).
pop_age_typed <- qs_read(here("data-processed", "pop_age_2020_typed.qs2"))

age_levels <- c(
  "0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34",
  "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65+"
)

pop_age <- pop_age_typed %>%
  pivot_longer(c(male, female), names_to = "sex", values_to = "population") %>%
  mutate(
    sex = str_to_title(sex),
    age_group = factor(agegroup, levels = age_levels)
  ) %>%
  select(area, age_group, sex, population)

tab_dir <- here("outputs", "paper", "tables")
fig_dir <- here("outputs", "paper", "figures")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

landscape <- prop_section(page_size = page_size(orient = "landscape"))

# Tables ----------------------------------------------------

# Table 2: case mix by mode of detection, South Tarawa 2018-2025
save_as_docx(
  out_tab_casemix(linelist_clean, 2018, 2025, transpose = TRUE, p_values = TRUE, figure_rows = TRUE),
  path = file.path(tab_dir, "Table2_case_mix_by_mode_2018_2025.docx")
)

# Supp Table 1: population by age group and year
save_as_docx(
  out_tab_denominators(pop_age, census_pop),
  path = file.path(tab_dir, "SuppTable1_age_group_denominators_by_year.docx"),
  pr_section = landscape
)

# Supp Table 2: population by area and year (rate denominators)
save_as_docx(
  out_tab_population(census_pop),
  path = file.path(tab_dir, "SuppTable2_population_denominators_by_area_year.docx"),
  pr_section = landscape
)

# Supp Table 3: case mix by mode, Betio 2023-25 (house-to-house period)
save_as_docx(
  out_tab_casemix(linelist_clean, 2023, 2025, transpose = TRUE, p_values = TRUE, figure_rows = TRUE, area = "Betio"),
  path = file.path(tab_dir, "SuppTable3_case_mix_by_mode_betio_2023_2025.docx")
)

# Supp Table 4: case mix by mode, rest of South Tarawa 2018-25 (only 8 house-to-house cases, the 2022 pilot)
save_as_docx(
  out_tab_casemix(linelist_clean, 2018, 2025, transpose = TRUE, p_values = TRUE, figure_rows = TRUE, area = "Rest of South Tarawa"),
  path = file.path(tab_dir, "SuppTable4_case_mix_by_mode_rest_south_tarawa_2018_2025.docx")
)

# Supp Table 5: notifications, person-years and rate by area and period, split at 1 April 2023, study period from 1 January 2019
save_as_docx(
  out_tab_rates_period(linelist_clean, census_pop, start_date = as.Date("2019-01-01")),
  path = file.path(tab_dir, "SuppTable5_rates_by_period.docx"),
  pr_section = landscape
)

# Figures ---------------------------------------------------

# Fig 1: orientation map - made in a separate repo

# Fig 2: notifications by age group (count = bars, rate = dotted line with diamonds), South Tarawa
save_figure(
  file.path(fig_dir, "Fig2_age_group_count_and_rate.png"),
  out_plot_age_rate(linelist_clean, pop_age, census_pop),
  width = 9, height = 5.4
)

write_csv(
  out_plot_age_rate(linelist_clean, pop_age, census_pop, return_data = TRUE),
  file.path(fig_dir, "Fig2_age_group_count_and_rate_data.csv")
)

# Fig 3: notification counts by mode of detection, Betio vs rest of South Tarawa
save_figure(
  file.path(fig_dir, "Fig3_count_by_mode_betio_vs_rest.png"),
  out_plot_rate_mode_stacked(linelist_clean, census_pop, use_counts = TRUE),
  width = 10, height = 5.6
)

# Fig 4: notification rate and case mix over time, Betio vs rest of South Tarawa,
# baseline (1 Jan 2019-31 Mar 2023) v intervention (1 Apr 2023-31 Dec 2025)
save_figure(
  file.path(fig_dir, "Fig4_rate_and_casemix_over_time.png"),
  out_plot_casemix_time(linelist_clean, census_pop),
  width = 8, height = 11
)

write_csv(
  out_plot_casemix_time(linelist_clean, census_pop, return_data = TRUE),
  file.path(fig_dir, "Fig4_rate_and_casemix_over_time_data.csv")
)

message("Outputs written to outputs/paper/tables and outputs/paper/figures")
