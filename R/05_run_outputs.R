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
  out_tab_casemix(
    linelist_clean, 2018, 2025, transpose = TRUE, p_values = TRUE, figure_rows = TRUE,
    caption = paste0(
      "Table 2. Leprosy notifications and selected patient characteristics in South Tarawa from 2018-2025, ",
      "comparing different modes of case detection. Data are n (%) unless stated; percentages are of the total number of cases for the column. ",
      "House-to-house: COMBINE screening, including the 2022 pilot; other active: household contact, population, ",
      "school and skin camp screening; passive: self-presentation and clinical referral. ",
      "*House-to-house mode of case detection was only used in 2022-2025. ",
      "†Newcombe-Wilson 95% CI; p: Fisher's exact test, house-to-house v passive only. ",
      "^Age summary excludes four cases with missing age."
    )
  ),
  path = file.path(tab_dir, "Table2_case_mix_by_mode_2018_2025.docx")
)

# Supp Table 1: population by age group and year
save_as_docx(
  out_tab_denominators(
    pop_age, census_pop,
    caption = paste0(
      "Table S1. Estimated population of South Tarawa by age group and year (rate denominators), 2018-2025. ",
      "Estimated population: the age group's share of the 2020 census population (age structure held constant) ",
      "multiplied by the annual total population estimate (2020 census; other years intercensal estimates), sexes combined. ",
      "Person-years: the sum of the annual estimates, 2018-2025 (the rate denominator in Figure 2). ",
      "Age groups may not sum exactly to the total because of rounding."
    )
  ),
  path = file.path(tab_dir, "SuppTable1_age_group_denominators_by_year.docx"),
  pr_section = landscape
)

# Supp Table 2: population by area and year (rate denominators)
save_as_docx(
  out_tab_population(
    census_pop,
    caption = paste0(
      "Table S2. Annual population estimates (rate denominators), South Tarawa, Betio and the rest of South Tarawa, 2018-2025. ",
      "2020 census; other years intercensal estimates. ",
      "Person-years: the sum of the annual estimates, 2018-2025 (the rate denominator)."
    )
  ),
  path = file.path(tab_dir, "SuppTable2_population_denominators_by_area_year.docx"),
  pr_section = landscape
)

# Supp Table 3: multibacillary leprosy and any disability by sex and age group, 2018-25
save_as_docx(
  out_tab_subgroup_comparison(
    linelist_clean, 2018, 2025,
    caption = paste0(
      "Table S3. Multibacillary leprosy and any disability at diagnosis by sex and age group. ",
      "Percentages are of the total number of cases in the subgroup. ",
      "†Newcombe-Wilson 95% CI; p: Fisher's exact test. ",
      "Comparisons overlap and are not independent; no adjustment for multiple comparisons."
    )
  ),
  path = file.path(tab_dir, "SuppTable3_mb_and_disability_by_sex_age_2018_2025.docx"),
  pr_section = landscape
)

# Supp Table 4: case mix by mode, Betio 2023-25 (house-to-house period)
save_as_docx(
  out_tab_casemix(
    linelist_clean, 2023, 2025, transpose = TRUE, p_values = TRUE, figure_rows = TRUE, area = "Betio",
    caption = paste0(
      "Table S4. Leprosy notifications and selected patient characteristics in Betio from 2023-2025, ",
      "comparing different modes of case detection. Data are n (%) unless stated; percentages are of the total number of cases for the column. ",
      "House-to-house: COMBINE screening; other active: household contact, population, ",
      "school and skin camp screening; passive: self-presentation and clinical referral. ",
      "†Newcombe-Wilson 95% CI; p: Fisher's exact test, house-to-house v passive only. ",
      "^Age summary excludes one case with missing age."
    )
  ),
  path = file.path(tab_dir, "SuppTable4_case_mix_by_mode_betio_2023_2025.docx")
)

# Supp Table 5: case mix by mode, rest of South Tarawa 2018-25 (only 8 house-to-house cases, the 2022 pilot)
save_as_docx(
  out_tab_casemix(
    linelist_clean, 2018, 2025, transpose = TRUE, p_values = TRUE, figure_rows = TRUE, area = "Rest of South Tarawa",
    caption = paste0(
      "Table S5. Leprosy notifications and selected patient characteristics in the rest of South Tarawa from 2018-2025, ",
      "comparing different modes of case detection. Data are n (%) unless stated; percentages are of the total number of cases for the column. ",
      "House-to-house: COMBINE screening, including the 2022 pilot; other active: household contact, population, ",
      "school and skin camp screening; passive: self-presentation and clinical referral. ",
      "*House-to-house mode of case detection was only used in 2022-2025. ",
      "†Newcombe-Wilson 95% CI; p: Fisher's exact test, house-to-house v passive only. ",
      "^Age summary excludes one case with missing age."
    )
  ),
  path = file.path(tab_dir, "SuppTable5_case_mix_by_mode_rest_south_tarawa_2018_2025.docx")
)

# Supp Table 6: notifications, person-years and rate by area and period, split at 1 April 2023, study period from 1 January 2019
save_as_docx(
  out_tab_rates_period(
    linelist_clean, census_pop, start_date = as.Date("2019-01-01"),
    caption = paste0(
      "Table S6. Notifications, person-years and case notification rate by area and period, South Tarawa, Kiribati. ",
      "Before: 1 January 2019 to 31 March 2023; after: 1 April 2023 to 31 December 2025 ",
      "(COMBINE house-to-house screening started in Betio on 1 April 2023); all: 1 January 2019 to 31 December 2025. ",
      "Person-years: the area's population summed over the period (2020 census; other years intercensal estimates), ",
      "prorated by day."
    )
  ),
  path = file.path(tab_dir, "SuppTable6_rates_by_period.docx"),
  pr_section = landscape
)

# Figures ---------------------------------------------------

# Fig 1: orientation map - made in a separate repo

# Fig 2: notifications by age group (count = bars, rate = dotted line with diamonds), South Tarawa
save_figure(
  file.path(fig_dir, "Fig2_age_group_count_and_rate.png"),
  out_plot_age_rate(linelist_clean, pop_age, census_pop),
  width = 107, height = 75, units = "mm", eps = TRUE
)

write_csv(
  out_plot_age_rate(linelist_clean, pop_age, census_pop, return_data = TRUE),
  file.path(fig_dir, "Fig2_age_group_count_and_rate_data.csv")
)

# Fig 3: notification counts by mode of detection, Betio vs rest of South Tarawa
save_figure(
  file.path(fig_dir, "Fig3_count_by_mode_betio_vs_rest.png"),
  out_plot_rate_mode_stacked(linelist_clean, census_pop, use_counts = TRUE),
  width = 107, height = 85, units = "mm", eps = TRUE
)

# Fig 4: notification rate and case mix over time, Betio vs rest of South Tarawa,
# baseline (1 Jan 2019-31 Mar 2023) v intervention (1 Apr 2023-31 Dec 2025)
save_figure(
  file.path(fig_dir, "Fig4_rate_and_casemix_over_time.png"),
  out_plot_casemix_time(linelist_clean, census_pop),
  width = 107, height = 170, units = "mm", eps = TRUE
)

write_csv(
  out_plot_casemix_time(linelist_clean, census_pop, return_data = TRUE),
  file.path(fig_dir, "Fig4_rate_and_casemix_over_time_data.csv")
)

message("Outputs written to outputs/paper/tables and outputs/paper/figures")
