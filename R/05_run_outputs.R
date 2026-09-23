# Title and description --------------------------------------------

# Runs every output function in 04_output_functions.R on the cleaned linelist
# and writes tables (DOCX) and figures (PNG, with their data as CSV) to
# outputs/tables/ and outputs/figures/. Tables first, then figures, each in a
# logical order; "draft_" files replicate a draft manuscript's own tables and
# figures (their numbering is that document's, not ours); "supp_" files are
# supplementary. Rates per 10,000 use annual population estimates (see Census
# below).

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
interventions_typed <- qs_read(here("data-processed", "interventions_typed.qs2"))

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

dir.create(here("outputs", "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs", "figures"), showWarnings = FALSE, recursive = TRUE)

landscape <- prop_section(page_size = page_size(orient = "landscape"))

# Tables ----------------------------------------------------

# Public health activities (text)
save_as_docx(
  out_tab_interventions(interventions_typed),
  path = here("outputs", "tables", "public_health_activities.docx"),
  pr_section = landscape
)

# Case mix by mode of detection
save_as_docx(
  out_tab_casemix(linelist_clean, 2022, 2025),
  path = here("outputs", "tables", "case_mix_by_mode_2022_2025.docx"),
  pr_section = landscape
)

save_as_docx(
  out_tab_casemix(linelist_clean, 2018, 2025),
  path = here("outputs", "tables", "case_mix_by_mode_2018_2025.docx"),
  pr_section = landscape
)

# Case notification rates
save_as_docx(
  out_tab_rates(linelist_clean, census_pop),
  path = here("outputs", "tables", "rates_per_10000.docx"),
  pr_section = landscape
)

# Population denominators
save_as_docx(
  out_tab_denominators(pop_age, census_pop),
  path = here("outputs", "tables", "supp_age_group_denominators_by_year.docx"),
  pr_section = landscape
)

save_as_docx(
  out_tab_population(census_pop),
  path = here("outputs", "tables", "supp_population_denominators_by_area_year.docx"),
  pr_section = landscape
)

# Draft manuscript replications
save_as_docx(
  out_tab_year(linelist_clean),
  path = here("outputs", "tables", "draft_table1_cases_by_year.docx"),
  pr_section = landscape
)

save_as_docx(
  out_tab_pathway(linelist_clean, "Betio", 2023, 2025),
  path = here("outputs", "tables", "draft_table2_betio_2023_2025_by_pathway.docx"),
  pr_section = landscape
)

save_as_docx(
  out_tab_pathway(linelist_clean, "Rest of South Tarawa", 2018, 2025),
  path = here("outputs", "tables", "draft_table3_rest_south_tarawa_2018_2025_by_pathway.docx"),
  pr_section = landscape
)

# Figures ---------------------------------------------------

# Orientation map of South Tarawa - NOT BUILT YET. Needs boundary data (e.g.
# the SPC popGIS file data-raw/gis/KIR_EA_Census2020FINAL.geojson) and the sf
# package.

# Notifications pyramid by age group and sex (count = bars, rate = dotted line
# with diamonds), South Tarawa and Betio. Both areas' numbers are in one CSV.
ggsave(
  here("outputs", "figures", "pyramid_south_tarawa.png"),
  out_plot_pyramid(linelist_clean, pop_age, census_pop, "South Tarawa"),
  width = 8, height = 6.5, dpi = 300
)

ggsave(
  here("outputs", "figures", "supp_pyramid_betio.png"),
  out_plot_pyramid(linelist_clean, pop_age, census_pop, "Betio"),
  width = 8, height = 6.5, dpi = 300
)

write_csv(
  out_plot_pyramid(linelist_clean, pop_age, census_pop, return_data = TRUE),
  here("outputs", "figures", "pyramid_data.csv")
)

# As above but by age group only (sexes combined), age group on the x axis,
# count = bars, rate = dotted line with diamonds
ggsave(
  here("outputs", "figures", "age_group_count_and_rate.png"),
  out_plot_age_rate(linelist_clean, pop_age, census_pop),
  width = 9, height = 5.4, dpi = 300
)

write_csv(
  out_plot_age_rate(linelist_clean, pop_age, census_pop, return_data = TRUE),
  here("outputs", "figures", "age_group_count_and_rate_data.csv")
)

# Notification rate by mode of detection, stacked, Betio vs rest of South
# Tarawa (additionality of house-to-house screening)
ggsave(
  here("outputs", "figures", "rate_by_mode_stacked_betio_vs_rest.png"),
  out_plot_rate_mode_stacked(linelist_clean, census_pop),
  width = 10, height = 5.6, dpi = 300
)

write_csv(
  out_plot_rate_mode_stacked(linelist_clean, census_pop, return_data = TRUE),
  here("outputs", "figures", "rate_by_mode_stacked_data.csv")
)

# Notification rate and case mix over time (% male, PB, child, any
# disability), Betio vs rest of South Tarawa
ggsave(
  here("outputs", "figures", "rate_and_casemix_over_time.png"),
  out_plot_casemix_time(linelist_clean, census_pop),
  width = 8, height = 11, dpi = 300
)

write_csv(
  out_plot_casemix_time(linelist_clean, census_pop, return_data = TRUE),
  here("outputs", "figures", "rate_and_casemix_over_time_data.csv")
)

# Draft manuscript replications
ggsave(
  here("outputs", "figures", "draft_fig2_age_distribution.png"),
  out_plot_age(linelist_clean),
  width = 8, height = 4.8, dpi = 300
)

ggsave(
  here("outputs", "figures", "draft_fig3_rate_betio_vs_rest.png"),
  out_plot_rates(linelist_clean, census_pop),
  width = 6, height = 4.8, dpi = 300
)

ggsave(
  here("outputs", "figures", "draft_fig4_pathway_by_year_2023_2025.png"),
  out_plot_pathway_year(linelist_clean, 2023, 2025),
  width = 8, height = 4.8, dpi = 300
)

# Supplementary figures

ggsave(
  here("outputs", "figures", "notifications_by_mode_by_year_stacked.png"),
  out_plot_mode_year(linelist_clean),
  width = 9, height = 5.4, dpi = 300
)

write_csv(
  out_plot_mode_year(linelist_clean, return_data = TRUE),
  here("outputs", "figures", "notifications_by_mode_by_year_data.csv")
)

ggsave(
  here("outputs", "figures", "notifications_by_mode_by_year_stacked_betio.png"),
  out_plot_mode_year(linelist_clean, area = "Betio"),
  width = 9, height = 5.4, dpi = 300
)

write_csv(
  out_plot_mode_year(linelist_clean, area = "Betio", return_data = TRUE),
  here("outputs", "figures", "notifications_by_mode_by_year_betio_data.csv")
)

ggsave(
  here("outputs", "figures", "notifications_by_mode_by_year_stacked_rest_south_tarawa.png"),
  out_plot_mode_year(linelist_clean, area = "Rest of South Tarawa"),
  width = 9, height = 5.4, dpi = 300
)

write_csv(
  out_plot_mode_year(linelist_clean, area = "Rest of South Tarawa", return_data = TRUE),
  here("outputs", "figures", "notifications_by_mode_by_year_rest_south_tarawa_data.csv")
)

ggsave(
  here("outputs", "figures", "rate_by_year_south_tarawa_betio_rest.png"),
  out_plot_rate_year(linelist_clean, census_pop),
  width = 8, height = 4.8, dpi = 300
)

write_csv(
  out_plot_rate_year(linelist_clean, census_pop, return_data = TRUE),
  here("outputs", "figures", "rate_by_year_data.csv")
)

ggsave(
  here("outputs", "figures", "rate_by_year_and_mode_betio_vs_rest.png"),
  out_plot_rate_mode_year(linelist_clean, census_pop),
  width = 9, height = 5.4, dpi = 300
)

write_csv(
  out_plot_rate_mode_year(linelist_clean, census_pop, return_data = TRUE),
  here("outputs", "figures", "rate_by_year_and_mode_data.csv")
)

# The 12 mockup plots, as real annual series
ts_plots <- tribble(
  ~file, ~indicator, ~groups, ~markers,
  "01_notifications_south_tarawa_all", "n", "all", FALSE,
  "02_prop_male_south_tarawa_all", "male", "all", FALSE,
  "03_notifications_betio_vs_rest", "n", "area", FALSE,
  "04_prop_male_betio_vs_rest", "male", "area", FALSE,
  "04b_prop_male_betio_vs_rest_annotated", "male", "area", TRUE,
  "05_prop_pb_betio_vs_rest", "pb", "area", FALSE,
  "06_prop_child_betio_vs_rest", "child", "area", FALSE,
  "07_prop_male_by_area_and_mode", "male", "area_mode", FALSE,
  "08_prop_pb_by_area_and_mode", "pb", "area_mode", FALSE,
  "09_prop_child_by_area_and_mode", "child", "area_mode", FALSE,
  "10_prop_male_by_pathway", "male", "pathway", FALSE,
  "11_prop_pb_by_pathway", "pb", "pathway", FALSE,
  "12_prop_child_by_pathway", "child", "pathway", FALSE
)

pwalk(ts_plots, function(file, indicator, groups, markers) {
  ggsave(
    here("outputs", "figures", paste0(file, ".png")),
    out_plot_timeseries(linelist_clean, indicator, groups, markers),
    width = 8, height = 4.8, dpi = 300
  )
})

# Numbers behind every time series plot (table view of the figures)
pmap_dfr(ts_plots, function(file, indicator, groups, markers) {
  out_plot_timeseries(linelist_clean, indicator, groups, markers, return_data = TRUE) %>%
    mutate(plot = file, indicator = indicator, .before = 1)
}) %>%
  write_csv(here("outputs", "figures", "timeseries_plot_data.csv"))

message("Outputs written to outputs/tables and outputs/figures")
