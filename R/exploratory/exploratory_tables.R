# Title and description --------------------------------------------

# Exploratory and superseded table functions - not in the manuscript.
# Needs the shared themes and helpers in R/04_output_functions.R (source that first).
# Called by run_exploratory.R

# Author:           Jeremy Hill
# Date commenced:   19 Sep 2026

# Tables ----------------------------------------------------

# out_tab_interventions()
# A text table describing public health activities implemented by the NLP
# and partners during the study period
# Text comes from data-raw/table1.csv (layout in the README)

out_tab_interventions <- function(interventions) {
  interventions %>%
    flextable() %>%
    set_caption("Table 1. Public health activities implemented by the NLP and partners during the study period.") %>%
    theme_lep_table() %>%
    align(align = "left", part = "all")
}

# out_tab_year()
# Table of cases by year of diagnosis in rows
# Columns for sex, age, disease class and disability

out_tab_year <- function(linelist) {
  tab <- bind_rows(
    linelist %>% mutate(row = as.character(year)),
    linelist %>% mutate(row = "Total")
  ) %>%
    group_by(row) %>%
    summarise(
      n = n(),
      male = sum(male),
      mb_adult = sum(leprosy_type == "MB" & age_group == "Adult"),
      mb_child = sum(leprosy_type == "MB" & age_group == "Child"),
      pb_adult = sum(leprosy_type == "PB" & age_group == "Adult"),
      pb_child = sum(leprosy_type == "PB" & age_group == "Child"),
      dis1 = sum(pat_disability == 1),
      dis2 = sum(pat_disability == 2),
      .groups = "drop"
    ) %>%
    mutate(across(male:dis2, ~ sprintf("%d (%.1f%%)", .x, 100 * .x / n))) %>%
    mutate(n = as.character(n)) %>%
    arrange(row == "Total", row)

  tab %>%
    flextable() %>%
    set_header_labels(
      row = "Year",
      n = "Total",
      male = "Male",
      mb_adult = "MB adult",
      mb_child = "MB child",
      pb_adult = "PB adult",
      pb_child = "PB child",
      dis1 = "Disability grade 1",
      dis2 = "Disability grade 2"
    ) %>%
    bold(i = ~ row == "Total") %>%
    add_footer_lines(
      "MB = multibacillary; PB = paucibacillary. Child = under 15 years. Percentages are of the total for that row."
    ) %>%
    set_caption("Clinical characteristics of all leprosy cases diagnosed in South Tarawa, Kiribati, 2018-2025") %>%
    theme_lep_table()
}

# out_tab_pathway()
# Table of cases in one area and period
# Rows disaggregated by pathway of detection (active / passive)
# Columns for sex, age, disease class and disability

out_tab_pathway <- function(linelist, area, start_year, end_year) {
  df <- linelist %>%
    filter(area_group == area, year >= start_year, year <= end_year)

  row_levels <- c(
    "Active case finding",
    "House-to-house",
    "Household contact",
    "Population screening",
    "Skin camp",
    "School screening",
    "Total active",
    "Passive case finding",
    "Self-presentation",
    "Clinical referral",
    "Total passive",
    "Total all cases"
  )

  tab <- bind_rows(
    df %>% mutate(row = as.character(pathway)),
    df %>% mutate(row = paste("Total", str_to_lower(mode))),
    df %>% mutate(row = "Total all cases")
  ) %>%
    group_by(row) %>%
    summarise(
      n = n(),
      male = sum(male),
      age_mean_sd = sprintf("%.1f (%.1f)", mean(pat_age, na.rm = TRUE), sd(pat_age, na.rm = TRUE)),
      age_median_iqr = sprintf(
        "%.0f (%.1f-%.1f)",
        median(pat_age, na.rm = TRUE),
        unname(quantile(pat_age, 0.25, na.rm = TRUE)),
        unname(quantile(pat_age, 0.75, na.rm = TRUE))
      ),
      mb_adult = sum(leprosy_type == "MB" & age_group == "Adult"),
      mb_child = sum(leprosy_type == "MB" & age_group == "Child"),
      pb_adult = sum(leprosy_type == "PB" & age_group == "Adult"),
      pb_child = sum(leprosy_type == "PB" & age_group == "Child"),
      dis1 = sum(pat_disability == 1),
      dis2 = sum(pat_disability == 2),
      .groups = "drop"
    ) %>%
    mutate(across(c(male, mb_adult:dis2), ~ sprintf("%d (%.1f%%)", .x, 100 * .x / n))) %>%
    mutate(n = as.character(n)) %>%
    bind_rows(tibble(row = c("Active case finding", "Passive case finding"))) %>%
    mutate(row = factor(row, levels = row_levels)) %>%
    arrange(row) %>%
    mutate(across(everything(), ~ replace_na(as.character(.), "")))

  n_age_missing <- sum(is.na(df$pat_age))

  ft <- tab %>%
    flextable() %>%
    set_header_labels(
      row = "",
      n = "Cases",
      male = "Male",
      age_mean_sd = "Age, mean (SD)",
      age_median_iqr = "Age, median (IQR)",
      mb_adult = "MB adult",
      mb_child = "MB child",
      pb_adult = "PB adult",
      pb_child = "PB child",
      dis1 = "Disability grade 1",
      dis2 = "Disability grade 2"
    ) %>%
    bold(i = ~ str_detect(row, "^Total|case finding$")) %>%
    add_footer_lines(
      "MB = multibacillary; PB = paucibacillary. Child = under 15 years. Percentages are of the total for that row."
    )

  if (n_age_missing > 0) {
    ft <- ft %>%
      add_footer_lines(paste0("Age summaries exclude ", n_age_missing, " case(s) with missing age."))
  }

  ft %>%
    set_caption(paste0(
      "Clinical characteristics of leprosy cases by pathway of detection: ",
      area, ", ", start_year, "-", end_year
    )) %>%
    theme_lep_table()
}

# out_tab_rates()
# Table of annualised case notification rate per 10,000 population
# Rows disaggregated by area (South Tarawa, Betio and the rest of South Tarawa)
#   rate = cases / person-years x 10,000, with an exact Poisson 95% CI.
#   person-years = sum of the annual population over the years in the period.
#   census_pop: tibble(area, year, population) - annual population per area
#   (2020 census, other years intercensal estimates)
#   return_data = TRUE returns the numbers (used by out_plot_rates)

out_tab_rates <- function(linelist, census_pop, return_data = FALSE) {
  periods <- tribble(
    ~area, ~start_year, ~end_year,
    "South Tarawa (all)", 2018, 2025,
    "Betio", 2018, 2025,
    "Betio", 2023, 2025,
    "Rest of South Tarawa", 2018, 2025
  )

  rates <- pmap_dfr(periods, function(area, start_year, end_year) {
    df <- linelist %>% filter(year >= start_year, year <= end_year)
    if (area != "South Tarawa (all)") {
      df <- df %>% filter(area_group == area)
    }
    person_years <- census_pop %>%
      filter(.data$area == .env$area, year >= start_year, year <= end_year) %>%
      pull(population) %>%
      sum()
    cases <- nrow(df)
    ci <- poisson.test(cases, T = person_years)$conf.int * 10000
    tibble(
      area, start_year, end_year, cases, person_years,
      rate = 10000 * cases / person_years,
      ci_low = ci[1],
      ci_high = ci[2]
    )
  })

  if (return_data) {
    return(rates)
  }

  rates %>%
    transmute(
      area,
      period = paste0(start_year, "-", end_year),
      cases = as.character(cases),
      person_years = format(person_years, big.mark = ","),
      rate = sprintf("%.1f", rate)
    ) %>%
    flextable() %>%
    set_header_labels(
      area = "Area",
      period = "Period",
      cases = "Cases",
      person_years = "Person-years",
      rate = "Annualised rate per 10,000"
    ) %>%
    add_footer_lines(
      "Rate = cases / person-years x 10,000. Person-years are the sum of annual population estimates (2020 census; other years intercensal estimates)."
    ) %>%
    set_caption("Annualised leprosy case notification rate, South Tarawa, Kiribati") %>%
    theme_lep_table()
}
