# Title and description --------------------------------------------

# Library of output functions for the South Tarawa leprosy paper:
#   out_tab_*  tables (flextable), for DOCX
#   out_plot_* figures (ggplot2), for PNG
# All take linelist_clean (03_clean_data.R). 05_run_outputs.R calls each one.
# Analysis population: all 810 notified cases, 2018-2025, as supplied (see
# outputs/data_cleaning/<raw filename> - notes.md).

# Author:           Jeremy Hill
# Date commenced:   19 Sep 2026

# Packages -----------------------------------------

library(here)
library(tidyverse)
library(lubridate)
library(flextable)
library(officer)

# Parameters ----------------------------------------

# Colours: every categorical colour comes from the ColorBrewer "Dark2" palette
# (scale_colour_brewer / scale_fill_brewer), assigned in factor-level order.
# Neutrals use R's built-in grey names.

# Public health activity start years (manuscript Table A) for annotated plots
ts_markers <- tribble(
  ~year, ~label,
  2018, "SDR PEP starts",
  2023, "COMBINE, Betio HHS",
  2024, "Village ACF+MDA"
)

# Shared themes ---------------------------------------------

theme_lep_plot <- theme_minimal(base_size = 11) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
    axis.text = element_text(colour = "grey30"),
    axis.title = element_text(colour = "black"),
    plot.title = element_text(face = "bold", size = 12),
    plot.caption = element_text(colour = "grey30", hjust = 0),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# theme_lep_table(): grey header, white body, uniform borders, small font
theme_lep_table <- function(ft, ...) {
  ft %>%
    fontsize(size = 9, part = "all") %>%
    bg(bg = "grey85", part = "header") %>%
    bg(bg = "white", part = "body") %>%
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    valign(valign = "center", part = "all") %>%
    border_remove() %>%
    border_outer(part = "all", border = fp_border(color = "grey40", width = 0.75)) %>%
    border_inner(part = "all", border = fp_border(color = "grey70", width = 0.5)) %>%
    autofit() %>%
    fit_to_width(max_width = 9)
}

# Tables ----------------------------------------------------

# out_tab_interventions(): manuscript Table 1 - public health activities
# implemented by the NLP and partners during the study period. This is text, not
# data: reproduced verbatim from Table A of manuscript v5 (edit the wording here
# if the manuscript changes).

out_tab_interventions <- function() {
  tribble(
    ~activity, ~description, ~years, ~where,
    "Awareness raising and health promotion",
    "Community drama, puppet shows, radio interviews, distribution of printed material, public engagement at mass gatherings and etc",
    "Throughout",
    "Nationally",
    "Health worker training",
    "Routine refresher training and supervision for health workers, focused on primary care nurses and volunteers",
    "Throughout",
    "Nationally",
    "Island-based outreach",
    "Periodic NLP outreach missions to high burden outer islands, delivering training, supervision, clinical services and ad-hoc ACF, without PEP.",
    "Until 2022",
    "Nationally",
    "ACF – skin camps",
    "Periodic intensive ‘one stop shop’ screening, diagnosis and treatment services, delivered at temporary clinic facilities and accompanied by mass media health promotion",
    "2016-2019",
    "Betio",
    "ACF – school screening",
    "School-based skin screening, without PEP",
    "2016-2019",
    "South Tarawa",
    "ACF – hot spots",
    "Screening drives in hot spot areas, without systematic enumeration or PEP",
    "2016-2019",
    "South Tarawa",
    "Contact investigation",
    "Ad-hoc, best effort contact investigation without PEP",
    "Prior to 2018",
    "Nationally",
    "SDR PEP",
    "Systematic identification, screening and follow-up of contacts, with two doses of SDR one year apart",
    "2018-present; retrospective identification of contacts for index cases 2010-2017",
    "Nationally",
    "COMBINE",
    "Population-wide systematic household-based screening and rifamycin MDA, integrated with TB screening",
    "2022-present",
    "Betio and Nanikai",
    "Island-based ACF+MDA",
    "Screening and SDR MDA for entire island populations, starting with highest burden outer islands",
    "2023-present",
    "High prevalence outer islands",
    "Village-based ACF+MDA",
    "Screening and SDR MDA for entire village populations in South Tarawa, starting with highest burden villages not yet reached by COMBINE",
    "2024-present",
    "Villages not reached by COMBINE",
    "Integrated outreach",
    "Island-based outreach visits coordinated by MHMS, and including the NLP along with staff from other disease programmes and departments.",
    "2025-present",
    "Selected outer islands"
  ) %>%
    flextable() %>%
    set_header_labels(
      activity = "Activity",
      description = "Description",
      years = "Years implemented",
      where = "Where implemented"
    ) %>%
    set_caption("Table 1. Public health activities implemented by the NLP and partners during the study period.") %>%
    theme_lep_table() %>%
    align(align = "left", part = "all")
}

# out_tab_year(): Table 1 - cases by year of diagnosis, with sex, class
# (MB/PB x adult/child) and disability grade. Percentages are of the row total.

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

# out_tab_pathway(): Tables 2 and 3 - cases in one area and period by pathway
# of detection (active / passive), with sex, age, class and disability.
# Percentages are of the row total. Age summaries exclude missing ages.

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

# out_tab_casemix(): case mix of notifications by mode of detection over a
# period (default the whole study period) - house-to-house (COMBINE, incl. 2022
# pilot) vs all other active vs all passive vs all notifications, all of South
# Tarawa. Male, all adults, all children and disability are % of the row total;
# MB and PB are % of the adults (or children) in that row.

out_tab_casemix <- function(linelist, start_year = 2018, end_year = 2025) {
  row_levels <- c("House-to-house", "All other active", "All passive", "All notifications")

  linelist <- linelist %>% filter(year >= start_year, year <= end_year)

  tab <- bind_rows(
    linelist %>% filter(pathway == "House-to-house") %>% mutate(row = "House-to-house"),
    linelist %>%
      filter(mode == "Active", pathway != "House-to-house") %>%
      mutate(row = "All other active"),
    linelist %>% filter(mode == "Passive") %>% mutate(row = "All passive"),
    linelist %>% mutate(row = "All notifications")
  ) %>%
    group_by(row) %>%
    summarise(
      n = n(),
      male = sum(male),
      adult_mb = sum(age_group == "Adult" & leprosy_type == "MB"),
      adult_pb = sum(age_group == "Adult" & leprosy_type == "PB"),
      adult_all = sum(age_group == "Adult"),
      child_mb = sum(age_group == "Child" & leprosy_type == "MB"),
      child_pb = sum(age_group == "Child" & leprosy_type == "PB"),
      child_all = sum(age_group == "Child"),
      dis1 = sum(pat_disability == 1),
      dis2 = sum(pat_disability == 2),
      .groups = "drop"
    ) %>%
    # MB/PB as % of adults or children in the row; computed before the
    # other columns are turned into text
    mutate(
      adult_mb = sprintf("%d (%.1f%%)", adult_mb, 100 * adult_mb / adult_all),
      adult_pb = sprintf("%d (%.1f%%)", adult_pb, 100 * adult_pb / adult_all),
      child_mb = sprintf("%d (%.1f%%)", child_mb, 100 * child_mb / child_all),
      child_pb = sprintf("%d (%.1f%%)", child_pb, 100 * child_pb / child_all)
    ) %>%
    mutate(across(c(male, adult_all, child_all, dis1, dis2), ~ sprintf("%d (%.1f%%)", .x, 100 * .x / n))) %>%
    mutate(n = as.character(n), row = factor(row, levels = row_levels)) %>%
    arrange(row) %>%
    mutate(row = as.character(row))

  # House-to-house only ran in 2022-2025: flag it when the period starts earlier
  h2h_note <- NULL
  if (start_year < 2022) {
    tab <- tab %>% mutate(row = if_else(row == "House-to-house", "House-to-house*", row))
    h2h_note <- "*House-to-house mode of case detection was only used in 2022-2025."
  }

  tab %>%
    flextable() %>%
    set_header_labels(
      row = "Mode of detection",
      n = "Cases",
      male = "Male",
      adult_mb = "MB",
      adult_pb = "PB",
      adult_all = "All adults",
      child_mb = "MB",
      child_pb = "PB",
      child_all = "All children",
      dis1 = "Grade 1",
      dis2 = "Grade 2"
    ) %>%
    add_header_row(
      values = c("Mode of detection", "Cases", "Male", "Adult", "Child", "Disability"),
      colwidths = c(1, 1, 1, 3, 3, 2),
      top = TRUE
    ) %>%
    merge_v(part = "header") %>%
    bold(i = ~ row == "All notifications") %>%
    add_footer_lines(c(
      paste0(
        "MB = multibacillary; PB = paucibacillary. Child = under 15 years. Male, all adults, all children and disability ",
        "percentages are of the total for that row; MB and PB percentages are of the adults or children in that row. ",
        "House-to-house = COMBINE screening, including the 2022 pilot; other active = household contact, population, school ",
        "and skin camp screening; passive = self-presentation and clinical referral."
      ),
      h2h_note
    )) %>%
    set_caption(paste0(
      "Case mix of leprosy cases by mode of detection, South Tarawa, Kiribati, ",
      start_year, "-", end_year
    )) %>%
    theme_lep_table()
}

# out_tab_rates(): annualised case notification rate per 10,000 population for
# South Tarawa, Betio and the rest of South Tarawa.
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

# out_tab_denominators(): estimated population of South Tarawa by age group and
# year, 2018-2025, sexes combined - the denominators behind the age-specific
# rates in Figures 2a and 2b. Each age group's share of the 2020 population
# (pop_age) is held constant and applied to each year's total population
# estimate (census_pop). Values are rounded to whole persons for display.
#   pop_age: tibble(area, age_group [ordered factor], sex, population), 2020
#   census_pop: tibble(area, year, population), annual total population

out_tab_denominators <- function(pop_age, census_pop) {
  share <- pop_age %>%
    filter(area == "South Tarawa") %>%
    group_by(age_group) %>%
    summarise(population = sum(population), .groups = "drop") %>%
    mutate(share = population / sum(population)) %>%
    select(age_group, share)

  totals <- census_pop %>%
    filter(area == "South Tarawa (all)", year >= 2018, year <= 2025)

  tab <- share %>%
    cross_join(totals) %>%
    mutate(population = share * population) %>%
    select(age_group, year, population) %>%
    bind_rows(totals %>% mutate(age_group = "All ages") %>% select(age_group, year, population)) %>%
    mutate(age_group = factor(age_group, levels = c(levels(share$age_group), "All ages"))) %>%
    group_by(age_group) %>%
    mutate(person_years = sum(population)) %>%
    ungroup() %>%
    mutate(
      population = format(round(population), big.mark = ","),
      person_years = format(round(person_years), big.mark = ",")
    ) %>%
    arrange(age_group) %>%
    pivot_wider(names_from = year, values_from = population) %>%
    relocate(person_years, .after = last_col()) %>%
    mutate(age_group = as.character(age_group))

  tab %>%
    flextable() %>%
    set_header_labels(age_group = "Age group (years)", person_years = "Person-years 2018-2025") %>%
    bold(i = ~ age_group == "All ages") %>%
    add_footer_lines(paste0(
      "Estimated population = the age group's share of the 2020 census population (age structure held constant) x the ",
      "annual total population estimate (2020 census; other years intercensal estimates). Sexes combined. ",
      "Person-years = the sum of the annual estimates, 2018-2025 (the rate denominator in Figures 2a and 2b). ",
      "Age groups may not sum exactly to the total because of rounding."
    )) %>%
    set_caption("Estimated population by age group and year (rate denominators), South Tarawa, Kiribati, 2018-2025") %>%
    theme_lep_table()
}

# out_tab_population(): annual population estimates 2018-2025 for South Tarawa,
# Betio and the rest of South Tarawa - the denominators behind the area-level
# rates. Person-years = sum of the annual estimates.
#   census_pop: tibble(area, year, population), annual population per area

out_tab_population <- function(census_pop) {
  area_levels <- c("South Tarawa (all)", "Betio", "Rest of South Tarawa")

  census_pop %>%
    filter(year >= 2018, year <= 2025) %>%
    group_by(area) %>%
    mutate(person_years = sum(population)) %>%
    ungroup() %>%
    mutate(
      area = factor(area, levels = area_levels),
      population = format(population, big.mark = ","),
      person_years = format(person_years, big.mark = ",")
    ) %>%
    arrange(area) %>%
    pivot_wider(names_from = year, values_from = population) %>%
    relocate(person_years, .after = last_col()) %>%
    mutate(area = as.character(area)) %>%
    flextable() %>%
    set_header_labels(area = "Area", person_years = "Person-years 2018-2025") %>%
    add_footer_lines(paste0(
      "Annual population estimates: 2020 census; other years intercensal estimates. ",
      "Person-years = the sum of the annual estimates, 2018-2025 (the rate denominator)."
    )) %>%
    set_caption("Annual population estimates (rate denominators), South Tarawa, Betio and the rest of South Tarawa, 2018-2025") %>%
    theme_lep_table()
}

# Figures ---------------------------------------------------

# out_plot_rates(): Figure 3 - annualised notification rate per 10,000,
# 2018-2025, Betio vs rest of South Tarawa, with 95% CI (see out_tab_rates)

out_plot_rates <- function(linelist, census_pop) {
  rates <- out_tab_rates(linelist, census_pop, return_data = TRUE) %>%
    filter(start_year == 2018, area != "South Tarawa (all)") %>%
    mutate(area = factor(area, levels = c("Betio", "Rest of South Tarawa")))

  ggplot(rates, aes(x = area, y = rate, fill = area)) +
    geom_col(width = 0.5) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, colour = "black") +
    geom_text(aes(y = 0, label = sprintf("%.1f", rate)), vjust = -1, colour = "white", size = 5) +
    scale_fill_brewer(palette = "Dark2", guide = "none") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Annualised leprosy case notification rate, 2018-2025",
      x = NULL,
      y = "Cases per 10,000 population per year",
      caption = paste0(
        "Error bars: exact Poisson 95% CI. Assumes Poisson variation;\n",
        "intervals may be slightly narrow if cases cluster.\n",
        "Denominator: annual population estimates (2020 census; other years estimated)."
      )
    ) +
    theme_lep_plot
}

# out_plot_rate_year(): case notification rate per 10,000 population by year
# of diagnosis, for South Tarawa (all), Betio and rest of South Tarawa.
# Each year's cases are divided by that year's population (census_pop).
# return_data = TRUE returns cases, population and rate per area and year

out_plot_rate_year <- function(linelist, census_pop, return_data = FALSE) {
  cases <- bind_rows(
    linelist %>% mutate(area = "South Tarawa (all)"),
    linelist %>% mutate(area = as.character(area_group))
  ) %>%
    count(area, year, name = "cases")

  rates <- census_pop %>%
    left_join(cases, by = c("area", "year")) %>%
    mutate(
      rate = 10000 * cases / population,
      area = factor(area, levels = c("Betio", "Rest of South Tarawa", "South Tarawa (all)"))
    ) %>%
    arrange(area, year)

  if (return_data) {
    return(rates)
  }

  ggplot(rates, aes(x = year, y = rate, colour = area, linetype = area, shape = area)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2.4) +
    scale_colour_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = 2018:2025) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Leprosy case notification rate by year of diagnosis",
      x = "Year of diagnosis",
      y = "Cases per 10,000 population",
      caption = "Denominator: annual population estimates (2020 census; other years estimated)."
    ) +
    theme_lep_plot
}

# out_plot_rate_mode_year(): case notification rate per 10,000 population by
# year of diagnosis and mode of detection (active / passive), Betio vs rest of
# South Tarawa. The denominator is the whole area's population that year, so
# active + passive = the area's total rate. Panels have separate y scales
# (active rates are much smaller than passive).
# return_data = TRUE returns cases, population and rate per area, mode, year

out_plot_rate_mode_year <- function(linelist, census_pop, return_data = FALSE) {
  rates <- linelist %>%
    mutate(area = as.character(area_group)) %>%
    count(area, mode, year, name = "cases") %>%
    complete(area, mode, year = 2018:2025, fill = list(cases = 0L)) %>%
    left_join(census_pop, by = c("area", "year")) %>%
    mutate(
      rate = 10000 * cases / population,
      area = factor(area, levels = c("Betio", "Rest of South Tarawa"))
    ) %>%
    arrange(mode, area, year)

  if (return_data) {
    return(rates)
  }

  ggplot(rates, aes(x = year, y = rate, colour = area, shape = area)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 2.4) +
    facet_wrap(~mode, scales = "free_y") +
    scale_colour_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = seq(2018, 2025, 1)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Leprosy case notification rate by mode of detection",
      x = "Year of diagnosis",
      y = "Cases per 10,000 population",
      caption = paste0(
        "Active = household contact, house-to-house, population, school and skin camp screening; passive = self-presentation and clinical referral.\n",
        "Rates use the whole area's population as denominator. Annual values rest on small numbers, especially active detection.\n",
        "Denominator: annual population estimates (2020 census; other years estimated)."
      )
    ) +
    theme_lep_plot +
    theme(
      strip.text = element_text(face = "bold", hjust = 0),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# out_plot_mode_year(): number of notifications by year of diagnosis, stacked by
# mode of detection - house-to-house (COMBINE, incl. 2022 pilot), all other
# active, all passive. Yearly totals labelled.
#   area: NULL = all of South Tarawa; or "Betio" / "Rest of South Tarawa"
#   (the 2022 pilot was in the rest of South Tarawa, not Betio)
# return_data = TRUE returns the counts per year and mode group

out_plot_mode_year <- function(linelist, area = NULL, return_data = FALSE) {
  mode_levels <- c("House-to-house", "All other active", "All passive")
  area_label <- if (is.null(area)) "South Tarawa" else area

  if (!is.null(area)) {
    linelist <- linelist %>% filter(area_group == area)
  }

  counts <- linelist %>%
    mutate(
      mode_group = case_when(
        pathway == "House-to-house" ~ "House-to-house",
        mode == "Active" ~ "All other active",
        TRUE ~ "All passive"
      ),
      mode_group = factor(mode_group, levels = mode_levels)
    ) %>%
    count(year, mode_group) %>%
    complete(year = 2018:2025, mode_group, fill = list(n = 0L))

  if (return_data) {
    return(counts)
  }

  totals <- counts %>%
    group_by(year) %>%
    summarise(total = sum(n), .groups = "drop")

  ggplot(counts, aes(x = factor(year), y = n, fill = mode_group)) +
    geom_col(position = "stack", colour = "white", linewidth = 0.6, width = 0.7) +
    geom_text(
      data = totals,
      aes(x = factor(year), y = total, label = total),
      vjust = -0.5,
      colour = "black",
      size = 3.6,
      inherit.aes = FALSE
    ) +
    scale_fill_brewer(palette = "Dark2") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = paste0("Leprosy notifications by mode of detection, ", area_label, " 2018-2025"),
      x = "Year of diagnosis",
      y = "Number of leprosy cases diagnosed",
      caption = paste0(
        "House-to-house = COMBINE screening",
        if (is.null(area) || area == "Rest of South Tarawa") " (including the 2022 pilot)",
        "; other active = household contact, population, school and skin camp screening;\n",
        "passive = self-presentation and clinical referral. Numbers above bars = total notifications."
      )
    ) +
    theme_lep_plot
}

# out_plot_rate_mode_stacked(): notification rate per 10,000 population by year
# of diagnosis, stacked by mode of detection (house-to-house, all other active,
# all passive), Betio and rest of South Tarawa side by side on the SAME y axis
# so heights and composition compare directly. Each area's population that year
# is the denominator (census_pop), so the bar height is the area's total rate.
# Dotted lines mark when house-to-house began in each area (2022 pilot in the
# rest of South Tarawa; COMBINE scale-up in Betio from 2023).
# return_data = TRUE returns cases, population and rate per area, year, mode

out_plot_rate_mode_stacked <- function(linelist, census_pop, return_data = FALSE) {
  mode_levels <- c("House-to-house", "All other active", "All passive")
  area_levels <- c("Rest of South Tarawa", "Betio")

  rates <- linelist %>%
    mutate(
      area = as.character(area_group),
      mode_group = case_when(
        pathway == "House-to-house" ~ "House-to-house",
        mode == "Active" ~ "All other active",
        TRUE ~ "All passive"
      ),
      mode_group = factor(mode_group, levels = mode_levels)
    ) %>%
    count(area, year, mode_group) %>%
    complete(area, year = 2018:2025, mode_group, fill = list(n = 0L)) %>%
    left_join(census_pop, by = c("area", "year")) %>%
    mutate(
      rate = 10000 * n / population,
      area = factor(area, levels = area_levels)
    ) %>%
    rename(cases = n)

  if (return_data) {
    return(rates)
  }

  totals <- rates %>%
    group_by(area, year) %>%
    summarise(total = sum(rate), .groups = "drop")

  # x positions are on the discrete year axis: 4.5 sits between 2021 and 2022
  markers <- tibble(
    area = factor(area_levels, levels = area_levels),
    x = c(4.5, 5.5),
    label = c("2022 pilot", "House-to-house starts")
  )

  # Bars are wide relative to their slot (bars in a set sit close together);
  # a modest gap between the two panels separates the two comparison sets.
  ggplot(rates, aes(x = factor(year), y = rate, fill = mode_group)) +
    geom_col(position = "stack", colour = "white", linewidth = 0.6, width = 0.85) +
    geom_text(
      data = totals,
      aes(x = factor(year), y = total, label = sprintf("%.1f", total)),
      vjust = -0.5,
      colour = "black",
      size = 3,
      inherit.aes = FALSE
    ) +
    geom_vline(
      data = markers,
      aes(xintercept = x),
      colour = "grey55",
      linetype = "dotted",
      inherit.aes = FALSE
    ) +
    geom_text(
      data = markers,
      aes(x = x, y = Inf, label = label),
      hjust = -0.08,
      vjust = 1.6,
      size = 3,
      colour = "grey30",
      inherit.aes = FALSE
    ) +
    facet_wrap(~area) +
    scale_fill_brewer(palette = "Dark2") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Leprosy case notification rate by mode of detection, 2018-2025",
      x = "Year of diagnosis",
      y = "Cases per 10,000 population",
      caption = paste0(
        "House-to-house = COMBINE screening (including the 2022 pilot); other active = household contact, population, school and skin camp screening;\n",
        "passive = self-presentation and clinical referral. Numbers above bars = total rate. Same y axis in both panels.\n",
        "Denominator: annual population estimates (2020 census; other years estimated)."
      )
    ) +
    theme_lep_plot +
    theme(
      strip.text = element_text(face = "bold", size = 12, hjust = 0, margin = margin(6, 6, 6, 6)),
      strip.background = element_rect(fill = "grey94", colour = NA),
      panel.spacing.x = unit(4, "lines"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}



# out_plot_casemix_time(): early impact on case notifications and case mix over
# time, Betio vs rest of South Tarawa, in five stacked panels: case notification
# rate per 10,000 population (top), then % male, % PB, % child (<15) and % with
# any disability (grade 1 or 2) among all notifications. The four % panels share
# one y scale (0-80) so they compare directly; the rate panel has its own.
# Points = annual value; horizontal lines = pooled value for 2018-22 and
# 2023-25 (pooled rate = cases / person-years); dotted line = start of 2023-25
# (house-to-house scale-up in Betio). census_pop: tibble(area, year, population).
# Grade 2 disability alone is too sparse for annual plotting; its pooled
# counts are printed in the caption.
# return_data = TRUE returns annual and period numbers. For the rate rows, n is
# the population (person-years for periods) and prop is the rate per 10,000.

out_plot_casemix_time <- function(linelist, census_pop, return_data = FALSE) {
  area_levels <- c("Betio", "Rest of South Tarawa")
  prop_levels <- c("Male", "PB", "Child", "Any disability")
  indicator_levels <- c("Rate", prop_levels)
  indicator_labels <- c(
    "Rate" = "Case notification rate (per 10,000 population)",
    "Male" = "% male",
    "PB" = "% PB",
    "Child" = "% child (<15)",
    "Any disability" = "% any disability (grade 1-2)"
  )

  base <- linelist %>%
    mutate(
      area = as.character(area_group),
      period = if_else(year <= 2022, "2018-2022", "2023-2025")
    )

  annual_props <- base %>%
    group_by(area, period, year) %>%
    summarise(
      n = n(),
      Male = sum(male),
      PB = sum(leprosy_type == "PB"),
      Child = sum(age_group == "Child"),
      `Any disability` = sum(pat_disability >= 1),
      .groups = "drop"
    ) %>%
    pivot_longer(all_of(prop_levels), names_to = "indicator", values_to = "events")

  annual_rate <- base %>%
    count(area, period, year, name = "events") %>%
    left_join(census_pop, by = c("area", "year")) %>%
    transmute(area, period, year, n = population, indicator = "Rate", events)

  # % panels: 100 x events / notifications; rate panel: 10,000 x cases / population
  annual <- bind_rows(annual_props, annual_rate) %>%
    mutate(prop = if_else(indicator == "Rate", 10000, 100) * events / n)

  periods <- annual %>%
    group_by(area, indicator, period) %>%
    summarise(
      events = sum(events),
      n = sum(n),
      x_start = min(year) - 0.35,
      x_end = max(year) + 0.35,
      .groups = "drop"
    ) %>%
    mutate(prop = if_else(indicator == "Rate", 10000, 100) * events / n)

  if (return_data) {
    return(bind_rows(annual %>% mutate(kind = "annual"), periods %>% mutate(kind = "period")))
  }

  # Grade 2 disability, pooled, for the caption
  g2 <- base %>%
    group_by(area, period) %>%
    summarise(g2 = sum(pat_disability == 2), n = n(), .groups = "drop") %>%
    mutate(text = sprintf("%d/%d (%.1f%%)", g2, n, 100 * g2 / n))
  g2_text <- function(a, p) g2$text[g2$area == a & g2$period == p]

  annual <- annual %>%
    mutate(
      area = factor(area, levels = area_levels),
      indicator = factor(indicator, levels = indicator_levels, labels = indicator_labels)
    )
  periods <- periods %>%
    mutate(
      area = factor(area, levels = area_levels),
      indicator = factor(indicator, levels = indicator_levels, labels = indicator_labels)
    )

  # Invisible points that force all four % panels to the same 0-80 y range
  common_range <- tibble(
    indicator = factor(prop_levels, levels = indicator_levels, labels = indicator_labels[indicator_levels]),
    year = 2018,
    prop = 80
  )

  ggplot(annual, aes(x = year, y = prop, colour = area, shape = area)) +
    geom_vline(xintercept = 2022.5, colour = "grey55", linetype = "dotted") +
    geom_blank(data = common_range, aes(x = year, y = prop), inherit.aes = FALSE) +
    geom_segment(
      data = periods,
      aes(x = x_start, xend = x_end, y = prop, yend = prop, colour = area),
      linewidth = 1.1,
      alpha = 0.75,
      inherit.aes = FALSE
    ) +
    geom_line(linewidth = 0.4, alpha = 0.6) +
    geom_point(size = 2.2) +
    facet_wrap(~indicator, ncol = 1, scales = "free_y") +
    scale_colour_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = 2018:2025) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "Case notification rate and case mix over time, Betio vs rest of South Tarawa",
      x = "Year of diagnosis",
      y = NULL,
      caption = paste0(
        "Points = annual value; horizontal lines = pooled value for 2018-22 and 2023-25. Dotted line = start of\n",
        "2023-25 (COMBINE house-to-house scale-up in Betio; the 2022 pilot in the rest of South Tarawa falls in the\n",
        "earlier period). % panels are % of all notifications and share one y scale. Rate denominator: annual\n",
        "population estimates (2020 census; other years estimated). Annual values rest on small numbers.\n",
        "Grade 2 disability, 2018-22 vs 2023-25: Betio ", g2_text("Betio", "2018-2022"), " vs ", g2_text("Betio", "2023-2025"),
        "; rest of South Tarawa ", g2_text("Rest of South Tarawa", "2018-2022"), " vs ", g2_text("Rest of South Tarawa", "2023-2025"), "."
      )
    ) +
    theme_lep_plot +
    theme(
      strip.text = element_text(face = "bold", size = 10, hjust = 0),
      strip.background = element_rect(fill = "grey94", colour = NA)
    )
}


# out_plot_pyramid(): notifications pyramid for Betio or all of South Tarawa -
# number of notified cases 2018-2025 by age group and sex (age on the vertical
# axis, males left, females right), with the age- and sex-specific case
# notification rate overlaid as a dotted line with diamonds.
#   bars (bottom axis)                 = notified cases 2018-2025 (count at bar end)
#   dotted line + diamonds (top axis)  = case notification rate per 10,000
#     population per year. Denominator: the 2020 age-and-sex structure (each
#     band's share of the area's 2020 population, pop_age) held constant and
#     applied to each year's total population estimate (census_pop), i.e.
#     person-years for a band = its 2020 share x the area's total person-years
#     2018-2025
# The top (rate) axis is scaled so the highest rate in EITHER area reaches the
# longest bar in that figure, so the rate axes are comparable between the two
# figures; the two axes are different units (this is a dual-axis chart).
# Cases with missing age are excluded (counted in the caption).
#   pop_age: tibble(area, age_group [ordered factor], sex, population), 2020
#   census_pop: tibble(area, year, population), annual total population
# return_data = TRUE returns share, person-years, cases and rate per area, age
# group, sex

out_plot_pyramid <- function(linelist, pop_age, census_pop, area = c("Betio", "South Tarawa"), return_data = FALSE) {
  area <- match.arg(area)
  age_levels <- levels(pop_age$age_group)

  # Total person-years 2018-2025 per area, from the annual population estimates
  py_total <- census_pop %>%
    mutate(area = if_else(area == "South Tarawa (all)", "South Tarawa", area)) %>%
    filter(area %in% c("Betio", "South Tarawa"), year >= 2018, year <= 2025) %>%
    group_by(area) %>%
    summarise(py_total = sum(population), .groups = "drop")

  cases <- bind_rows(
    linelist %>% filter(area_group == "Betio") %>% mutate(area = "Betio"),
    linelist %>% mutate(area = "South Tarawa")
  ) %>%
    filter(!is.na(pat_age)) %>%
    mutate(
      age_group = cut(pat_age, breaks = c(-Inf, seq(4, 64, by = 5), Inf), labels = age_levels),
      sex = if_else(male, "Male", "Female")
    ) %>%
    count(area, age_group, sex, name = "cases")

  rates <- pop_age %>%
    group_by(area) %>%
    mutate(share = population / sum(population)) %>%
    ungroup() %>%
    left_join(py_total, by = "area") %>%
    left_join(cases, by = c("area", "age_group", "sex")) %>%
    mutate(
      cases = replace_na(cases, 0L),
      person_years = share * py_total,
      rate = 10000 * cases / person_years
    )

  if (return_data) {
    return(rates)
  }

  # Both areas share the rate range; each figure has its own case-count range
  rate_max <- max(rates$rate)
  df <- rates %>% filter(area == .env$area)
  cases_max <- max(df$cases)
  k <- cases_max / rate_max
  lim <- 1.2 * cases_max

  n_missing_age <- linelist %>%
    filter(if (area == "Betio") area_group == "Betio" else TRUE, is.na(pat_age)) %>%
    nrow()

  df <- df %>%
    mutate(
      sign = if_else(sex == "Male", -1, 1),
      cases_signed = sign * cases,
      rate_signed = sign * rate * k,
      # count labels go beyond whichever extends further, bar end or diamond,
      # with a fixed gap (in axis units) so they clear the diamond
      label_pos = sign * (pmax(cases, rate * k) + 0.05 * lim)
    )

  ggplot(df, aes(x = age_group, colour = sex, fill = sex)) +
    geom_col(aes(y = cases_signed), width = 0.85, colour = "white", linewidth = 0.4, alpha = 0.35) +
    geom_text(
      data = df %>% filter(cases > 0),
      aes(x = age_group, y = label_pos, label = cases, hjust = if_else(sex == "Male", 1, 0)),
      size = 3,
      colour = "black",
      inherit.aes = FALSE
    ) +
    geom_line(aes(y = rate_signed, group = sex), linetype = "dotted", linewidth = 0.9) +
    geom_point(aes(y = rate_signed), shape = 18, size = 3.6) +
    coord_flip() +
    scale_fill_brewer(palette = "Dark2", name = NULL) +
    scale_colour_brewer(palette = "Dark2", name = NULL) +
    scale_y_continuous(
      limits = c(-lim, lim),
      breaks = scales::breaks_extended(n = 9),
      labels = function(x) scales::comma(abs(x)),
      sec.axis = sec_axis(
        ~ . / k,
        name = "Case notification rate per 10,000 per year (dotted line)",
        breaks = seq(-60, 60, by = 20),
        labels = function(x) abs(x)
      )
    ) +
    labs(
      title = paste0("Notifications by age group and sex, ", if (area == "Betio") "Betio" else "South Tarawa", " 2018-2025"),
      x = "Age group (years)",
      y = "Number of notified cases (bars)",
      caption = paste0(
        "Bars = notified cases 2018-2025 (numbers = cases). Dotted line with diamonds = case notification rate per 10,000\n",
        "population per year (top axis). Rate denominator: 2020 age-and-sex structure applied to each year's total population\n",
        "estimate. ", n_missing_age, " case(s) with missing age excluded."
      )
    ) +
    theme_lep_plot +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3))
}

# out_plot_age_rate(): all South Tarawa notifications 2018-2025 by age group,
# sexes combined - age group on the x axis, number of notified cases as bars
# (left axis) and case notification rate per 10,000 per year as a dotted line
# with diamonds (right axis; dual axis, scaled so the highest rate reaches the
# tallest bar). Built from out_plot_pyramid(return_data = TRUE), so cases and
# the rate denominator (2020 age structure x annual total population) are
# identical to the pyramid, summed over sex.
# return_data = TRUE returns cases, person-years and rate per age group

out_plot_age_rate <- function(linelist, pop_age, census_pop, return_data = FALSE) {
  df <- out_plot_pyramid(linelist, pop_age, census_pop, return_data = TRUE) %>%
    filter(area == "South Tarawa") %>%
    group_by(area, age_group) %>%
    summarise(cases = sum(cases), person_years = sum(person_years), .groups = "drop") %>%
    mutate(rate = 10000 * cases / person_years)

  if (return_data) {
    return(df)
  }

  k <- max(df$cases) / max(df$rate)
  lim <- 1.15 * max(df$cases)

  n_missing_age <- sum(is.na(linelist$pat_age))

  ggplot(df, aes(x = age_group)) +
    geom_col(aes(y = cases), fill = "grey80", width = 0.85, colour = "white", linewidth = 0.4) +
    geom_text(
      aes(y = pmax(cases, rate * k) + 0.03 * lim, label = cases),
      vjust = 0,
      size = 3,
      colour = "black"
    ) +
    geom_line(aes(y = rate * k, group = 1), linetype = "dotted", linewidth = 0.9, colour = "black") +
    geom_point(aes(y = rate * k), shape = 18, size = 3.6, colour = "black") +
    scale_y_continuous(
      limits = c(0, lim),
      expand = expansion(mult = c(0, 0)),
      sec.axis = sec_axis(
        ~ . / k,
        name = "Case notification rate per 10,000 per year (dotted line)"
      )
    ) +
    labs(
      title = "Notifications by age group, South Tarawa 2018-2025",
      x = "Age group (years)",
      y = "Number of notified cases (bars)",
      caption = paste0(
        "Bars = notified cases 2018-2025 (numbers = cases). Dotted line with diamonds = case notification rate per 10,000\n",
        "population per year (right axis). Rate denominator: 2020 age structure applied to each year's total population\n",
        "estimate. ", n_missing_age, " case(s) with missing age excluded."
      )
    ) +
    theme_lep_plot +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# out_plot_age(): Figure 2 - number of cases by age group at diagnosis.
# Cases with missing age are excluded and counted in the caption.

out_plot_age <- function(linelist) {
  age_bands <- linelist %>%
    filter(!is.na(pat_age)) %>%
    mutate(
      age_band = cut(
        pat_age,
        breaks = c(-Inf, 4, 14, 24, 34, 44, 54, 64, Inf),
        labels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+")
      )
    ) %>%
    count(age_band, .drop = FALSE)

  ggplot(age_bands, aes(x = age_band, y = n)) +
    geom_col(fill = "grey40", width = 0.7) +
    geom_text(aes(label = n), vjust = -0.4, colour = "grey30", size = 3.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Age at diagnosis of leprosy cases, South Tarawa 2018-2025",
      x = "Age at diagnosis (years)",
      y = "Number of leprosy cases diagnosed",
      caption = paste0(
        "n = ", sum(age_bands$n), " cases with known age; ",
        sum(is.na(linelist$pat_age)), " cases with missing age excluded."
      )
    ) +
    theme_lep_plot
}

# out_plot_pathway_year(): Figure 4 - cases by year and pathway of detection,
# Betio and rest of South Tarawa side by side (same y scale).

out_plot_pathway_year <- function(linelist, start_year = 2023, end_year = 2025) {
  linelist %>%
    filter(year >= start_year, year <= end_year) %>%
    count(area_group, year, pathway) %>%
    ggplot(aes(x = factor(year), y = n, fill = pathway)) +
    geom_col(position = "stack", colour = "white", linewidth = 0.6, width = 0.7) +
    facet_wrap(~area_group) +
    scale_fill_brewer(palette = "Dark2") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = paste0("Leprosy cases by pathway of detection, ", start_year, "-", end_year),
      x = "Year of diagnosis",
      y = "Number of leprosy cases diagnosed"
    ) +
    theme_lep_plot +
    theme(strip.text = element_text(face = "bold", hjust = 0))
}

# out_plot_timeseries(): the 12 mockup plots - one indicator by year of
# diagnosis, for South Tarawa (all) and optionally further series.
#   indicator: n (cases), male, pb (paucibacillary), child (<15 years), the
#              last three as % of cases that year
#   groups:    all | area (Betio, rest) | area_mode (area x active/passive) |
#              pathway (house-to-house, other active, passive)
#   markers:   add vertical lines for public health activity start years
#   return_data = TRUE returns the plotted numbers (n, numerator, value)

out_plot_timeseries <- function(
  linelist,
  indicator = c("n", "male", "pb", "child"),
  groups = c("all", "area", "area_mode", "pathway"),
  markers = FALSE,
  return_data = FALSE
) {
  indicator <- match.arg(indicator)
  groups <- match.arg(groups)

  series_data <- bind_rows(
    linelist %>% mutate(series = "South Tarawa (all)"),
    switch(
      groups,
      all = NULL,
      area = linelist %>% mutate(series = as.character(area_group)),
      area_mode = linelist %>%
        mutate(series = paste0(area_group, ", ", str_to_lower(mode))),
      pathway = linelist %>%
        mutate(
          series = case_when(
            pathway == "House-to-house" ~ "House-to-house",
            mode == "Active" ~ "Other active",
            TRUE ~ "Passive"
          )
        )
    )
  )

  ts <- series_data %>%
    group_by(series, year) %>%
    summarise(
      n = n(),
      male = sum(male),
      pb = sum(leprosy_type == "PB"),
      child = sum(age_group == "Child"),
      .groups = "drop"
    ) %>%
    complete(
      series,
      year = 2018:2025,
      fill = list(n = 0L, male = 0L, pb = 0L, child = 0L)
    ) %>%
    mutate(
      value = if (indicator == "n") {
        n
      } else {
        if_else(n > 0, 100 * .data[[indicator]] / n, NA_real_)
      },
      series = fct_relevel(factor(series), "South Tarawa (all)")
    )

  if (return_data) {
    return(ts)
  }

  y_label <- c(
    n = "Number of leprosy cases diagnosed",
    male = "% male",
    pb = "% paucibacillary (PB)",
    child = "% child (<15 years)"
  )[[indicator]]

  p <- ggplot(ts, aes(x = year, y = value, colour = series, linetype = series, shape = series)) +
    geom_line(linewidth = 0.7, na.rm = TRUE) +
    geom_point(size = 2.4, na.rm = TRUE) +
    scale_colour_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = 2018:2025) +
    labs(
      title = paste0(y_label, " by year of diagnosis"),
      x = "Year of diagnosis",
      y = y_label,
      caption = if (groups != "all") {
        "Annual values in subgroups rest on small numbers; gaps = no cases that year."
      }
    ) +
    theme_lep_plot +
    guides(colour = guide_legend(nrow = 2))

  if (indicator == "n") {
    p <- p + scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))
  } else {
    p <- p + scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0.03, 0.02)))
  }

  if (markers) {
    p <- p +
      geom_vline(
        data = ts_markers,
        aes(xintercept = year),
        colour = "grey55",
        linetype = "dotted",
        inherit.aes = FALSE
      ) +
      geom_text(
        data = ts_markers,
        aes(x = year, y = 3, label = label),
        angle = 90,
        hjust = 0,
        vjust = -0.5,
        size = 2.8,
        colour = "grey30",
        inherit.aes = FALSE
      )
  }

  p
}
