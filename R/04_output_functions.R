# Title and description --------------------------------------------

# Output functions for the figures and tables in the South Tarawa leprosy paper:
#   out_tab_*  tables (flextable), for DOCX
#   out_plot_* figures (ggplot2), for PNG
# All take linelist_clean (03_clean_data.R). 05_run_outputs.R calls each one and
# names the files by manuscript table/figure number.
# Analysis population: all 810 notified cases, 2018-2025
# Colours: every categorical colour comes from the ColorBrewer "Dark2" palette (scale_colour_brewer / scale_fill_brewer)
# Exploratory and superseded outputs are in R/exploratory/

# Author:           Jeremy Hill
# Date commenced:   19 Sep 2026

# Packages -----------------------------------------

library(here)
library(tidyverse)
library(lubridate)
library(flextable)
library(officer)

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

# Figure output ---------------------------------------------

# save_figure()
# Drop-in for ggsave() (300 dpi) for the out_plot_* figures
# Writes the figure's title and caption to a .txt beside the PNG, same name
# embed_text = FALSE (default): title and caption left out of the PNG so it stands alone
# embed_text = TRUE: title and caption kept in the PNG as well

save_figure <- function(filename, plot, width, height, embed_text = FALSE) {
  labs_plot <- get_labs(plot)

  # Caption line breaks are for the plot layout only
  text <- c(labs_plot$title, labs_plot$caption) %>%
    str_replace_all("\n", " ") %>%
    str_squish()
  write_lines(paste(text, collapse = "\n\n"), str_replace(filename, "\\.png$", ".txt"))

  if (!embed_text) plot <- plot + labs(title = NULL, caption = NULL)

  ggsave(filename, plot, width = width, height = height, dpi = 300)
}

# Tables ----------------------------------------------------

# casemix_p_chisq(), casemix_p_trend()
# p-values for out_tab_casemix(): x = mode group (1-3), y = characteristic
# chisq: chi-square, or Fisher's exact (dagger) if any expected count < 5
# trend: linear-by-linear (Mantel-Haenszel) test, modes scored 1-3 in order

casemix_fmt_p <- function(p) if (p < 0.001) "<0.001" else sprintf("%.3f", p)

casemix_p_chisq <- function(x, y) {
  counts <- table(x, y)
  expected <- outer(rowSums(counts), colSums(counts)) / sum(counts)
  if (any(expected < 5)) {
    paste0(casemix_fmt_p(fisher.test(counts, workspace = 2e7)$p.value), "†")
  } else {
    casemix_fmt_p(chisq.test(counts)$p.value)
  }
}

casemix_p_trend <- function(x, y) {
  r <- cor(as.numeric(x), as.numeric(y))
  casemix_fmt_p(pchisq((length(x) - 1) * r^2, df = 1, lower.tail = FALSE))
}

# casemix_h2h_v_passive()
# House-to-house (group 1) vs passive (group 3) for a logical characteristic y
# diff_ci: house-to-house minus passive, percentage points with a Newcombe-Wilson 95% CI
#   (suits small counts)
# p_fisher: Fisher's exact p-value

casemix_h2h_v_passive <- function(group, y) {
  x1 <- sum(y[group == 1])
  n1 <- sum(group == 1)
  x2 <- sum(y[group == 3])
  n2 <- sum(group == 3)
  p1 <- x1 / n1
  p2 <- x2 / n2
  ci1 <- prop.test(x1, n1, correct = FALSE)$conf.int
  ci2 <- prop.test(x2, n2, correct = FALSE)$conf.int
  d <- p1 - p2
  lower <- d - sqrt((p1 - ci1[1])^2 + (ci2[2] - p2)^2)
  upper <- d + sqrt((ci1[2] - p1)^2 + (p2 - ci2[1])^2)
  keep <- group %in% c(1, 3)

  tibble(
    diff_ci = sprintf("%+.1f (%+.1f to %+.1f)", 100 * d, 100 * lower, 100 * upper),
    p_fisher = casemix_fmt_p(fisher.test(table(group[keep], y[keep]))$p.value)
  )
}

# out_tab_casemix()
# Table of cases by mode of detection in rows
# Period defined in parameters (default the whole study period)
# area = NULL (all South Tarawa), "Betio" or "Rest of South Tarawa"
# Columns for sex, age, disease class and disability
# Male, all adults, all children and disability are % of the row total
# MB and PB are % of the adults (or children) in that row
# Age is median (IQR), excluding cases with missing age (noted in the footer if any)
# transpose = TRUE: characteristics in rows, modes in columns (portrait)
# p_values = TRUE (needs transpose): footer line for the p-value columns is the last one
#   figure_rows = FALSE: p-value columns comparing the three modes (chi-square /
#     Fisher, and trend); age not tested (no p-value shown for it)
#   figure_rows = TRUE: All notifications first; then the difference between
#     house-to-house and passive (percentage points, 95% CI) and its Fisher's
#     exact p-value; one comparison per row. Age not tested (no p-value shown for it)
# figure_rows = TRUE (needs transpose): rows are cases, male, age, MB, child (<15)
#   and any disability (grade 1-2, as in Fig 4), then grade 1
#   and grade 2; all % of the column (age excepted)

out_tab_casemix <- function(linelist, start_year = 2018, end_year = 2025,
                            transpose = FALSE, p_values = FALSE, figure_rows = FALSE, area = NULL) {
  if (p_values && !transpose) stop("p_values = TRUE needs transpose = TRUE")
  if (figure_rows && !transpose) stop("figure_rows = TRUE needs transpose = TRUE")

  row_levels <- c("House-to-house", "All other active", "All passive", "All notifications")

  linelist <- linelist %>% filter(year >= start_year, year <= end_year)
  if (!is.null(area)) linelist <- linelist %>% filter(area_group == area)

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
      age_median_iqr = sprintf(
        "%.0f (%.1f-%.1f)",
        median(pat_age, na.rm = TRUE),
        unname(quantile(pat_age, 0.25, na.rm = TRUE)),
        unname(quantile(pat_age, 0.75, na.rm = TRUE))
      ),
      mb = sum(leprosy_type == "MB"),
      any_dis = sum(pat_disability >= 1),
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
    mutate(across(c(male, mb, any_dis, adult_all, child_all, dis1, dis2), ~ sprintf("%d (%.1f%%)", .x, 100 * .x / n))) %>%
    mutate(n = as.character(n), row = factor(row, levels = row_levels)) %>%
    arrange(row) %>%
    mutate(row = as.character(row))

  # House-to-house only ran in 2022-2025: flag it when the period starts earlier
  h2h_note <- NULL
  if (start_year < 2022) {
    tab <- tab %>% mutate(row = if_else(row == "House-to-house", "House-to-house*", row))
    h2h_note <- "*House-to-house mode of case detection was only used in 2022-2025."
  }

  n_age_missing <- sum(is.na(linelist$pat_age))
  age_note <- if (n_age_missing > 0) paste0("Age summaries exclude ", n_age_missing, " case(s) with missing age.") else NULL

  p_note <- NULL

  if (!transpose) {
    ft <- tab %>%
      select(-mb, -any_dis) %>%
      flextable() %>%
      set_header_labels(
        row = "Mode of detection",
        n = "Cases",
        male = "Male",
        age_median_iqr = "Median (IQR)",
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
        values = c("Mode of detection", "Cases", "Male", "Age", "Adult", "Child", "Disability"),
        colwidths = c(1, 1, 1, 1, 3, 3, 2),
        top = TRUE
      ) %>%
      merge_v(part = "header") %>%
      bold(i = ~ row == "All notifications")
  } else {
    char_levels <- c(
      n = "Cases", male = "Male", age_median_iqr = "Age, median (IQR)",
      adult_all = "All adults", adult_mb = "Adult MB", adult_pb = "Adult PB",
      child_all = "All children", child_mb = "Child MB", child_pb = "Child PB",
      dis1 = "Disability grade 1", dis2 = "Disability grade 2"
    )
    if (figure_rows) {
      char_levels <- c(
        n = "Cases", male = "Male", age_median_iqr = "Age, median (IQR)", mb = "MB",
        child_all = "Child (<15 years)",
        any_dis = "Any disability (grade 1-2)", dis1 = "Grade 1", dis2 = "Grade 2"
      )
    }

    tab_t <- tab %>%
      pivot_longer(-row, names_to = "characteristic", values_to = "value") %>%
      pivot_wider(names_from = row, values_from = value) %>%
      filter(characteristic %in% names(char_levels)) %>%
      mutate(characteristic = factor(characteristic, levels = names(char_levels), labels = char_levels)) %>%
      arrange(characteristic) %>%
      mutate(characteristic = as.character(characteristic))

    if (figure_rows) {
      tab_t <- tab_t %>%
        relocate(`All notifications`, .after = characteristic) %>%
        rename(`Other active` = `All other active`, Passive = `All passive`)
    }

    if (p_values) {
      # Groups in order 1-3 for the trend test; All notifications not tested
      test_df <- linelist %>%
        mutate(group = case_when(
          pathway == "House-to-house" ~ 1,
          mode == "Active" ~ 2,
          mode == "Passive" ~ 3
        ))
      adults <- test_df %>% filter(age_group == "Adult")
      children <- test_df %>% filter(age_group == "Child")

      # Each p-value sits on the first row of its test; MB/PB (detailed layout)
      # and disability grade p-values span two rows (merged below)
      if (!figure_rows) {
        p_tab <- tibble(
          characteristic = c("Male", "All adults", "Adult MB", "Child MB", "Disability grade 1"),
          p_chisq = c(
            casemix_p_chisq(test_df$group, test_df$male),
            casemix_p_chisq(test_df$group, test_df$age_group),
            casemix_p_chisq(adults$group, adults$leprosy_type),
            casemix_p_chisq(children$group, children$leprosy_type),
            casemix_p_chisq(test_df$group, test_df$pat_disability)
          ),
          p_trend = c(
            casemix_p_trend(test_df$group, test_df$male),
            casemix_p_trend(test_df$group, test_df$age_group),
            casemix_p_trend(adults$group, adults$leprosy_type),
            casemix_p_trend(children$group, children$leprosy_type),
            casemix_p_trend(test_df$group, test_df$pat_disability)
          )
        )
      } else {
        # Each row is its own comparison of the characteristic against its absence; Cases row has none.
        # Age not tested (no p-value shown for it)
        p_tab <- bind_rows(
          casemix_h2h_v_passive(test_df$group, test_df$male),
          casemix_h2h_v_passive(test_df$group, test_df$leprosy_type == "MB"),
          casemix_h2h_v_passive(test_df$group, test_df$age_group == "Child"),
          casemix_h2h_v_passive(test_df$group, test_df$pat_disability >= 1),
          casemix_h2h_v_passive(test_df$group, test_df$pat_disability == 1),
          casemix_h2h_v_passive(test_df$group, test_df$pat_disability == 2)
        ) %>%
          mutate(characteristic = c(
            "Male", "MB", "Child (<15 years)", "Any disability (grade 1-2)", "Grade 1", "Grade 2"
          ), .before = 1)
      }

      tab_t <- tab_t %>%
        left_join(p_tab, by = "characteristic") %>%
        mutate(across(all_of(names(p_tab)[-1]), ~ replace_na(.x, "")))

      # Delete this line (and the p columns) if p-values are dropped
      p_note <- if (!figure_rows) {
        paste0(
          "p-values compare house-to-house, other active and passive detection (all notifications not tested). ",
          "Chi-square test, or Fisher's exact test (†) where any expected count is <5; trend = linear-by-linear ",
          "(Mantel-Haenszel) test across the modes in that order. Each p-value tests the characteristic as a whole: ",
          "sex, adult v child, MB v PB (within adults and within children) and disability grade (0, 1, 2). ",
          "No adjustment for multiple comparisons."
        )
      } else {
        paste0(
          "H2H = house-to-house. H2H-passive = house-to-house minus passive, in percentage points, with Newcombe-Wilson 95% CI. ",
          "p = Fisher's exact test, house-to-house v passive only (other active and all notifications not included); ",
          "age not tested. Each row is a separate comparison of the characteristic against its absence. ",
          "No adjustment for multiple comparisons."
        )
      }
    }

    ft <- tab_t %>%
      flextable() %>%
      set_header_labels(
        characteristic = "Characteristic",
        p_chisq = "p, chi-square / Fisher",
        p_trend = "p, trend",
        diff_ci = "H2H-passive (95% CI)",
        p_fisher = "p"
      )

    if ("All notifications" %in% names(tab_t)) ft <- bold(ft, j = "All notifications")

    if (figure_rows) ft <- padding(ft, i = 7:8, j = 1, padding.left = 12)

    if (p_values && !figure_rows) {
      for (rows in list(5:6, 8:9, 10:11)) {
        for (col in c("p_chisq", "p_trend")) ft <- merge_at(ft, i = rows, j = col)
      }
    }
  }

  ft %>%
    add_footer_lines(c(
      paste0(
        "MB = multibacillary; PB = paucibacillary. Child = under 15 years. ",
        if (!figure_rows) "Male, all adults, all children and disability percentages are of the total for that " else "Percentages are of the total for that ",
        if (transpose) "column" else "row",
        if (!figure_rows) "; MB and PB percentages are of the adults or children in that " else "",
        if (!figure_rows) (if (transpose) "column" else "row") else "",
        ". ",
        "House-to-house = COMBINE screening, including the 2022 pilot; other active = household contact, population, school ",
        "and skin camp screening; passive = self-presentation and clinical referral."
      ),
      h2h_note,
      age_note,
      p_note
    )) %>%
    set_caption(paste0(
      "Case mix of leprosy cases by mode of detection, ", if (is.null(area)) "South Tarawa" else area, ", Kiribati, ",
      start_year, "-", end_year
    )) %>%
    theme_lep_table()
}

# out_tab_rates_period()
# Lookup table of notifications, person-years and case notification rate per 10,000
# person-years, by area (South Tarawa (all), Betio, rest of South Tarawa) and period
# (before/after/all), split at the main intervention date (1 April 2023, COMBINE
# house-to-house scale-up in Betio)
# Before = start_date-31/3/2023, after = 1/4/2023-31/12/2025, all = start_date-31/12/2025
#   start_date = beginning of the study period covered (default 1/1/2018)
# Person-years: person_years_for_period(), prorated by day for the part-year at the cut
#   census_pop: tibble(area, year, population)
#   return_data = TRUE returns the numbers

out_tab_rates_period <- function(linelist, census_pop, start_date = as.Date("2018-01-01"), return_data = FALSE) {
  area_levels <- c("South Tarawa (all)", "Betio", "Rest of South Tarawa")
  cutoff_date <- as.Date("2023-04-01")
  period_bounds <- tribble(
    ~period, ~start_date, ~end_date,
    "Before", start_date, cutoff_date - 1,
    "After", cutoff_date, as.Date("2025-12-31"),
    "All", start_date, as.Date("2025-12-31")
  )

  by_area <- linelist %>% mutate(area = as.character(area_group))
  df <- bind_rows(by_area, by_area %>% mutate(area = "South Tarawa (all)"))

  rates <- pmap_dfr(period_bounds, function(period, start_date, end_date) {
    map_dfr(area_levels, function(a) {
      cases <- df %>%
        filter(area == a, pat_date_of_diagnosis >= start_date, pat_date_of_diagnosis <= end_date) %>%
        nrow()
      tibble(
        area = a,
        period = period,
        cases = cases,
        person_years = person_years_for_period(a, start_date, end_date, census_pop)
      )
    })
  }) %>%
    mutate(rate = 10000 * cases / person_years)

  if (return_data) {
    return(rates)
  }

  rates %>%
    mutate(
      area = factor(area, levels = area_levels),
      period = factor(period, levels = c("Before", "After", "All"))
    ) %>%
    arrange(area, period) %>%
    transmute(
      area, period,
      cases = as.character(cases),
      person_years = format(round(person_years), big.mark = ","),
      rate = sprintf("%.1f", rate)
    ) %>%
    flextable() %>%
    set_header_labels(
      area = "Area",
      period = "Period",
      cases = "Notifications",
      person_years = "Person-years",
      rate = "Rate per 10,000 person-years"
    ) %>%
    merge_v(j = "area") %>%
    add_footer_lines(paste0(
      "Before = ", format(start_date, "%d %B %Y"), "-31 March 2023; after = 1 April 2023-31 December 2025 ",
      "(COMBINE house-to-house screening started in Betio on 1 April 2023); all = ", format(start_date, "%d %B %Y"),
      "-31 December 2025. Person-years are the area's population summed over the period (2020 census; other ",
      "years intercensal estimates), prorated by day for the part-year at the cut."
    )) %>%
    set_caption("Notifications, person-years and case notification rate by area and period, South Tarawa, Kiribati") %>%
    theme_lep_table()
}

# out_tab_denominators()
# Table of estimated population of South Tarawa by age group and year, 2018-2025
# Denominators for age-specific rates
# Each age group's share of the 2020 population is held constant and applied to each year's total population
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

# out_tab_population()
# Table of annual population estimates 2018-2025, South Tarawa, Betio and rest of South Tarawa
# Denominators for area-level rates
# Person-years = sum of the annual estimates
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

# out_plot_pyramid()
# Plot of notifications pyramid for Betio or all of South Tarawa
# Notified cases 2018-2025 by age group and sex, age on vertical axis, males left, females right
# Age- and sex-specific case notification rate overlaid as dotted line with diamonds
# Bars (bottom axis) = notified cases; dotted line + diamonds (top axis) = rate per 10,000 population per year
# Rate denominator: 2020 age-and-sex structure (pop_age) held constant, applied to each year's total population (census_pop)
# Person-years for a band = its 2020 share x area's total person-years 2018-2025
# Top axis scaled so highest rate in either area reaches longest bar - rate axes comparable between the two figures, dual-axis chart
# Cases with missing age excluded, counted in caption
#   pop_age: tibble(area, age_group [ordered factor], sex, population), 2020
#   census_pop: tibble(area, year, population), annual total population
#   return_data = TRUE returns share, person-years, cases and rate per area, age group, sex

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

# out_plot_age_rate()
# Plot of all South Tarawa notifications 2018-2025 by age group, sexes combined
# Age group on x axis, notified cases as bars (left axis), rate per 10,000 per year as dotted line with diamonds (right axis)
# Dual axis scaled so highest rate reaches tallest bar
# Built from out_plot_pyramid(return_data = TRUE), summed over sex - cases and rate denominator identical to the pyramid
#   return_data = TRUE returns cases, person-years and rate per age group

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
      x = NULL,
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


# out_plot_rate_mode_stacked()
# Plot of notification rate per 10,000 population (or, use_counts = TRUE, raw notification
# counts) by year, stacked by mode of detection
# Modes: house-to-house, all other active, all passive
# Panels for Betio and rest of South Tarawa, same y axis so heights and composition compare directly
# Denominator is each area's population that year (census_pop), so bar height is area's total rate
# Dotted lines mark when house-to-house began in each area (2022 pilot in rest of South Tarawa, COMBINE scale-up in Betio from 2023)
#   return_data = TRUE returns cases, population and rate per area, year, mode (unaffected by use_counts)

out_plot_rate_mode_stacked <- function(linelist, census_pop, use_counts = FALSE, return_data = FALSE) {
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

  rates <- rates %>% mutate(value = if (use_counts) cases else rate)

  totals <- rates %>%
    group_by(area, year) %>%
    summarise(total = sum(value), .groups = "drop")

  # x positions are on the discrete year axis: 4.5 sits between 2021 and 2022
  markers <- tibble(
    area = factor(area_levels, levels = area_levels),
    x = c(4.5, 5.5),
    label = c("2022 pilot", "House-to-house starts")
  )

  # Bars are wide relative to their slot (bars in a set sit close together);
  # a modest gap between the two panels separates the two comparison sets.
  ggplot(rates, aes(x = factor(year), y = value, fill = mode_group)) +
    geom_col(position = "stack", colour = "white", linewidth = 0.6, width = 0.85) +
    geom_text(
      data = totals,
      aes(x = factor(year), y = total, label = if (use_counts) sprintf("%d", total) else sprintf("%.1f", total)),
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
      title = paste0("Leprosy case notification ", if (use_counts) "counts" else "rate", " by mode of detection, 2018-2025"),
      x = NULL,
      y = if (use_counts) "Number of notified cases" else "Cases per 10,000 population",
      caption = paste0(
        "House-to-house = COMBINE screening (including the 2022 pilot); other active = household contact, population, school and skin camp screening;\n",
        "passive = self-presentation and clinical referral. Numbers above bars = total ", if (use_counts) "cases" else "rate", ". Same y axis in both panels.",
        if (use_counts) "" else "\nDenominator: annual population estimates (2020 census; other years estimated)."
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



# person_years_for_period()
# Person-years for an area over an arbitrary date range, prorating each
# calendar year's population (census_pop) by the share of that year's days
# falling inside the range

person_years_for_period <- function(pop_area, start_date, end_date, census_pop) {
  years <- year(start_date):year(end_date)
  purrr::map_dbl(years, function(y) {
    year_start <- as.Date(paste0(y, "-01-01"))
    year_end <- as.Date(paste0(y, "-12-31"))
    overlap_start <- max(start_date, year_start)
    overlap_end <- min(end_date, year_end)
    fraction <- (as.numeric(overlap_end - overlap_start) + 1) / (as.numeric(year_end - year_start) + 1)
    population <- census_pop$population[census_pop$area == pop_area & census_pop$year == y]
    population * fraction
  }) %>% sum()
}

# out_plot_casemix_time()
# Plot of case notifications and case mix over time, Betio vs rest of South Tarawa
# Five stacked panels: case notification rate per 10,000 population, then % male, % MB, % child (<15), % any disability (grade 1-2)
# Four % panels share one y scale (0-80) so they compare directly; rate panel has its own
# Points = annual value; horizontal lines = pooled value for baseline (1/1/2019-31/3/2023) and intervention (1/4/2023-31/12/2025) periods
# Dotted line = 1 April 2023, COMBINE house-to-house scale-up in Betio
# Annual points and their line/rate use all years 2018-2025
# Annual points and their line/rate still use all years 2018-2025, unchanged
# Person-years for the bars are prorated by day for the part-year at the April 2023 cut
#   census_pop: tibble(area, year, population)
#   return_data = TRUE returns annual and period numbers - for rate rows, n is population (person-years for periods) and prop is rate per 10,000

out_plot_casemix_time <- function(linelist, census_pop, return_data = FALSE) {
  area_levels <- c("Betio", "Rest of South Tarawa")
  prop_levels <- c("Male", "MB", "Child", "Any disability")
  indicator_levels <- c("Rate", prop_levels)
  indicator_labels <- c(
    "Rate" = "Case notification rate (per 10,000 population)",
    "Male" = "% male",
    "MB" = "% MB",
    "Child" = "% child (<15)",
    "Any disability" = "% any disability (grade 1-2)"
  )

  cutoff_date <- as.Date("2023-04-01")
  period_bounds <- tribble(
    ~period, ~start_date, ~end_date,
    "Baseline (Jan 2019-Mar 2023)", as.Date("2019-01-01"), cutoff_date - 1,
    "Intervention (Apr 2023-Dec 2025)", cutoff_date, as.Date("2025-12-31")
  )
  # x-axis (year) position of the cutoff, for the segment ends and the dotted line
  cutoff_x <- year(cutoff_date) + (yday(cutoff_date) - 1) / (as.numeric(as.Date(paste0(year(cutoff_date), "-12-31")) - as.Date(paste0(year(cutoff_date), "-01-01"))) + 1)

  base <- linelist %>% mutate(area = as.character(area_group))

  annual_props <- base %>%
    group_by(area, year) %>%
    summarise(
      n = n(),
      Male = sum(male),
      MB = sum(leprosy_type == "MB"),
      Child = sum(age_group == "Child"),
      `Any disability` = sum(pat_disability >= 1),
      .groups = "drop"
    ) %>%
    pivot_longer(all_of(prop_levels), names_to = "indicator", values_to = "events")

  annual_rate <- base %>%
    count(area, year, name = "events") %>%
    left_join(census_pop, by = c("area", "year")) %>%
    transmute(area, year, n = population, indicator = "Rate", events)

  # % panels: 100 x events / notifications; rate panel: 10,000 x cases / population
  annual <- bind_rows(annual_props, annual_rate) %>%
    mutate(prop = if_else(indicator == "Rate", 10000, 100) * events / n)

  periods <- pmap_dfr(period_bounds, function(period, start_date, end_date) {
    df <- base %>% filter(pat_date_of_diagnosis >= start_date, pat_date_of_diagnosis <= end_date)
    map_dfr(area_levels, function(a) {
      df_a <- df %>% filter(area == a)
      counts <- tibble(
        indicator = indicator_levels,
        events = c(
          nrow(df_a),
          sum(df_a$male),
          sum(df_a$leprosy_type == "MB"),
          sum(df_a$age_group == "Child"),
          sum(df_a$pat_disability >= 1)
        ),
        n = c(person_years_for_period(a, start_date, end_date, census_pop), rep(nrow(df_a), 4))
      )
      counts %>% mutate(area = a, period = period, x_start = start_date, x_end = end_date)
    })
  }) %>%
    mutate(prop = if_else(indicator == "Rate", 10000, 100) * events / n)

  if (return_data) {
    return(bind_rows(annual %>% mutate(kind = "annual"), periods %>% mutate(kind = "period")))
  }

  # Grade 2 disability, pooled, for the caption
  g2 <- pmap_dfr(period_bounds, function(period, start_date, end_date) {
    base %>%
      filter(pat_date_of_diagnosis >= start_date, pat_date_of_diagnosis <= end_date) %>%
      group_by(area) %>%
      summarise(g2 = sum(pat_disability == 2), n = n(), .groups = "drop") %>%
      mutate(period = period)
  }) %>%
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
      indicator = factor(indicator, levels = indicator_levels, labels = indicator_labels),
      # segments span their actual period, with a small gap either side to show the
      # split at the cutoff (baseline's start and intervention's end get the same
      # padding as the annual points they sit within)
      x_start = if_else(x_start == min(period_bounds$start_date), year(min(period_bounds$start_date)) - 0.35, cutoff_x + 0.15),
      x_end = if_else(x_end == max(period_bounds$end_date), year(max(period_bounds$end_date)) + 0.35, cutoff_x - 0.15)
    )

  # Invisible points that force all four % panels to the same 0-80 y range
  common_range <- tibble(
    indicator = factor(prop_levels, levels = indicator_levels, labels = indicator_labels[indicator_levels]),
    year = 2018,
    prop = 80
  )

  ggplot(annual, aes(x = year, y = prop, colour = area, shape = area)) +
    geom_vline(xintercept = cutoff_x, colour = "grey55", linetype = "dotted") +
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
        "Points = annual value (2018-2025); horizontal lines = pooled value for the baseline (1 January 2019-31\n",
        "March 2023) and intervention (1 April 2023-31 December 2025) periods. Dotted line = 1 April 2023, when\n",
        "COMBINE house-to-house screening started in Betio. % panels are % of all notifications and share one y\n",
        "scale. Rate denominator: annual population estimates (2020 census; other years estimated), prorated by day\n",
        "for the part-year at the cut. Annual values rest on small numbers.\n",
        "Grade 2 disability, baseline vs intervention: Betio ", g2_text("Betio", "Baseline (Jan 2019-Mar 2023)"),
        " vs ", g2_text("Betio", "Intervention (Apr 2023-Dec 2025)"),
        "; rest of South Tarawa ", g2_text("Rest of South Tarawa", "Baseline (Jan 2019-Mar 2023)"),
        " vs ", g2_text("Rest of South Tarawa", "Intervention (Apr 2023-Dec 2025)"), "."
      )
    ) +
    theme_lep_plot +
    theme(
      strip.text = element_text(face = "bold", size = 10, hjust = 0),
      strip.background = element_rect(fill = "grey94", colour = NA)
    )
}


