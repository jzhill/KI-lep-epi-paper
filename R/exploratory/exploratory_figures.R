# Title and description --------------------------------------------

# Exploratory and superseded figure functions - not in the manuscript.
# Needs the shared themes and helpers in R/04_output_functions.R (source that first).
# Called by run_exploratory.R

# Author:           Jeremy Hill
# Date commenced:   19 Sep 2026

# Parameters ----------------------------------------

# Public health activity start years for annotated plots
ts_markers <- tribble(
  ~year, ~label,
  2018, "SDR PEP starts",
  2023, "COMBINE, Betio HHS",
  2024, "Village ACF+MDA"
)

# Figures ---------------------------------------------------

# out_plot_rates()
# Plot of annualised case notification rate per 10,000 population, 2018-2025
# Betio vs rest of South Tarawa, with 95% CI
# Uses out_tab_rates() for the numbers

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

# out_plot_rate_year()
# Plot of case notification rate per 10,000 population by year of diagnosis
# Lines for South Tarawa (all), Betio and rest of South Tarawa
# Each year's cases divided by that year's population (census_pop)
#   return_data = TRUE returns cases, population and rate per area and year

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

# out_plot_rate_mode_year()
# Plot of case notification rate per 10,000 population by year of diagnosis
# Panels by mode of detection (active / passive), lines for Betio vs rest of South Tarawa
# Denominator is the whole area's population that year, so active + passive sum to area's total rate
# Panels have separate y scales - active rates much smaller than passive
#   return_data = TRUE returns cases, population and rate per area, mode, year

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
      x = NULL,
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

# out_plot_mode_year()
# Plot of number of notifications by year of diagnosis, stacked by mode of detection
# Modes: house-to-house (COMBINE, incl. 2022 pilot), all other active, all passive
# Yearly totals labelled
#   area: NULL = all of South Tarawa, or "Betio" / "Rest of South Tarawa" (2022 pilot was in rest of South Tarawa, not Betio)
#   return_data = TRUE returns counts per year and mode group

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

# out_plot_casemix_time_2018start()
# Plot of case notifications and case mix over time, Betio vs rest of South Tarawa
# Five stacked panels: case notification rate per 10,000 population, then % male, % MB, % child (<15), % any disability (grade 1-2)
# Four % panels share one y scale (0-80) so they compare directly; rate panel has its own
# Points = annual value, horizontal lines = pooled value for the baseline and intervention periods (pooled rate = cases / person-years)
# Dotted line = 1 April 2023, the main intervention date (COMBINE house-to-house scale-up in Betio); baseline covers the full study period before it: 1/1/2018-31/3/2023
# Person-years for the bars are prorated by day for the part-year at the April 2023 cut
# Grade 2 disability alone too sparse for annual plotting - pooled counts printed in caption
#   census_pop: tibble(area, year, population)
#   return_data = TRUE returns annual and period numbers - for rate rows, n is population (person-years for periods) and prop is rate per 10,000

out_plot_casemix_time_2018start <- function(linelist, census_pop, return_data = FALSE) {
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
    "Baseline (Jan 2018-Mar 2023)", as.Date("2018-01-01"), cutoff_date - 1,
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
      # segments meet at the cutoff, with a small gap either side to show the split;
      # the outer ends get the same padding as the annual points they sit beyond
      x_start = if_else(x_start == min(period_bounds$start_date), 2018 - 0.35, cutoff_x + 0.15),
      x_end = if_else(x_end == max(period_bounds$end_date), 2025 + 0.35, cutoff_x - 0.15)
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
        "Points = annual value; horizontal lines = pooled value for the baseline and intervention periods. Dotted\n",
        "line = 1 April 2023, when COMBINE house-to-house screening started in Betio (the 2022 pilot in the rest of\n",
        "South Tarawa falls in the baseline period). % panels are % of all notifications and share one y scale. Rate\n",
        "denominator: annual population estimates (2020 census; other years estimated), prorated by day for the\n",
        "part-year at the cut. Annual values rest on small numbers.\n",
        "Grade 2 disability, baseline vs intervention: Betio ", g2_text("Betio", "Baseline (Jan 2018-Mar 2023)"),
        " vs ", g2_text("Betio", "Intervention (Apr 2023-Dec 2025)"),
        "; rest of South Tarawa ", g2_text("Rest of South Tarawa", "Baseline (Jan 2018-Mar 2023)"),
        " vs ", g2_text("Rest of South Tarawa", "Intervention (Apr 2023-Dec 2025)"), "."
      )
    ) +
    theme_lep_plot +
    theme(
      strip.text = element_text(face = "bold", size = 10, hjust = 0),
      strip.background = element_rect(fill = "grey94", colour = NA)
    )
}

# out_plot_age()
# Plot of number of cases by age group at diagnosis
# Cases with missing age excluded, counted in caption

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

# out_plot_pathway_year()
# Plot of cases by year and pathway of detection
# Panels for Betio and rest of South Tarawa, same y scale

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

# out_plot_timeseries()
# Plot of one indicator by year of diagnosis, for South Tarawa (all) and optionally further series
# Underlies the 12 mockup plots
#   indicator: n (cases), male, pb (paucibacillary), child (<15 years) - last three as % of cases that year
#   groups: all | area (Betio, rest) | area_mode (area x active/passive) | pathway (house-to-house, other active, passive)
#   markers: add vertical lines for public health activity start years
#   return_data = TRUE returns plotted numbers (n, numerator, value)

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
