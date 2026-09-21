# Title and description --------------------------------------------

# Cleaning the typed leprosy linelist and deriving analysis columns
# Ruled documented in outputs/data_cleaning/<raw filename> - notes.md
# Input:  data-processed/linelist_typed.qs2
# Output: data-processed/linelist_clean.qs2

# Author:           Jeremy Hill
# Date commenced:   19 Sep 2026

# Packages -----------------------------------------

library(here)
library(tidyverse)
library(lubridate)
library(qs2)

# Load ---------------------------------------------------

linelist <- qs_read(here("data-processed", "linelist_typed.qs2"))

# Clean and derive ------------------------------------------

linelist_clean <- linelist %>%
  mutate(
    # Row in the source spreadsheet (row 1 = header), for tracing back to Excel
    excel_row = row_number() + 1L,
    year = year(pat_date_of_diagnosis),

    # R1: standardise spelling of disease classification ("PB child" -> "PB Child")
    pat_class = str_replace(pat_class, "child$", "Child"),

    # R2: age group from the disease classification
    age_group = case_when(
      str_detect(pat_class, "Child") ~ "Child",
      str_detect(pat_class, "Adult") ~ "Adult"
    ) %>%
      factor(levels = c("Child", "Adult")),
    leprosy_type = case_when(
      str_starts(pat_class, "MB") ~ "MB",
      str_starts(pat_class, "PB") ~ "PB"
    ) %>%
      factor(levels = c("PB", "MB")),
    male = pat_sex == "M",

    # R3: pathway and mode of detection from pat_source
    pathway = case_when(
      pat_source == "Self (Voluntary)" ~ "Self-presentation",
      pat_source %in%
        c(
          "Village Health Centre",
          "village clinic",
          "referal Fiji",
          "referral (Australia)"
        ) ~ "Clinical referral",
      pat_source == "Household Contact" ~ "Household contact",
      pat_source %in%
        c("PEARL", "Pearl Project Betio screening", "Pearl Pilot") ~ "House-to-house",
      pat_source == "Population Screening" ~ "Population screening",
      pat_source == "Skin Camp" ~ "Skin camp",
      pat_source == "School Screening" ~ "School screening"
    ) %>%
      factor(
        levels = c(
          "Self-presentation",
          "Clinical referral",
          "Household contact",
          "House-to-house",
          "Population screening",
          "Skin camp",
          "School screening"
        )
      ),
    mode = if_else(
      pathway %in% c("Self-presentation", "Clinical referral"),
      "Passive",
      "Active"
    ) %>%
      factor(levels = c("Active", "Passive")),

    # R4: Betio (including the mixed "Betio/..." labels) versus rest of South Tarawa
    area_group = if_else(
      str_starts(pat_village_clean, "Betio"),
      "Betio",
      "Rest of South Tarawa"
    ) %>%
      factor(levels = c("Betio", "Rest of South Tarawa")),

    # F1, F2: flags only, nothing excluded or changed
    flag_present_island = !(pat_present_island %in% c("S Tarawa", "S Trawa")),
    flag_reg_year = str_extract(pat_reg_no, "^[0-9]{4}") != as.character(year)
  )

# A new pat_source value in a future extract would silently become NA above
stopifnot(!anyNA(linelist_clean$pathway))

# Save ---------------------------------------------------

qs_save(linelist_clean, here("data-processed", "linelist_clean.qs2"))
message("Saved ", nrow(linelist_clean), " rows to data-processed/linelist_clean.qs2")
