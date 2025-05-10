#----------------------------------------------------------------------
# Purpose: preparation of the main variables for further analysis
#----------------------------------------------------------------------

library(tidyverse)
library(here)

data <- read_csv(here::here("data/data_lab2.csv"))

# Process main variables - blood pressure measurements
# Aggregate 4 blood pressure measurements to get a single variable
mean_of_2closest <- function(vec, allowed_diff) {
  # Returns the mean of 2 closest elements that differ at most by allowed_diff
  sorted_vec <- sort(vec)
  if (length(sorted_vec) == 0) {
    return(NA);
  } else if (length(sorted_vec) == 1) {
    return(sorted_vec[1])
  }
  
  diffs <- diff(sorted_vec)
  min_diff_index <- which.min(diffs)
  if (diffs[min_diff_index] > allowed_diff) {
    return(NA)
  } else {
    closest_elements <- sorted_vec[min_diff_index:(min_diff_index + 1)]
    return(mean(closest_elements))
  }
}
data <- data |>
  mutate(
    diastolic1 = if_else(diastolic1 == 0, NA, diastolic1),
    diastolic2 = if_else(diastolic2 == 0, NA, diastolic2),
    diastolic3 = if_else(diastolic3 == 0, NA, diastolic3),
    diastolic4 = if_else(diastolic4 == 0, NA, diastolic4),
  )
data <- data %>%
  mutate(
    diastolic = apply(
      select(., diastolic1, diastolic2, diastolic3, diastolic4), 
      1, 
      partial(mean_of_2closest, allowed_diff = 8)
    ),
    systolic = apply(
      select(., systolic1, systolic2, systolic3, systolic4), 
      1, 
      partial(mean_of_2closest, allowed_diff = 8)
    ),
    .before = 2
  ) |>
  select(
    -c(diastolic1, diastolic2, diastolic3, diastolic4, systolic1, systolic2, systolic3, systolic4)
  ) |>
  filter(!is.na(systolic) & !is.na(diastolic))

# Add new variable: blood pressure category
data <- data |>
  mutate(
    blood_pressure_category = case_when(
      diastolic >= 90 | systolic >= 140 ~ "Hypertension Stage 2",
      diastolic >= 80 | systolic >= 130 ~ "Hypertension Stage 1",
      systolic >= 120 ~ "Elevated",
      .default = "Normal"
    ) |>
      factor(
        levels = c("Normal", "Elevated", "Hypertension Stage 1", "Hypertension Stage 2"),
        ordered = TRUE
      ),
    .before = 4
  )

# Process other variables
data <- data |>
  mutate(
    race = if_else(race == "Other Race - Including Multi-Racial", "Other Race", race)
  )

# 1.
whole_data <- data |>
  select(
    systolic, diastolic, blood_pressure_category,
    age, gender, race
  )

# 2. Select data related to fat distribution
# fat was measured for people aged 8-60
fat_data <- data |>
  filter(
    !is.na(visceral_fat_g),
    !is.na(android_fat_g),
    !is.na(gynoid_fat_g)
  ) |> 
  select(
    systolic, diastolic, blood_pressure_category,
    age, gender, race, 
    android_fat_g, android_non_fat_g,
    gynoid_fat_g, gynoid_non_fat_g,
    visceral_fat_g, subcutaneous_fat_g
  )

