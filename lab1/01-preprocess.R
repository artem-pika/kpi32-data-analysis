#----------------------------------------------------------------------
# Purpose: data preparation for further analysis
#----------------------------------------------------------------------

library(tidyverse)
library(here)

data <- read_csv(here::here("data/data0.csv"))

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

data <- data |>
  mutate(
    education = case_when(
      education == "9-11th grade (Includes 12th grade with no diploma)" ~ "9-11th grade",
      education == "High school graduate/GED or equivalent" ~ "High school graduate",
      education == "Some college or AA degree" ~ "Some college degree",
      .default = education
    )
  )
data <- data |>
  mutate(
    education = factor(
      education, 
      levels = c("Less than 9th grade", "9-11th grade", "High school graduate", "Some college degree", "College graduate or above"),
      ordered = TRUE
    )
  )

# pulse = 0 is a measurement error, pulse > 150 is fine
data <- data |>
  mutate(pulse = if_else(pulse == 0, NA, pulse))

data <- data |>
  mutate(
    android_fat_percent = android_fat_g / android_g,
    gynoid_fat_percent = gynoid_fat_g / gynoid_g,
    android_gynoid_ratio = android_fat_g / gynoid_fat_g
  )

data <- data |>
  mutate(
    vigorous_phys_activity_days = if_else(vigorous_phys_activity == "No", 0, vigorous_phys_activity_days)
  )
data <- data |>
  mutate(
    moderate_phys_activity_days = if_else(moderate_phys_activity == "No", 0, moderate_phys_activity_days)
  )

data <- data |>
  mutate(
    diet_quality = factor(
      diet_quality,
      levels = c("Poor", "Fair", "Good", "Very good", "Excellent"),
      ordered = TRUE
    )
  )

# 1. Select non-NA weight and pulse
whole_data <- data |>
  filter(
    !is.na(weight_kg),
    !is.na(pulse),
  ) |> 
  select(where(\(x) sum(is.na(x)) == 0))

# 2. Select data related to lifestyle factors
# People aged < 20 are not included
lifestyle_data <- data |>
  filter(
    !is.na(vigorous_phys_activity_days), 
    !is.na(moderate_phys_activity_days),
    !is.na(sleep),
    !is.na(smoked_100_cigs),
    !is.na(diet_quality),
    !is.na(education),
    !is.na(poverty_index),
    !is.na(weight_kg)
  ) |>
  select(where(\(x) sum(is.na(x)) == 0))


# 3. Select data related to fat distribution
# fat was measured for people aged 8-60
fat_data <- data |>
  filter(
    !is.na(visceral_fat_g),
    !is.na(android_fat_g),
    !is.na(gynoid_fat_g),
    !is.na(waist_cm),
    !is.na(weight_kg)
  ) |> 
  select(where(\(x) sum(is.na(x)) == 0)) 
