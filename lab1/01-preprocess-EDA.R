#----------------------------------------------------------------------
# Purpose: EDA that motivated data preprocessing
#----------------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)

count_na = function(data) {
  na_count <- data |>
    summarize(across(everything(), \(x) sum(is.na(x)))) |>
    pivot_longer(
      everything(),
      names_to = "column",
      values_to = "na_count"
    ) 
  na_percent <- data |>
    summarize(across(everything(), \(x) mean(is.na(x)))) |>
    pivot_longer(
      everything(),
      names_to = "column",
      values_to = "na_percent"
    )
  return(left_join(na_percent, na_count))
}
categorical_summary <- function(data, col_name) {
  data |>
    count(!!sym(col_name)) |>
    mutate(
      percentage = n / sum(n), 
      .before = 2
    )
}

data <- read_csv(here::here("data/data0.csv"))
names(data)
dim(data)
count_na(data) |>
  print(n = 50)

# Process main variables - blood pressure measurements
data |> 
  select(
    diastolic1, diastolic2, diastolic3, diastolic4, 
    systolic1, systolic2, systolic3, systolic4
  ) |>
  glimpse()
data |> 
  select(
    diastolic1, diastolic2, diastolic3, diastolic4, 
    systolic1, systolic2, systolic3, systolic4
  ) |>
  count_na()

# How much rows have at least 1 non-NA diastolic and systolic measurement?
data |>
  filter(
    (!is.na(diastolic1) | !is.na(diastolic2) | !is.na(diastolic3) | !is.na(diastolic4)) 
    & 
    (!is.na(systolic1) | !is.na(systolic2) | !is.na(systolic3) | !is.na(systolic4))
  ) |>
  nrow()

d1 <- ggplot(data, aes(x = diastolic1)) +
  geom_histogram(binwidth = 1)
d2 <- ggplot(data, aes(x = diastolic2)) +
  geom_histogram(binwidth = 1)
d3 <- ggplot(data, aes(x = diastolic3)) +
  geom_histogram(binwidth = 1)
d4 <- ggplot(data, aes(x = diastolic4)) +
  geom_histogram(binwidth = 1)
s1 <- ggplot(data, aes(x = systolic1)) +
  geom_histogram(binwidth = 1)
s2 <- ggplot(data, aes(x = systolic2)) +
  geom_histogram(binwidth = 1)
s3 <- ggplot(data, aes(x = systolic3)) +
  geom_histogram(binwidth = 1)
s4 <- ggplot(data, aes(x = systolic4)) +
  geom_histogram(binwidth = 1)
(d1 / d2 / d3 / d4) | (s1 / s2 / s3 / s4)
# Only even numbers are present in the measurements

# Is 0 diastolic blood pressure a measurement error?
data |>
  filter(diastolic1 == 0) |>
  glimpse()

data <- data |>
  mutate(
    diastolic1 = if_else(diastolic1 == 0, NA, diastolic1),
    diastolic2 = if_else(diastolic2 == 0, NA, diastolic2),
    diastolic3 = if_else(diastolic3 == 0, NA, diastolic3),
    diastolic4 = if_else(diastolic4 == 0, NA, diastolic4),
  )
data |>
  filter(
    (!is.na(diastolic1) | !is.na(diastolic2) | !is.na(diastolic3) | !is.na(diastolic4)) 
    & 
    (!is.na(systolic1) | !is.na(systolic2) | !is.na(systolic3) | !is.na(systolic4))
  ) |>
  nrow()

# Aggregate the 4 blood pressure measurements to get a single variable
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
# How much of the rows in which closest measurements differ more than by 8 will be removed?
nrow(data)

p1 <- ggplot(data, aes(x = diastolic)) +
  geom_histogram(binwidth = 1)
p2 <- ggplot(data, aes(x = systolic)) +
  geom_histogram(binwidth = 1)
p1 | p2
ggplot(data, aes(x = diastolic, y = systolic)) +
  geom_point(size = 0.1, alpha = 0.1)

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
categorical_summary(data, "blood_pressure_category")

count_na(data) |>
  print(n = 50)

# Do other variables have measurement errors?
# demographics
data |>
  count(survey_year)

ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 1)

data |>
  count(gender)

data |>
  count(race)
data <- data |>
  mutate(
    race = if_else(race == "Other Race - Including Multi-Racial", "Other Race", race)
  )

data |>
  count(education)
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

ggplot(data, aes(x = poverty_index)) +
  geom_histogram()
summary(data$poverty_index)
data |>
  filter(poverty_index == 0) |>
  glimpse()
data |>
  distinct(poverty_index)

# pulse
ggplot(data, aes(x = pulse)) +
  geom_histogram(binwidth = 1)
ggplot(data, aes(x = pulse)) +
  geom_boxplot()
data |>
  filter(pulse == 0) |>
  glimpse()
data |> 
  filter(pulse > 150) |>
  glimpse()

# pulse = 0 is a measurement error, pulse > 150 is fine
data <- data |>
  mutate(pulse = if_else(pulse == 0, NA, pulse))

# weight_kg, waist_cm
ggplot(data, aes(x = weight_kg)) +
  geom_histogram(binwidth = 1)

ggplot(data, aes(x = waist_cm)) +
  geom_histogram(binwidth = 1)

# fat
ggplot(data, aes(x = subcutaneous_fat_g)) +
  geom_histogram(binwidth = 10)

ggplot(data, aes(x = visceral_fat_g)) +
  geom_histogram(binwidth = 20)
data |>
  filter(visceral_fat_g == 0) |>
  glimpse() # children, fine

# android_fat_g + android_non_fat_g = andorid_g
data |>
  ggplot(aes(android_fat_g + android_non_fat_g, android_g)) +
  geom_point(size = 0.2)
# gynoid_fat_g + gynoid_non_fat_g = gynoid_g
data |>
  ggplot(aes(gynoid_fat_g + gynoid_non_fat_g, gynoid_g)) +
  geom_point(size = 0.2)

data <- data |>
  mutate(
    android_fat_percent = android_fat_g / android_g,
    gynoid_fat_percent = gynoid_fat_g / gynoid_g,
    android_gynoid_ratio = android_fat_g / gynoid_fat_g
  )

ggplot(data, aes(x = android_g)) +
  geom_histogram(binwidth = 100)
ggplot(data, aes(x = android_fat_g)) +
  geom_histogram(binwidth = 100)
ggplot(data, aes(x = android_non_fat_g)) +
  geom_histogram(binwidth = 100)
ggplot(data, aes(x = android_fat_percent)) +
  geom_histogram(binwidth = 0.01)

ggplot(data, aes(x = gynoid_g)) +
  geom_histogram(binwidth = 100)
ggplot(data, aes(x = gynoid_fat_g)) +
  geom_histogram(binwidth = 100)
ggplot(data, aes(x = gynoid_non_fat_g)) +
  geom_histogram(binwidth = 100)
ggplot(data, aes(x = gynoid_fat_percent)) +
  geom_histogram(binwidth = 0.01)

# lifestyle
data |>
  count(vigorous_phys_activity)
data |>
  count(vigorous_phys_activity_days)
data <- data |>
  mutate(
    vigorous_phys_activity_days = if_else(vigorous_phys_activity == "No", 0, vigorous_phys_activity_days)
  )

data |>
  count(moderate_phys_activity)
data |>
  count(moderate_phys_activity_days)
data <- data |>
  mutate(
    moderate_phys_activity_days = if_else(moderate_phys_activity == "No", 0, moderate_phys_activity_days)
  )

p1 <- ggplot(data, aes(x = sleep)) +
  geom_histogram(binwidth = 0.5)
p2 <- ggplot(data, aes(x = sleep)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~survey_year)
p1 | p2

data |>
  count(smoking_now)

data |>
  count(smoked_100_cigs)

data |>
  count(diet_quality)
data <- data |>
  mutate(
    diet_quality = factor(
      diet_quality,
      levels = c("Poor", "Fair", "Good", "Very good", "Excellent"),
      ordered = TRUE
    )
  )
data |>
  count(diet_quality)

# Check number of NA in important categories
count_na(data) |>
  print(n = 50)

# 1. Select non-NA weight and pulse
data |>
  filter(
    !is.na(weight_kg),
    !is.na(pulse),
  ) |> 
  count_na() |>
  print(n = 50)

whole_data <- data |>
  filter(
    !is.na(weight_kg),
    !is.na(pulse),
  ) |> 
  select(where(\(x) sum(is.na(x)) == 0))
dim(whole_data)
nrow(data)

# 2. Select data related to lifestyle factors
# People aged < 20 are not included
data |>
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
  count_na() |>
  print(n = 50)

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
dim(lifestyle_data)
dim(data)

ggplot(lifestyle_data, aes(x = age)) +
  geom_histogram(binwidth = 1)

# 1. Select data related to fat distribution
# fat was measured for people aged 8-60
data |>
  filter(
    !is.na(visceral_fat_g),
    !is.na(android_fat_g),
    !is.na(gynoid_fat_g)
  ) |> count_na() |> print(n = 50)

data |>
  filter(
    !is.na(visceral_fat_g),
    !is.na(android_fat_g),
    !is.na(gynoid_fat_g),
    !is.na(waist_cm),
    !is.na(weight_kg)
  ) |> dim()

fat_data <- data |>
  filter(
    !is.na(visceral_fat_g),
    !is.na(android_fat_g),
    !is.na(gynoid_fat_g),
    !is.na(waist_cm),
    !is.na(weight_kg)
  ) |> 
  select(where(\(x) sum(is.na(x)) == 0))

dim(fat_data)
dim(data)

ggplot(fat_data, aes(x = age)) +
  geom_histogram(binwidth = 1)

# Subsamples summaries
data.frame(
  list(
    "Subset name" = c("whole_data", "lifestyle_data", "fat_data"),
    "Number of observations" = c(nrow(whole_data), nrow(lifestyle_data), nrow(fat_data)),
    "Number of variables" = c(ncol(whole_data), ncol(lifestyle_data), ncol(fat_data)),
    "Age range" = c("8+", "20+", "8-59")
  )
)

# What if we combine subsamples?
dim(inner_join(fat_data, lifestyle_data))  # becomes small sample size