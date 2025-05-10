#----------------------------------------------------------------------
# Purpose: EDA
#----------------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)
library(GGally)
library(ggridges)
library(scales)

source(here::here("lab1/01-preprocess.R"))

numeric_summary <- function(data) {
  summary_statistics = list()
  for (col_name in names(data)) {
    col <- data[[col_name]]
    summary_statistics[[col_name]] <- list(
      column = col_name,
      min = min(col), 
      q1 = quantile(col, 0.25)[[1]], 
      mean = mean(col), 
      median = median(col), 
      q3 = quantile(col, 0.75)[[1]], 
      max = max(col)
    )
  }
  
  table <- do.call(bind_rows, summary_statistics) |>
    knitr::kable(
      col.names = c("variable name", "min", "1st quartile", "mean", "median", "3rd quartile", "max"),
      digits = 2
    )
  return(table)
}
categorical_summary <- function(data, col_name) {
  data |>
    count(!!sym(col_name)) |>
    mutate(
      percentage = n / sum(n), 
      .before = 2
    )
}

# 0. Age, Weight, Race, Gender ----------------------------------------------------
dim(whole_data)
names(whole_data)

# What are numeric variable summaries?
whole_data |>
  select(
    diastolic, systolic,
    age, weight_kg,
  ) |>
  numeric_summary() |>
  knitr::kable()

# What are categorical variable summaries?
categorical_summary(whole_data, "blood_pressure_category")
categorical_summary(whole_data, "gender")
categorical_summary(whole_data, "race")

# How distributions of numeric variables look?
ggplot(whole_data, aes(x = diastolic)) +
  geom_histogram(binwidth = 2)
ggplot(whole_data, aes(x = systolic)) +
  geom_histogram(binwidth = 2)

ggplot(whole_data, aes(x = pulse)) +
  geom_histogram(binwidth = 2)

ggplot(whole_data, aes(x = weight_kg)) +
  geom_histogram(binwidth = 2)

ggplot(whole_data, aes(x = age)) +
  geom_histogram(binwidth = 2)

# What are correlations with blood pressure?
whole_data |>
  select(
    diastolic, systolic, 
    weight_kg, age
  ) |>
  rename(
    weight = weight_kg,
  ) |> 
  ggcorr(
    method = c("pairwise", "spearman"),
    label = TRUE,
    label_round = 2
  ) 

# Systolic and diastolic are not similar?
ggplot(whole_data, aes(x = diastolic, y = systolic)) +
  geom_point(size = 0.2, alpha = 0.2)
ggplot(whole_data, aes(x = diastolic, y = systolic, color = blood_pressure_category)) +
  geom_point(size = 0.2, alpha = 0.2) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 1.5)))

# Older people have higher blood pressure?
p1 <- ggplot(whole_data, aes(x = age, y = systolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth()
p2 <- ggplot(whole_data, aes(x = age, y = diastolic)) +
  geom_point(size = 0.2, alpha = 0.1) +
  geom_smooth()
p1 | p2

ggplot(whole_data, aes(x = age, y = systolic)) +
  geom_boxplot(aes(group = cut_interval(age, 12)))
ggplot(whole_data, aes(x = age, y = diastolic)) +
  geom_boxplot(aes(group = cut_interval(age, 12)))

ggplot(whole_data, aes(x = age, y = blood_pressure_category, fill = blood_pressure_category)) +
  geom_density_ridges(alpha = 0.5) +
  theme(legend.position = "none")

# People with more weight have higher blood pressure?
whole_data |>
  filter(weight_kg > 200) |>
  ggplot(aes(x = blood_pressure_category)) +
  geom_bar()

ggplot(whole_data, aes(x = weight_kg, y = systolic, color = age)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  scale_color_viridis_c()
ggplot(whole_data, aes(x = weight_kg, y = diastolic, color = age)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  scale_color_viridis_c()

# How does age affect weight?
ggplot(whole_data, aes(x = age, y = weight_kg)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth()

# For fixed age, people with more weight have higher blood pressure?
# We need a variable that represents if person has large weight, that is independent of age.
whole_data <- whole_data |>
  group_by(age) |>
  mutate(
    relative_weight = (weight_kg - mean(weight_kg)) / sd(weight_kg)
  )

whole_data |>
  select(
    diastolic, systolic, 
    relative_weight, weight_kg, age
  ) |>
  ggcorr(
    method = c("pairwise", "spearman"),
    label = TRUE,
    label_round = 2
  )

ggplot(whole_data, aes(x = relative_weight, y = systolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth()
ggplot(whole_data, aes(x = relative_weight, y = diastolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth()
ggplot(whole_data, aes(x = relative_weight, color = blood_pressure_category)) +
  geom_density()

# What about categorical variables?
# Does gender affect blood pressure?
p1 <- ggplot(whole_data, aes(x = systolic, color = gender)) +
  geom_density()
p2 <- ggplot(whole_data, aes(x = diastolic, color = gender)) +
  geom_density()
(p1 | p2) + plot_layout(guides = "collect")

ggplot(whole_data, aes(y = systolic, x = gender)) +
  geom_boxplot()
ggplot(whole_data, aes(y = diastolic, x = gender)) +
  geom_boxplot()
whole_data |>
  group_by(gender) |>
  summarise(
    diastolic_mean = mean(diastolic),
    diastolic_median = median(diastolic),
    systolic_mean = mean(systolic),
    systolic_median = median(systolic),
  )

# Is it because of weight?
ggplot(whole_data, aes(x = weight_kg, color = gender)) +
  geom_density()

# Does blood pressure differ by races?
p1 <- ggplot(whole_data, aes(y = fct_reorder(race, systolic, \(x) -median(x)), x = systolic)) +
  geom_boxplot() +
  labs(y = "Race")
p2 <- ggplot(whole_data, aes(y = fct_reorder(race, diastolic, \(x) -median(x)), x = diastolic)) +
  geom_boxplot() +
  labs(y = "Race")
p1 | p2

whole_data |>
  group_by(race) |>
  summarise(
    systolic_median = median(systolic),
    diastolic_median = median(diastolic)
  ) |>
  arrange(systolic_median)


# 1. Lifestyle ---------------------------------------------------------------
# Important note: people aged only 20-80 are present in this sample
dim(lifestyle_data)
names(lifestyle_data)

# What are numeric variable summaries?
lifestyle_data |>
  select(
    diastolic, systolic, 
    age, weight_kg, 
    poverty_index, sleep
  ) |>
  numeric_summary()  
# this subsample tends to have higher blood pressure because of age > 20

# What are categorical variable summaries?
categorical_summary(lifestyle_data, "blood_pressure_category")
categorical_summary(lifestyle_data, "gender")
categorical_summary(lifestyle_data, "race")
categorical_summary(lifestyle_data, "education")
categorical_summary(lifestyle_data, "vigorous_phys_activity_days")
categorical_summary(lifestyle_data, "moderate_phys_activity_days")
categorical_summary(lifestyle_data, "smoked_100_cigs")
categorical_summary(lifestyle_data, "diet_quality")

# How distributions of numeric variables look?
p1 <- ggplot(lifestyle_data, aes(x = diastolic)) +
  geom_histogram(binwidth = 2)
p2 <- ggplot(lifestyle_data, aes(x = systolic)) +
  geom_histogram(binwidth = 2)
p3 <- ggplot(lifestyle_data, aes(x = weight_kg)) +
  geom_histogram()
p4 <- ggplot(lifestyle_data, aes(x = age)) +
  geom_histogram(binwidth = 2)
p5 <- ggplot(lifestyle_data, aes(x = poverty_index)) +
  geom_histogram()
p6 <- ggplot(lifestyle_data, aes(x = sleep)) +
  geom_histogram(binwidth = 1)

(p1 | p2 | p3) / (p4 | p5 | p6)

# What are correlations with blood pressure?
lifestyle_data |>
  select(
    diastolic, systolic, 
    weight_kg, age,
    poverty_index, sleep
  ) |>
  rename(
    weight = weight_kg,
    income = poverty_index
  ) |> 
  ggcorr(
    method = c("pairwise", "spearman"),
    label = TRUE,
    label_round = 2
  )

# No relationship between sleep, income and blood pressure?
ggplot(lifestyle_data, aes(x = poverty_index, y = systolic)) +
  geom_point(size = 0.2, alpha = 0.2)
ggplot(lifestyle_data, aes(x = sleep, y = systolic)) +
  geom_point(size = 0.2, alpha = 0.2)
ggplot(lifestyle_data, aes(x = sleep, color = blood_pressure_category)) +
  geom_density()

# Maybe really rich people have better blood pressure?
ggplot(lifestyle_data, aes(x = systolic, color = poverty_index >= 5)) +
  geom_density()

# What about categorical variables?
# Lower education means less healthy person?
ggplot(lifestyle_data, aes(x = systolic, y = education)) +
  geom_boxplot()
ggplot(lifestyle_data, aes(x = diastolic, y = education)) +
  geom_boxplot()

# just curious about distribution of education among age > 20
ggplot(lifestyle_data, aes(y = education, x = age)) +
  geom_boxplot(varwidth = TRUE)

# just curious about distribution of education among different genders
ggplot(lifestyle_data, aes(x = gender, fill = education)) +
  geom_bar(position = "fill")

# Physical activity?
ggplot(lifestyle_data, aes(x = systolic, color = vigorous_phys_activity)) +
  geom_density()
p1 <- ggplot(lifestyle_data, aes(x = systolic, y = factor(vigorous_phys_activity_days))) +
  geom_boxplot() +
  labs(y = "days", title = "Vigorous")+
  theme(plot.title = element_text(hjust = 0.5))
p2 <- ggplot(lifestyle_data, aes(x = systolic, y = vigorous_phys_activity)) +
  geom_boxplot() +
  labs(y = "days > 0")
# Moderate physical activity has lesser effect?
p3 <- ggplot(lifestyle_data, aes(x = systolic, y = factor(moderate_phys_activity_days))) +
  geom_boxplot() +
  labs(y = "days", title = "Moderate") +
  theme(plot.title = element_text(hjust = 0.5))
p4 <- ggplot(lifestyle_data, aes(x = systolic, y = moderate_phys_activity)) +
  geom_boxplot() +
  labs(y = "days > 0")
(p1 / p2) | (p3 / p4)

# Smoking?
ggplot(lifestyle_data, aes(x = systolic, color = smoked_100_cigs)) +
  geom_density()
ggplot(lifestyle_data, aes(x = systolic, y = smoked_100_cigs)) +
  geom_boxplot()
ggplot(lifestyle_data, aes(x = diastolic, y = smoked_100_cigs)) +
  geom_boxplot()

# Do males or females smoke more?
ggplot(lifestyle_data, aes(x = gender, fill = smoked_100_cigs)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Percentage", labels = label_percent())

lifestyle_data |>
  group_by(gender) |>
  summarize(smoked_100_cigs_percent = mean(smoked_100_cigs == "Yes"))

# Diet?
ggplot(lifestyle_data, aes(x = systolic, y = diet_quality)) +
  geom_boxplot()

# How lifestyle differs among races?
ggplot(lifestyle_data, aes(y = fct_reorder(race, blood_pressure_category, \(x) -mean(x == "Normal")), fill = blood_pressure_category)) +
  geom_bar(position = "fill") +
  labs(y = "Race") +
  scale_x_continuous(name = "Percentage", labels = label_percent())
ggplot(lifestyle_data, aes(y = fct_reorder(race, education, \(x) -mean(x == "College graduate or above")), fill = education)) +
  geom_bar(position = "fill") +
  labs(y = "Race") +
  scale_x_continuous(name = "Percentage", labels = label_percent())
ggplot(lifestyle_data, aes(y = fct_reorder(race, smoked_100_cigs, \(x) mean(x == "Yes")), fill = smoked_100_cigs)) +
  geom_bar(position = "fill") +
  labs(y = "Race") + 
  scale_x_continuous(name = "Percentage", labels = label_percent())
ggplot(lifestyle_data, aes(y = fct_reorder(race, diet_quality, \(x) -mean(x %in% c("Excellent", "Very good"))), fill = diet_quality)) +
  geom_bar(position = "fill") +
  labs(y = "Race") + 
  scale_x_continuous(name = "Percentage", labels = label_percent())
ggplot(lifestyle_data, aes(y = fct_reorder(race, weight_kg, \(x) median(x)), x = weight_kg)) +
  geom_boxplot() +
  labs(y = "Race")

# 2. Fat ------------------------------------------------------------------
# Important note: people aged only 0-60 are present in this sample
dim(fat_data)
names(fat_data)

# What are numeric variable summaries?
fat_data |>
  select(
    diastolic, systolic,
    age, weight_kg, waist_cm, 
    subcutaneous_fat_g, visceral_fat_g,
    android_g, android_fat_g, android_non_fat_g,
    gynoid_g, gynoid_fat_g, gynoid_non_fat_g
  ) |>
  numeric_summary()

# What are categorical variable summaries?
categorical_summary(fat_data, "blood_pressure_category")
categorical_summary(fat_data, "gender")
categorical_summary(fat_data, "race")

# How distributions of numeric variables look?
p1 <- ggplot(fat_data, aes(x = diastolic)) +
  geom_histogram()
p2 <- ggplot(fat_data, aes(x = systolic)) +
  geom_histogram()

p3 <- ggplot(fat_data, aes(x = weight_kg)) +
  geom_histogram()
p4 <- ggplot(fat_data, aes(x = waist_cm)) +
  geom_histogram()

p5 <- ggplot(fat_data, aes(x = age)) +
  geom_histogram(binwidth = 2)
(p1 | p2) / (p3 | p4 | p5)

p1 <- ggplot(fat_data, aes(x = subcutaneous_fat_g)) +
  geom_histogram() +
  scale_x_log10()
p2 <- ggplot(fat_data, aes(x = visceral_fat_g)) +
  geom_histogram(bins = 100) +
  scale_x_log10()
p1 | p2

p1 <- ggplot(fat_data, aes(x = android_g)) +
  geom_histogram() +
  scale_x_log10()
p2 <- ggplot(fat_data, aes(x = android_fat_g)) +
  geom_histogram() +
  scale_x_log10()
p3 <- ggplot(fat_data, aes(x = android_non_fat_g)) +
  geom_histogram() +
  scale_x_log10()

p4 <- ggplot(fat_data, aes(x = gynoid_g)) +
  geom_histogram() +
  scale_x_log10()
p5 <- ggplot(fat_data, aes(x = gynoid_fat_g)) +
  geom_histogram() +
  scale_x_log10()
p6 <- ggplot(fat_data, aes(x = gynoid_non_fat_g)) +
  geom_histogram() +
  scale_x_log10()
(p1 / p2 / p3) | (p4 / p5 / p6)

# Is fat distribution different among men and women?
ggplot(fat_data, aes(x = android_fat_percent, y = gynoid_fat_percent)) +
  geom_point(size = 0.2, alpha = 0.2)
ggplot(fat_data, aes(x = subcutaneous_fat_g, y = visceral_fat_g)) +
  geom_point(size = 0.2, alpha = 0.2)

ggplot(fat_data, aes(x = android_fat_percent, y = gynoid_fat_percent, color = gender)) +
  geom_point(size = 0.2, alpha = 0.2) + 
  guides(color = guide_legend(override.aes = list(size = 1.5, alpha = 1)))
ggplot(fat_data, aes(x = subcutaneous_fat_g, y = visceral_fat_g, color = gender)) +
  geom_point(size = 0.2, alpha = 0.2) + 
  guides(color = guide_legend(override.aes = list(size = 1.5, alpha = 1)))

# How does android and gynoid fat distribution differ among males and females?
ggplot(fat_data, aes(x = android_g, color = gender)) +
  geom_density()
ggplot(fat_data, aes(x = android_fat_g, color = gender)) +
  geom_density()
p1 <- ggplot(fat_data, aes(x = android_fat_percent, color = gender)) +
  geom_density()

ggplot(fat_data, aes(x = gynoid_g, color = gender)) +
  geom_density()
ggplot(fat_data, aes(x = gynoid_fat_g, color = gender)) +
  geom_density()
p2 <- ggplot(fat_data, aes(x = gynoid_fat_percent, color = gender)) +
  geom_density()
(p1 | p2) + plot_layout(guides = "collect")

ggplot(fat_data, aes(x = android_gynoid_ratio, color = gender)) +
  geom_density()

# Does more fat (android, gynoid, visceral, subcutaneous) indicate higher blood pressure?
# Lets check the correlations
p1 <- fat_data |>
  filter(gender == "Male") |>
  select(
    diastolic, systolic,
    android_fat_g, gynoid_fat_g,
    visceral_fat_g, subcutaneous_fat_g
  ) |>
  ggcorr(
    method = c("pairwise", "spearman"),
    label = TRUE,
    label_round = 2
  ) +
  labs(title = "Male")
p2 <- fat_data |>
  filter(gender == "Female") |>
  select(
    diastolic, systolic,
    android_fat_g, gynoid_fat_g,
    visceral_fat_g, subcutaneous_fat_g
  ) |>
  ggcorr(
    method = c("pairwise", "spearman"),
    label = TRUE,
    label_round = 2
  ) +
  labs(title = "Female")
(p1 | p2) + plot_layout(guides = "collect")

# Look at relationships
p1 <- ggplot(fat_data, aes(x = android_fat_g, y = systolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)
p2 <- ggplot(fat_data, aes(x = android_fat_g, y = diastolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)
p1 | p2
p1 <- ggplot(fat_data, aes(x = gynoid_fat_g, y = systolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)
p2 <- ggplot(fat_data, aes(x = gynoid_fat_g, y = diastolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)
p1 | p2
p1 <- ggplot(fat_data, aes(x = visceral_fat_g, y = systolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)
p2 <- ggplot(fat_data, aes(x = visceral_fat_g, y = diastolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)
p1 | p2
p1 <- ggplot(fat_data, aes(x = subcutaneous_fat_g, y = systolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)
p2 <- ggplot(fat_data, aes(x = subcutaneous_fat_g, y = diastolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)
p1 | p2

# Fat amount depends on age
ggplot(fat_data, aes(x = age, y = android_fat_g)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)
ggplot(fat_data, aes(x = age, y = gynoid_fat_g)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)
ggplot(fat_data, aes(x = age, y = visceral_fat_g)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)
ggplot(fat_data, aes(x = age, y = subcutaneous_fat_g)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)

# With age visceral fat grows while weight not
fat_data |>
  mutate(visceral_fat_kg = visceral_fat_g / 1000) |>
  ggplot(aes(x = age)) +
  geom_smooth(aes(y = weight_kg), color = "deepskyblue2") +
  geom_smooth(aes(y = visceral_fat_kg * 100), color = "brown2") +
  scale_y_continuous(
    name = "Weight (kg)",
    sec.axis = sec_axis(~ . / 100, name="Visceral fat (kg)")
  ) +
  theme(
    axis.title.y = element_text(color = "deepskyblue2"),
    axis.title.y.right = element_text(color = "brown2")
  )
fat_data |>
  mutate(subcutaneous_fat_kg = subcutaneous_fat_g / 1000) |>
  ggplot(aes(x = age)) +
  geom_smooth(aes(y = weight_kg), color = "deepskyblue2") +
  geom_smooth(aes(y = subcutaneous_fat_kg * 40), color = "chocolate1") +
  scale_y_continuous(
    name = "Weight (kg)",
    sec.axis = sec_axis(~ . / 40, name="Subcutaneous fat (kg)")
  ) +
  theme(
    axis.title.y = element_text(color = "deepskyblue2"),
    axis.title.y.right = element_text(color = "chocolate1")
  )

# Fat mass or lean mass causes higher blood pressure?
fat_data <- fat_data |>
  mutate(
    android_fat_percent = android_fat_g / android_g,
    gynoid_fat_percent = gynoid_fat_g / gynoid_g
  )

p1 <- fat_data |>
  filter(gender == "Male") |>
  select(
    diastolic, systolic,
    android_fat_percent, gynoid_fat_percent
  ) |>
  ggcorr(
    method = c("pairwise", "spearman"),
    label = TRUE,
    label_round = 2
  ) +
  labs(title = "Male")
p2 <- fat_data |>
  filter(gender == "Female") |>
  select(
    diastolic, systolic,
    android_fat_percent, gynoid_fat_percent
  ) |>
  ggcorr(
    method = c("pairwise", "spearman"),
    label = TRUE,
    label_round = 2
  ) +
  labs(title = "Female")
(p1 | p2) + plot_layout(guides = "collect")

ggplot(fat_data, aes(x = android_fat_percent, y = systolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  scale_x_continuous(labels = label_percent()) +
  facet_wrap(~gender)
ggplot(fat_data, aes(x = android_fat_percent, y = diastolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  scale_x_continuous(labels = label_percent()) +
  facet_wrap(~gender)
ggplot(fat_data, aes(x = gynoid_fat_percent, y = systolic, color = age)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  scale_color_viridis_c() +
  scale_x_continuous(labels = label_percent()) +
  facet_wrap(~gender)
ggplot(fat_data, aes(x = gynoid_fat_percent, y = diastolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  scale_x_continuous(labels = label_percent()) +
  facet_wrap(~gender)

ggplot(fat_data, aes(x = age, y = android_fat_percent)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth()
ggplot(fat_data, aes(x = age, y = gynoid_fat_percent)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth()

# Android fat is more dangerous than gynoid fat?
p1 <- fat_data |>
  filter(gender == "Male") |>
  select(
    diastolic, systolic,
    android_gynoid_ratio
  ) |>
  ggcorr(
    method = c("pairwise", "spearman"),
    label = TRUE,
    label_round = 2
  ) +
  labs(title = "Male")
p2 <- fat_data |>
  filter(gender == "Female") |>
  select(
    diastolic, systolic,
    android_gynoid_ratio
  ) |>
  ggcorr(
    method = c("pairwise", "spearman"),
    label = TRUE,
    label_round = 2
  ) +
  labs(title = "Female")
(p1 | p2) + plot_layout(guides = "collect")

ggplot(fat_data, aes(x = android_gynoid_ratio, y = systolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() + 
  facet_wrap(~gender) 
ggplot(fat_data, aes(x = android_gynoid_ratio, y = diastolic)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() + 
  facet_wrap(~gender) 

# Visceral fat is more dangerous than subcutaneous?
fat_data <- fat_data |>
  mutate(
    visceral_fat_percent = visceral_fat_g / weight_kg,
    subc_fat_percent = subcutaneous_fat_g / weight_kg
  )
  
p1 <- fat_data |>
  filter(gender == "Male") |>
  select(
    diastolic, systolic,
    visceral_fat_percent, subc_fat_percent
  ) |>
  ggcorr(
    method = c("pairwise", "spearman"),
    label = TRUE,
    label_round = 2
  ) +
  labs(title = "Male")
p2 <- fat_data |>
  filter(gender == "Female") |>
  select(
    diastolic, systolic,
    visceral_fat_percent, subc_fat_percent
  ) |>
  ggcorr(
    method = c("pairwise", "spearman"),
    label = TRUE,
    label_round = 2
  ) +
  labs(title = "Female")
(p1 | p2) + plot_layout(guides = "collect")

ggplot(fat_data, aes(x = age, y = visceral_fat_percent)) +
  geom_point(size = 0.2, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~gender)
