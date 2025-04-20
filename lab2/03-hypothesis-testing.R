#----------------------------------------------------------------------
# Purpose: Test hypothesis using confidence intervals and statistical tests
#----------------------------------------------------------------------

library(tidyverse)
library(here)
library(boot)

source(here::here("lab2/01-preprocess.R"))

# Statistics --------------------------------------------------------------
var_of_mean <- function(x) {
  return(var(x) / length(x))
}
se_of_mean <- function(x) {
  return(sqrt(var_of_mean(x)))
}
var_of_sd <- function(x) {
  n <- length(x)
  m2 <- mean((x - mean(x))**2)
  m4 <- mean((x - mean(x))**4)
  return((m4 / m2 - m2) / 4 / n)
}
se_of_sd <- function(x) {
  return(sqrt(var_of_sd(x)))
}
var_of_quantile <- function(x, q, R = 300) {
  qs <- boot(x, \(x, inds) quantile(x[inds], q), R = R)$t |>
    as.numeric()  # var() needs vector as input, otherwise it will output 1by1 matrix
  return(var(qs))
}
se_of_quantile <- function(x, q, R = 300) {
  return(sqrt(var_of_quantile(x, q, R = R)))
}
var_of_median <- function(x, R = 300) {
  return(var_of_quantile(x, q = 0.5, R = R))
}
se_of_median <- function(x, R = 300) {
  return(se_of_quantile(x, q = 0.5, R = R))
}

boot_cor <- function(data, indices) {
  return(cor(data[indices, 1], data[indices, 2], method = "spearman"))
}
double_boot_cor <- function(data, indices, R_for_sd = 200) {
  cor_hat <- boot_cor(data, indices)
  boot_out <- boot(data[indices, ], boot_cor, R = R_for_sd)
  return(c(cor_hat, var(as.numeric(boot_out$t))))
}

conf_int <- function(data, column, statistic, se_of_statistic, conf = 0.95, digits = 2, do_arrange = FALSE) {
  result <- data |>
    summarise(
      value = statistic(!!sym(column)),
      se = se_of_statistic(!!sym(column)),
      a = value + qnorm(0.5 - conf / 2) * se,
      b = value + qnorm(0.5 + conf / 2) * se
    ) 
  if (do_arrange == TRUE) {
    result <- result |> arrange(value)
  }
  return(knitr::kable(result, digits = digits))
}

display_conf_int <- function(a, b) {
  paste("[", a, ", ", b, "]", sep = "")
}
display_cor_conf_ints <- function(conf_ints, digits = 3) {
  return(sprintf(
    "norm [%s, %s], basic [%s, %s], perc [%s, %s]", 
    formatC(conf_ints$normal, digits = digits)[2],
    formatC(conf_ints$normal, digits = digits)[3],
    formatC(conf_ints$basic, digits = digits)[4], 
    formatC(conf_ints$basic, digits = digits)[5],
    formatC(conf_ints$perc, digits = digits)[4],
    formatC(conf_ints$perc, digits = digits)[5]
  ))
}

all_p_values <- c()
# Blood Pressure ----------------------------------------------------------
# Descriptive statistics
conf_int(whole_data, "systolic", mean, se_of_mean)
conf_int(whole_data, "systolic", sd, se_of_sd)

conf_int(whole_data, "diastolic", mean, se_of_mean)
conf_int(whole_data, "diastolic", sd, se_of_sd)

whole_data |>
  group_by(blood_pressure_category) |>
  summarise(
    p = n() / dim(whole_data)[1],
    p_se = sqrt(p * (1 - p) / dim(whole_data)[1]),
    a = p + qnorm(0.025) * p_se,
    b = p + qnorm(0.975) * p_se,
  )

# All 5 types of conf. intervals
set.seed(1)
boot_out <- select(whole_data, systolic, diastolic) |>
  boot(double_boot_cor, R = 200, R_for_sd = 50)
boot.ci(boot_out, type = c("norm", "basic", "stud", "perc"))
boot_out$statistic <- boot_cor  # avoid double bootstrap in BCa
boot.ci(boot_out, type = "bca")

# From now on only normal, basic and percentile will be used
# correlation between systolic and diastolic
boot_out <- select(whole_data, systolic, diastolic) |>
  boot(boot_cor, R = 1000)
boot.ci(boot_out, type = c("norm", "basic", "perc"))


# Age, race, gender -------------------------------------------------------
# Higher age means worse systolic blood pressure?
grouped_by_age <- whole_data |>
  mutate(age_category = cut_width(age, 10)) |>
  group_by(age_category)

conf_int(grouped_by_age, "systolic", mean, se_of_mean, digits = 1)
conf_int(grouped_by_age, "systolic", sd, se_of_sd, digits = 1)

conf_int(grouped_by_age, "diastolic", mean, se_of_mean, digits = 1)
conf_int(grouped_by_age, "diastolic", sd, se_of_sd, digits = 1)

# TEST mean_i+1 > mean_i and sd_i+1 > sd_i
age_categories <- summarise(grouped_by_age)$age_category

p_values <- c()
for (i in 1:(length(age_categories) - 1)) {
  x <- filter(ungroup(grouped_by_age), age_category == age_categories[i])$systolic
  y <- filter(ungroup(grouped_by_age), age_category == age_categories[i + 1])$systolic 
  test_value <- (mean(x) - mean(y)) / sqrt(var_of_mean(x) + var_of_mean(y))
  p_values[i] <- pnorm(test_value)
}
tibble(formatC(p_values))
tibble(p_values < 0.05)
all_p_values <- c(all_p_values, p_values)

p_values <- c()
for (i in 1:(length(age_categories) - 1)) {
  x <- filter(ungroup(grouped_by_age), age_category == age_categories[i])$systolic
  y <- filter(ungroup(grouped_by_age), age_category == age_categories[i + 1])$systolic 
  test_value <- (sd(x) - sd(y)) / sqrt(var_of_sd(x) + var_of_sd(y))
  p_values[i] <- pnorm(test_value)
}
tibble(formatC(p_values))
tibble(p_values < 0.05)
all_p_values <- c(all_p_values, p_values)

# correlation between age and systolic, diastolic
set.seed(1)
boot_out <- select(whole_data, age, systolic) |>
  boot(boot_cor, R = 1000)
boot.ci(boot_out, type = c("norm", "basic", "perc")) |>
  display_cor_conf_ints()

boot_out <- whole_data |>
  filter(age < 50) |>
  select(age, diastolic) |>
  boot(boot_cor, R = 1000)
boot.ci(boot_out, type = c("norm", "basic", "perc")) |>
  display_cor_conf_ints()

boot_out <- whole_data |>
  filter(age >= 50) |>
  select(age, diastolic) |>
  boot(boot_cor, R = 1000)
boot.ci(boot_out, type = c("norm", "basic", "perc")) |>
  display_cor_conf_ints()

# age distribution for different blood pressure categories
set.seed(1)
whole_data |>
  group_by(blood_pressure_category) |>
  conf_int("age", median, se_of_median)

whole_data |>
  filter(blood_pressure_category != "Elevated") |>
  group_by(blood_pressure_category == "Normal") |>
  conf_int("age", partial(quantile, probs=0.25), partial(se_of_quantile, q=0.25))
whole_data |>
  filter(blood_pressure_category != "Elevated") |>
  group_by(blood_pressure_category == "Normal") |>
  conf_int("age", partial(quantile, probs=0.75), partial(se_of_quantile, q=0.75))

x <- filter(whole_data, blood_pressure_category == "Normal")$age
y <- filter(whole_data, blood_pressure_category %in% c("Hypertension Stage 1", "Hypertension Stage 2"))$age
test_value <- (quantile(x, 0.75) - quantile(y, 0.25)) / sqrt(var_of_quantile(x, 0.25) + var_of_quantile(y, 0.75))
formatC(pnorm(test_value))
all_p_values <- c(all_p_values, pnorm(test_value))

# Blood pressure differs by gender 
whole_data |>
  group_by(gender) |>
  conf_int("systolic", mean, se_of_mean) 
whole_data |>
  group_by(gender) |>
  conf_int("diastolic", mean, se_of_mean)

x <- filter(whole_data, gender == "Female")$systolic
y <- filter(whole_data, gender == "Male")$systolic
test_value <- (mean(x) - mean(y)) / sqrt(var_of_mean(x) + var_of_mean(y))
formatC(pnorm(test_value))
all_p_values <- c(all_p_values, pnorm(test_value))

x <- filter(whole_data, gender == "Female")$diastolic
y <- filter(whole_data, gender == "Male")$diastolic
test_value <- (mean(x) - mean(y)) / sqrt(var_of_mean(x) + var_of_mean(y))
formatC(pnorm(test_value))
all_p_values <- c(all_p_values, pnorm(test_value))

# Same age distribution by gender?
whole_data |>
  group_by(gender) |>
  conf_int("age", median, se_of_median)

# Blood pressure differs by race
group_by(whole_data, race) |>
  conf_int("systolic", mean, se_of_mean, digits = 1, do_arrange = TRUE)
group_by(whole_data, race) |>
  conf_int("diastolic", mean, se_of_mean, digits = 1, do_arrange = TRUE)

# Same age distribution by race?
set.seed(1)
group_by(whole_data, race) |>
  conf_int("age", median, se_of_median, digits = 1, do_arrange = TRUE)

# race and gender are independent?
whole_data |>
  group_by(race) |>
  summarise(
    p = mean(gender == "Male"),
    se = sqrt(p * (1 - p) / n()),
    a = p + qnorm(0.025) * se,
    b = p + qnorm(0.975) * se,
  )

# Fat ---------------------------------------------------------------------
fat_data <- fat_data |>
  mutate(
    android_fat_percent = android_fat_g / (android_non_fat_g + android_fat_g),
    gynoid_fat_percent = gynoid_fat_g / (gynoid_non_fat_g + gynoid_fat_g),
    android_gynoid_fat_ratio = android_fat_g / gynoid_fat_g,
    android_gynoid_non_fat_ratio = android_non_fat_g / gynoid_non_fat_g,
    visceral_subcutaneous_ratio = visceral_fat_g / subcutaneous_fat_g,
  )

# Fat distribution differs by gender
grouped_by_gender <- group_by(fat_data, gender)

# Android region
set.seed(1)
conf_int(grouped_by_gender, "android_fat_g", partial(quantile, probs=0.2), partial(se_of_quantile, q=0.2), digits=0)
conf_int(grouped_by_gender, "android_fat_g", partial(quantile, probs=0.5), partial(se_of_quantile, q=0.5), digits=0)
conf_int(grouped_by_gender, "android_fat_g", partial(quantile, probs=0.8), partial(se_of_quantile, q=0.8), digits=0)

conf_int(grouped_by_gender, "android_non_fat_g", partial(quantile, probs=0.2), partial(se_of_quantile, q=0.2), digits=0)
conf_int(grouped_by_gender, "android_non_fat_g", partial(quantile, probs=0.5), partial(se_of_quantile, q=0.5), digits=0)
conf_int(grouped_by_gender, "android_non_fat_g", partial(quantile, probs=0.8), partial(se_of_quantile, q=0.8), digits=0)

conf_int(grouped_by_gender, "android_fat_percent", partial(quantile, probs=0.2), partial(se_of_quantile, q=0.2), digits=3)
conf_int(grouped_by_gender, "android_fat_percent", partial(quantile, probs=0.5), partial(se_of_quantile, q=0.5), digits=3)
conf_int(grouped_by_gender, "android_fat_percent", partial(quantile, probs=0.8), partial(se_of_quantile, q=0.8), digits=3)

# Gynoid region
set.seed(1)
conf_int(grouped_by_gender, "gynoid_fat_g", partial(quantile, probs=0.2), partial(se_of_quantile, q=0.2), digits=0)
conf_int(grouped_by_gender, "gynoid_fat_g", partial(quantile, probs=0.5), partial(se_of_quantile, q=0.5), digits=0)
conf_int(grouped_by_gender, "gynoid_fat_g", partial(quantile, probs=0.8), partial(se_of_quantile, q=0.8), digits=0)

conf_int(grouped_by_gender, "gynoid_non_fat_g", partial(quantile, probs=0.2), partial(se_of_quantile, q=0.2), digits=0)
conf_int(grouped_by_gender, "gynoid_non_fat_g", partial(quantile, probs=0.5), partial(se_of_quantile, q=0.5), digits=0)
conf_int(grouped_by_gender, "gynoid_non_fat_g", partial(quantile, probs=0.8), partial(se_of_quantile, q=0.8), digits=0)

conf_int(grouped_by_gender, "gynoid_fat_percent", partial(quantile, probs=0.2), partial(se_of_quantile, q=0.2), digits=3)
conf_int(grouped_by_gender, "gynoid_fat_percent", partial(quantile, probs=0.5), partial(se_of_quantile, q=0.5), digits=3)
conf_int(grouped_by_gender, "gynoid_fat_percent", partial(quantile, probs=0.8), partial(se_of_quantile, q=0.8), digits=3)

# Android fat / gynoid fat
set.seed(1)
conf_int(grouped_by_gender, "android_gynoid_fat_ratio", partial(quantile, probs=0.5), partial(se_of_quantile, q=0.5), digits=3)
conf_int(grouped_by_gender, "android_gynoid_non_fat_ratio", partial(quantile, probs=0.5), partial(se_of_quantile, q=0.5), digits=3)

# Test female android/gynoid median < male android/gynoid median
x <- filter(grouped_by_gender, gender == "Female")$android_gynoid_fat_ratio
y <- filter(grouped_by_gender, gender == "Male")$android_gynoid_fat_ratio
test_value <- (median(x) - median(y)) / sqrt(var_of_median(x) + var_of_median(y))
formatC(pnorm(test_value))
all_p_values <- c(all_p_values, pnorm(test_value))

x <- filter(grouped_by_gender, gender == "Male")$android_gynoid_non_fat_ratio
y <- filter(grouped_by_gender, gender == "Female")$android_gynoid_non_fat_ratio
test_value <- (median(x) - median(y)) / sqrt(var_of_median(x) + var_of_median(y))
formatC(pnorm(test_value))
all_p_values <- c(all_p_values, pnorm(test_value))

# Visceral and subcutaneous fat
set.seed(1)
conf_int(grouped_by_gender, "visceral_fat_g", partial(quantile, probs=0.2), partial(se_of_quantile, q=0.2), digits=0)
conf_int(grouped_by_gender, "visceral_fat_g", partial(quantile, probs=0.5), partial(se_of_quantile, q=0.5), digits=0)
conf_int(grouped_by_gender, "visceral_fat_g", partial(quantile, probs=0.8), partial(se_of_quantile, q=0.8), digits=0)

conf_int(grouped_by_gender, "subcutaneous_fat_g", partial(quantile, probs=0.2), partial(se_of_quantile, q=0.2), digits=0)
conf_int(grouped_by_gender, "subcutaneous_fat_g", partial(quantile, probs=0.5), partial(se_of_quantile, q=0.5), digits=0)
conf_int(grouped_by_gender, "subcutaneous_fat_g", partial(quantile, probs=0.8), partial(se_of_quantile, q=0.8), digits=0)

conf_int(grouped_by_gender, "visceral_subcutaneous_ratio", partial(quantile, probs=0.2), partial(se_of_quantile, q=0.2), digits=3)
conf_int(grouped_by_gender, "visceral_subcutaneous_ratio", partial(quantile, probs=0.5), partial(se_of_quantile, q=0.5), digits=3)
conf_int(grouped_by_gender, "visceral_subcutaneous_ratio", partial(quantile, probs=0.8), partial(se_of_quantile, q=0.8), digits=3)


# Correlations between fats
females <- filter(fat_data, gender == "Female")
males <- filter(fat_data, gender == "Male")

# higher android fat ~ higher andorid non-fat
set.seed(1)
boot_out <- select(females, android_fat_g, android_non_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, android_fat_g, android_non_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

# higher gynoid fat ~ higher gynoid non fat
boot_out <- select(females, gynoid_fat_g, gynoid_non_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, gynoid_fat_g, gynoid_non_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

# higher android fat ~ higher gynoid fat
boot_out <- select(females, android_fat_g, gynoid_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, android_fat_g, gynoid_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

# higher visceral fat ~ higher subcutaneous fat
boot_out <- select(females, visceral_fat_g, subcutaneous_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, visceral_fat_g, subcutaneous_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

# Amount of fat with age
adult_females <- fat_data |> filter(age >= 26, gender == "Female")
adult_males <- fat_data |> filter(age >= 26, gender == "Male")

set.seed(1)
boot_out <- select(adult_females, age, android_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(adult_males, age, android_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(adult_females, age, gynoid_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(adult_males, age, gynoid_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(adult_females, age, visceral_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(adult_males, age, visceral_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(adult_females, age, subcutaneous_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(adult_males, age, subcutaneous_fat_g) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

# Visceral, subcutaneous, gynoid, android_non_fat, gynoid_non_fat -> blood pressure
females <- filter(fat_data, gender == "Female")
males <- filter(fat_data, gender == "Male")

# Android region -> blood pressure
set.seed(1)
# systolic
boot_out <- select(females, android_fat_g, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, android_fat_g, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, android_non_fat_g, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, android_non_fat_g, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, android_fat_percent, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, android_fat_percent, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

# diastolic
boot_out <- select(females, android_fat_g, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, android_fat_g, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, android_non_fat_g, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, android_non_fat_g, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, android_fat_percent, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, android_fat_percent, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

# Gynoid region -> blood pressure
set.seed(1)
# systolic
boot_out <- select(females, gynoid_fat_g, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, gynoid_fat_g, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, gynoid_non_fat_g, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, gynoid_non_fat_g, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, gynoid_fat_percent, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, gynoid_fat_percent, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

# diastolic
boot_out <- select(females, gynoid_fat_g, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, gynoid_fat_g, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, gynoid_non_fat_g, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, gynoid_non_fat_g, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, gynoid_fat_percent, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, gynoid_fat_percent, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

# android fat is more dangerous than gynoid fat?
set.seed(1)
boot_out <- select(females, android_gynoid_fat_ratio, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, android_gynoid_fat_ratio, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, android_gynoid_non_fat_ratio, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, android_gynoid_non_fat_ratio, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, android_gynoid_fat_ratio, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, android_gynoid_fat_ratio, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, android_gynoid_non_fat_ratio, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, android_gynoid_non_fat_ratio, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

# Visceral, subcutaneous fat -> blood pressure
set.seed(1)
boot_out <- select(females, visceral_fat_g, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, visceral_fat_g, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, subcutaneous_fat_g, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, subcutaneous_fat_g, systolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, visceral_fat_g, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, visceral_fat_g, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

boot_out <- select(females, subcutaneous_fat_g, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))
boot_out <- select(males, subcutaneous_fat_g, diastolic) |>
  boot(boot_cor, R = 1000)
display_cor_conf_ints(boot.ci(boot_out, type = c("norm", "basic", "perc")))

# Interesting finding: two groups of children males
fat_data |>
  filter(gender == "Male", age < 20) |>
  ggplot(aes(x = visceral_fat_g / subcutaneous_fat_g)) +
  geom_freqpoly()

fat_data |>
  filter(gender == "Male", age < 20) |> 
  group_by(category = visceral_subcutaneous_ratio < 0.5) |>
  conf_int("systolic", mean, se_of_mean)

# Multiple hypothesis testing
all_p_values_adjusted <- p.adjust(all_p_values, method = "BH")
tibble(formatC(all_p_values_adjusted))
tibble(all_p_values_adjusted < 0.05)

