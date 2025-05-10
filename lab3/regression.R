#----------------------------------------------------------------------
# Purpose: Model relationship between dependent and independent variables via linear (in coefficients) regression
#----------------------------------------------------------------------

library(tidyverse)
library(GGally)
library(here)
library(lmtest)
library(sandwich)
library(car)
library(stargazer)

source(here::here("lab3/preprocess.R"))

# How amount of visceral fat affects systolic blood pressure?
display_models <- function(..., output_type = "text") {
  models = list(...)
  models_se = map(
    models, 
    \(model) coeftest(model, vcov. = vcovHC(model, type = "HC1"))[, 2]
  )
  
  stargazer(
    models, type = output_type, 
    se = models_se,
    keep.stat = c("n", "adj.rsq")
  )
}

# systolic ~ log(fat) --------------------------------------------------
# Only fat
model0 <- lm(systolic ~ log(visceral_fat_g), data)

# With control variables, show results of each step
model_controls1 <- lm(systolic ~ log(visceral_fat_g) + age, data)

model_controls2 <- lm(systolic ~ log(visceral_fat_g) + age + gender, data)

model_controls3 <- lm(systolic ~ log(visceral_fat_g) + age + gender + race, data)

model_controls4 <- lm(systolic ~ log(visceral_fat_g) + age + gender + race + log(lean_mass_g) + height_cm, data)

model_controls5 <- lm(systolic ~ log(visceral_fat_g) + age + gender + race + log(lean_mass_g) + height_cm + log(subcutaneous_fat_g) + log(gynoid_fat_g), data)

display_models(model0, model_controls1, model_controls3, model_controls4, model_controls5, output_type = "latex")

# With interaction terms
model_interactions1 <- lm(systolic ~ log(visceral_fat_g) + age + gender + race + log(lean_mass_g) + height_cm + log(subcutaneous_fat_g) + log(gynoid_fat_g) + log(visceral_fat_g):log(lean_mass_g) + log(visceral_fat_g):height_cm, data)

model_interactions2 <- lm(systolic ~ log(visceral_fat_g) + age + gender + race + log(lean_mass_g) + height_cm + log(subcutaneous_fat_g) + log(gynoid_fat_g) + log(visceral_fat_g):log(lean_mass_g) + log(visceral_fat_g):height_cm + log(visceral_fat_g):age, data)

model_interactions3 <- lm(
  systolic ~ 
    log(visceral_fat_g) + age + gender + race + log(lean_mass_g) + height_cm + log(subcutaneous_fat_g) + log(gynoid_fat_g) + 
    log(visceral_fat_g):log(lean_mass_g) + log(visceral_fat_g):height_cm + log(visceral_fat_g):age +
    log(visceral_fat_g):gender + age:gender + log(lean_mass_g):gender + height_cm:gender + log(subcutaneous_fat_g):gender + log(gynoid_fat_g):gender, data)

model_interactions4 <- lm(
  systolic ~ 
    log(visceral_fat_g) + age + gender + race + log(lean_mass_g) + height_cm + log(subcutaneous_fat_g) + log(gynoid_fat_g) + 
    log(visceral_fat_g):log(lean_mass_g) + log(visceral_fat_g):height_cm + log(visceral_fat_g):age +
    log(visceral_fat_g):gender + age:gender + log(lean_mass_g):gender + height_cm:gender + log(subcutaneous_fat_g):gender + log(gynoid_fat_g):gender +
    age:is_black, data)

display_models(model_controls5, model_interactions1, model_interactions2, model_interactions4, output_type = "latex")

# With polynomials
model_polynomials1 <- lm(
  systolic ~ 
    log(visceral_fat_g) + age + I(age^2) + I(age^3) + gender + race + log(lean_mass_g) + height_cm + log(subcutaneous_fat_g) + log(gynoid_fat_g) + 
    log(visceral_fat_g):log(lean_mass_g) + log(visceral_fat_g):height_cm + log(visceral_fat_g):(age + I(age^2)) +
    log(visceral_fat_g):gender + (age + I(age^2) + I(age^3)):gender + log(lean_mass_g):gender + height_cm:gender + log(subcutaneous_fat_g):gender + log(gynoid_fat_g):gender +
    (age + I(age^2)):is_black, data)
  
model_polynomials2 <- lm(
  systolic ~ 
    log(visceral_fat_g) + age + I(age^2) + I(age^3) + I(age^4) + gender + race + log(lean_mass_g) + height_cm + log(subcutaneous_fat_g) + log(gynoid_fat_g) + 
    log(visceral_fat_g):log(lean_mass_g) + log(visceral_fat_g):height_cm + log(visceral_fat_g):(age + I(age^2) + I(age^3)) +
    log(visceral_fat_g):gender + (age + I(age^2) + I(age^3) + I(age^4)):gender + log(lean_mass_g):gender + height_cm:gender + log(subcutaneous_fat_g):gender + log(gynoid_fat_g):gender +
    (age + I(age^2) + I(age^3) + I(age^4)):is_black, data)

display_models(model_interactions4, model_polynomials1, model_polynomials2, output_type = "latex")

# Interpretation
visceral_fat_deriv <- function(model, lean_mass_g = 1, height_cm = 0, age = 0, gender = "Female") {
  coef <- model$coefficients
  get_coef <- \(coef_name) {
    if (coef_name %in% names(coef)) {
      return(coef[[coef_name]])
    } else {
      return(0)
    }
  }
  
  get_coef("log(visceral_fat_g)") +
    get_coef("log(visceral_fat_g):log(lean_mass_g)") * log(lean_mass_g) +
    get_coef("log(visceral_fat_g):height_cm") * height_cm +
    get_coef("log(visceral_fat_g):age") * age +
    get_coef("log(visceral_fat_g):I(age^2)") * age^2 +
    get_coef("log(visceral_fat_g):I(age^3)") * age^3 +
    get_coef("log(visceral_fat_g):genderMale") * (gender == "Male")
}
plot_visceral_fat_deriv <- function(model, data) {
  data |>
    mutate(
      `d(systolic)/d(log(visceral_fat))` = visceral_fat_deriv(model, lean_mass_g, height_cm, age, gender)
    ) |>
    ggplot(aes(x = `d(systolic)/d(log(visceral_fat))`, color = gender)) +
    geom_freqpoly() +
    theme(text = element_text(size = 20))
}

plot_correlations <- function(data) {
  data |>
    mutate(
      visc = log(visceral_fat_g),
      subc = log(subcutaneous_fat_g),
      gynoid = log(gynoid_fat_g),
      mass = log(lean_mass_g),
      height = height_cm
    ) |>
    mutate(
      `visc*mass` = visc * mass,
      `visc*height` = visc * height,
      `visc*age` = visc * age,
      `visc*age2` = visc * age^2,
      `visc*gender` = visc * (gender == "Male"),
      `age*gender` = age * (gender == "Male"),
      `age2*gender` = age^2 * (gender == "Male"),
      `age3*gender` = age^3 * (gender == "Male"),
      `mass*gender` = mass * (gender == "Male"),
      `height*gender` = height * (gender == "Male"),
      `subc*gender` = subc * (gender == "Male"),
      `gynoid*gender` = gynoid * (gender == "Male"),
      `age:is_black` = age * is_black,
      age2 = age^2,
      age3 = age^3,
    ) |>
    select(
      visc, age,
      mass, height, 
      subc, gynoid,
      age2, age3,
      `visc*mass`, `visc*height`, 
      `visc*age`, `visc*age2`, `visc*gender`, 
      `age*gender`, `age2*gender`, `age3*gender`,
      `mass*gender`, `height*gender`, `subc*gender`, `gynoid*gender`, 
      `age:is_black`
    ) |>
    ggcorr(method = c("pairwise", "pearson"), label = TRUE)
}
data |> plot_correlations()

# high correlation example
data |>
  mutate(
    visc = log(visceral_fat_g),
    subc = log(subcutaneous_fat_g),
    gynoid = log(gynoid_fat_g),
    mass = log(lean_mass_g),
    height = height_cm
  ) |>
  mutate(
    `visc*mass` = visc * mass,
    `visc*height` = visc * height,
    `visc*age` = visc * age,
    `visc*age2` = visc * age^2,
    `visc*gender` = visc * (gender == "Male"),
    `age*gender` = age * (gender == "Male"),
    `age2*gender` = age^2 * (gender == "Male"),
    `age3*gender` = age^3 * (gender == "Male"),
    `mass*gender` = mass * (gender == "Male"),
    `height*gender` = height * (gender == "Male"),
    `subc*gender` = subc * (gender == "Male"),
    `gynoid*gender` = gynoid * (gender == "Male"),
    `age:is_black` = age * is_black,
    age2 = age^2,
    age3 = age^3,
  ) |>
  ggplot(aes(`visc*gender`, `mass*gender`, color=gender)) +
  geom_point() +
  theme(text = element_text(size = 20))


display_models(model_polynomials1, output_type = "latex")

plot_visceral_fat_deriv(model_polynomials1, data)
data |>
  mutate(
    visceral_fat_effect = visceral_fat_deriv(model_polynomials1, lean_mass_g, height_cm, age, gender)
  ) |>
  group_by(gender) |>
  summarise(median(visceral_fat_effect))

# test coef of age
linearHypothesis(model_polynomials1, 
                 c("log(visceral_fat_g):age = 0", "log(visceral_fat_g):I(age^2) = 0"),
                 vcov = vcovHC(model_polynomials1, type = "HC1"))

coef <- model_polynomials1$coefficients
x <- seq(min(data$age), max(data$age), length.out = 500)
y <- coef[["log(visceral_fat_g):age"]] * x + coef[["log(visceral_fat_g):I(age^2)"]] * x^2 
as.data.frame(list(age = x, coef = y)) |>
  ggplot(aes(age, coef)) +
  geom_path() +
  ylab("0.279*age - 0.004*age^2") + 
  theme(text = element_text(size = 20))

