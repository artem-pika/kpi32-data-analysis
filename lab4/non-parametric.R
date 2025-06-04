#----------------------------------------------------------------------
# Purpose: non parametric regression
#----------------------------------------------------------------------

library(tidyverse)
library(here)
library(np)
library(modelsummary)

source(here::here("lab4/preprocess.R"))

set.seed(1)

train_test_split <- function(data, test_size = 0.5) {
  n = dim(data)[1]
  test_indices = sample.int(n, size = as.integer(test_size * n))
  return(
    list(
      train_data = data[-test_indices, ],
      test_data = data[test_indices, ]
    )
  )
}

data_split <- train_test_split(data, test_size = 0.5)
train_data <- data_split$train_data
test_data <- data_split$test_data
dim(train_data)
dim(test_data)

fit_model <- function(formula, regtype) {
  # Find bandwidths on train data
  bws <- npregbw(
    formula, 
    data = train_data, 
    regtype = regtype,
    nmulti = 1
  )$bw
  
  # Fit model on test data with bandwidths from train data
  model <- npreg(
    formula,
    data = test_data,
    bws = bws,
    regtype = regtype
  )
  return(model)
}

plot_preds <- function(prev_plot, preds, color) {
  y <- preds$fit
  se <- preds$se.fit

  prev_plot + 
    geom_path(aes(y = y, color = color)) + 
    geom_ribbon(aes(ymin = y - se * qnorm(0.975), ymax = y + se * qnorm(0.975), fill = color), alpha = 0.5)
}

plot_error_bars <- function(prev_plot, preds, color) {
  y <- preds$fit
  se <- preds$se.fit
  
  prev_plot + 
    geom_errorbar(aes(ymin = y - qnorm(0.975) * se, ymax = y + qnorm(0.975) * se, color = color), width = 0.1)
}

# systolic ~ visceral fat
model0_lc <- fit_model(systolic ~ visceral_fat_g, 'lc')
print('fitted model0_lc')
model0_ll <- fit_model(systolic ~ visceral_fat_g, 'll')
print('fitted model0_ll')

X_pred <- data.frame(
  visceral_fat_g = seq(quantile(data$visceral_fat_g, 0.02), quantile(data$visceral_fat_g, 0.98), length.out = 200)
)
preds_lc <- predict(model0_lc, newdata = X_pred, se.fit = TRUE)
preds_ll <- predict(model0_ll, newdata = X_pred, se.fit = TRUE)
ggplot(mapping = aes(X_pred[["visceral_fat_g"]])) |> 
  plot_preds(preds_lc, color = 'lc') |>
  plot_preds(preds_ll, color = 'll') +
  scale_color_manual(name = "Type", values = c('#F8766D', '#619CFF')) + 
  scale_fill_manual(name = "Type", values = c('#F8766D', '#619CFF')) +
  labs(x = "visceral fat", y = "systolic") +
  theme(text = element_text(size = 16))

# systolic ~ visceral fat, age
model1_lc <- fit_model(systolic ~ visceral_fat_g + age, "lc")
print('fitted model1_lc')
model1_ll <- fit_model(systolic ~ visceral_fat_g + age, "ll")
print('fitted model1_ll')

# visceral fat
X_pred <- data.frame(
  visceral_fat_g = seq(quantile(data$visceral_fat_g, 0.02), quantile(data$visceral_fat_g, 0.98), length.out = 200),
  age = median(data$age)
)
preds_lc <- predict(model1_lc, newdata = X_pred, se.fit = TRUE)
preds_ll <- predict(model1_ll, newdata = X_pred, se.fit = TRUE)
ggplot(mapping = aes(X_pred[["visceral_fat_g"]])) |> 
  plot_preds(preds_lc, color = 'lc') |>
  plot_preds(preds_ll, color = 'll') +
  scale_color_manual(name = "Type", values = c('#F8766D', '#619CFF')) + 
  scale_fill_manual(name = "Type", values = c('#F8766D', '#619CFF')) +
  labs(x = "visceral fat", y = "systolic", subtitle = paste("age = ", X_pred[["age"]][1], sep = "")) + 
  theme(text = element_text(size = 16))
  
# age
X_pred <- data.frame(
  age = seq(min(data$age), max(data$age), length.out = 200),
  visceral_fat_g = median(data$visceral_fat_g)
)
preds_lc <- predict(model1_lc, newdata = X_pred, se.fit = TRUE)
preds_ll <- predict(model1_ll, newdata = X_pred, se.fit = TRUE)
ggplot(mapping = aes(X_pred[["age"]])) |> 
  plot_preds(preds_lc, color = 'lc') |>
  plot_preds(preds_ll, color = 'll') +
  scale_color_manual(name = "Type", values = c('#F8766D', '#619CFF')) + 
  scale_fill_manual(name = "Type", values = c('#F8766D', '#619CFF')) +
  labs(x = "age", y = "systolic", subtitle = paste("visceral fat = ", X_pred[["visceral_fat_g"]][1], sep = "")) + 
  theme(text = element_text(size = 16))

# systolic ~ visceral fat, age, linear(gender, race, log(lean mass))
# Find coefficients of the linear part
model_pl <- npplreg(
  systolic ~ genderMale + `raceNon-Hispanic Black` + log(lean_mass_g) | visceral_fat_g + age,
  data = train_data,
  regtype = 'lc',
  nmulti = 1
)
print('fitted model_pl')

# compare to linear model
model_linear <- lm(
  systolic ~ genderMale + `raceNon-Hispanic Black` + log(lean_mass_g) +
    log(visceral_fat_g) + age + I(age^2) + I(age^3) + log(visceral_fat_g):age + log(visceral_fat_g):I(age^2), 
  data = data
)

model_pl_results <- list(
  tidy = tibble(
    term = names(model_pl$xcoef),
    estimate = model_pl$xcoef,
    std.error = model_pl$xcoeferr,
    p.value = 2 * pnorm(-abs(estimate / std.error))
  ), 
  glance = data.frame(Num.Obs. = model_pl$nobs)
)
class(model_pl_results) <- "modelsummary_list"
modelsummary(
  list(model_linear, model_pl_results),
  stars = TRUE, gof_omit = "^(?!Num.Obs.)", output = "latex_tabular"
) 

# remove linear part
coef <- model_pl$xcoef
train_data_sub <- train_data |>
  mutate(
    systolic = systolic - 
      genderMale * coef[["genderMale"]] - 
      `raceNon-Hispanic Black` * coef[["raceNon-Hispanic Black"]] - 
      log(lean_mass_g) * coef[["log(lean_mass_g)"]]
  )
test_data_sub <- test_data |>
  mutate(
    systolic = systolic - 
      genderMale * coef[["genderMale"]] - 
      `raceNon-Hispanic Black` * coef[["raceNon-Hispanic Black"]] - 
      log(lean_mass_g) * coef[["log(lean_mass_g)"]]
  )

linear_part <- 1 * coef[["genderMale"]] + median(log(data$lean_mass_g)) * coef[["log(lean_mass_g)"]]

# Fit "Y - linear_part = nonparametric_part + e"
bws <- npregbw(
  systolic ~ visceral_fat_g + age, 
  data = train_data_sub, 
  regtype = "ll",
  nmulti = 1
)$bw
model_np <- npreg(
  systolic ~ visceral_fat_g + age,
  data = test_data_sub,
  bws = bws,
  regtype = "ll"
)
print('fitted model_np')

# visceral fat
X_pred <- data.frame(
  visceral_fat_g = seq(quantile(data$visceral_fat_g, 0.02), quantile(data$visceral_fat_g, 0.98), length.out = 200),
  age = median(data$age)
)
preds <- predict(model_np, newdata = X_pred, se.fit = TRUE)
preds$fit <- preds$fit + linear_part
ggplot(mapping = aes(X_pred[["visceral_fat_g"]])) |> 
  plot_preds(preds, color = "") +
  labs(x = "visceral fat", y = "systolic") + 
  scale_color_manual(values = c("#619CFF")) + 
  scale_fill_manual(values = c("#619CFF")) +
  labs(x = "visceral fat", y = "systolic", subtitle = paste("age = ", X_pred[["age"]][1], ", gender = Male, race = non-black, lean mass = ", median(data$lean_mass_g), sep = "")) + 
  theme(legend.position = "none", text = element_text(size = 16))

# age
X_pred <- data.frame(
  age = seq(min(data$age), max(data$age), length.out = 200),
  visceral_fat_g = median(data$visceral_fat_g)
)
preds <- predict(model_np, newdata = X_pred, se.fit = TRUE)
preds$fit <- preds$fit + linear_part
ggplot(mapping = aes(X_pred[["age"]])) |> 
  plot_preds(preds, color = "") +
  scale_color_manual(values = c("#619CFF")) + 
  scale_fill_manual(values = c("#619CFF")) +
  labs(x = "age", y = "systolic", subtitle = paste("visceral fat = ", X_pred[["visceral_fat_g"]][1], ", gender = Male, race = non-black, lean mass = ", median(data$lean_mass_g), ", race = non-black", sep = "")) + 
  theme(legend.position = "none", text = element_text(size = 16))
