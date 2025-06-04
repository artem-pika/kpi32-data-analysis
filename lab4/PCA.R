#----------------------------------------------------------------------
# Purpose: PCA
#----------------------------------------------------------------------

library(tidyverse)
library(here)
library(FactoMineR)
library(factoextra)

source(here::here("lab4/preprocess.R"))

names(data)
data_for_pca <- data |>
  mutate(
    `log(visceral_fat_g)` = log(visceral_fat_g),
    `log(lean_mass_g)` = log(lean_mass_g),
    `log(subcutaneous_fat_g)` = log(subcutaneous_fat_g),
    `log(gynoid_fat_g)` = log(gynoid_fat_g)
  ) |>
  select(
    systolic, blood_pressure_category, `log(visceral_fat_g)`, age, genderMale, 
    `raceMexican American`, `raceNon-Hispanic Asian`, `raceNon-Hispanic Black`, `raceNon-Hispanic White`, `raceOther Hispanic`,
    height_cm, `log(lean_mass_g)`, `log(subcutaneous_fat_g)`, `log(gynoid_fat_g)`
  )
dim(data_for_pca)[2] - 2

pca <- PCA(data_for_pca, graph = FALSE, quanti.sup = c(1), quali.sup = c(2))
fviz_screeplot(pca, addlabels = TRUE)
30.8 + 15.4 + 11.5 + 10.7 + 10 + 9.4
  
fviz_pca_var(pca, axes = c(1, 2), repel = TRUE)
fviz_pca_var(pca, axes = c(3, 5), repel = TRUE)

fviz_pca_biplot(pca, axes = c(1, 2), geom = "point", alpha.ind = 1, col.ind = data$gender)
fviz_pca_biplot(pca, axes = c(1, 2), geom = "point", alpha.ind = 1, col.ind = data_for_pca$blood_pressure_category)
fviz_pca_biplot(pca, axes = c(3, 5), geom = "point", alpha.ind = 1, col.ind = data$race)
fviz_pca_biplot(pca, axes = c(3, 5), geom = "point", alpha.ind = 1, col.ind = data$gender)

