library(tidyverse)
library(nhanesA)


extract_data_by_year <- function(survey_year_symbol) {
  append_survey_year <- function(filename) {
    paste(filename, survey_year_symbol, sep = "_")
  }
  
  file <- append_survey_year("BPX")
  blood_pressure <- tibble(nhanes(file))
  cat(file, "done\n")
  blood_pressure <- blood_pressure |>
    mutate(
      seqn = SEQN,
      diastolic1 = BPXDI1,
      diastolic2 = BPXDI2,
      diastolic3 = BPXDI3,
      diastolic4 = BPXDI4,
      systolic1 = BPXSY1,
      systolic2 = BPXSY2,
      systolic3 = BPXSY3,
      systolic4 = BPXSY4,
      pulse = BPXPLS,
      .keep = "none"
    )
  
  file <- append_survey_year("BMX")
  body_measures <- tibble(nhanes(file))
  cat(file, "done\n")
  body_measures <- body_measures |>
    mutate(
      seqn = SEQN,
      weight_kg = BMXWT,
      height_cm = BMXHT,
      waist_cm = BMXWAIST,
      .keep = "none"
    )
  
  file <- append_survey_year("DXX")
  dxx <- tibble(nhanes(file))
  cat(file, "done\n")
  dxx <- dxx |>
    mutate(
      seqn = SEQN,
      lean_mass_g = DXDSTLI,
      fat_mass_g = DXDSTFAT,
      .keep = "none"
    )
  
  file <- append_survey_year("DXXAG")
  dxxag <- tibble(nhanes(file))
  cat(file, "done\n")
  dxxag <- dxxag |>
    mutate(
      seqn = SEQN,
      visceral_fat_g = DXXVFATM,
      subcutaneous_fat_g = DXXSATM,
      android_fat_g = DXXANFM,
      android_non_fat_g = DXXANLM,
      gynoid_fat_g = DXXGYFM,
      gynoid_non_fat_g = DXXGYLM,
      .keep = "none"
    )
 
  file <- append_survey_year("DEMO")
  demographics <- tibble(nhanes(file))
  cat(file, "done\n")
  demographics <- demographics |>
    mutate(
      seqn = SEQN,
      age = RIDAGEYR,
      gender = RIAGENDR,
      race = RIDRETH3,
      .keep = "none"
    )

  tables <- list(
    blood_pressure, 
    body_measures,
    dxx,
    dxxag,
    demographics
  )
  final <- reduce(tables, .f = left_join)
  return(final)
}

# The following might take a while
data_G <- extract_data_by_year("G")
data_H <- extract_data_by_year("H")
data_I <- extract_data_by_year("I")
data_J <- extract_data_by_year("J")
data <- bind_rows(data_G, data_H, data_I, data_J)
write_csv(data, "data/data_lab3.csv")
