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
      waist_cm = BMXWAIST,
      .keep = "none"
    )
  
  file <- append_survey_year("DXXAG")
  dxxag <- tibble(nhanes(file))
  cat(file, "done\n")
  dxxag <- dxxag |>
    mutate(
      seqn = SEQN,
      subcutaneous_fat_g = DXXSATM,
      visceral_fat_g = DXXVFATM,
      android_g = DXXANTOM,
      android_fat_g = DXXANFM,
      android_non_fat_g = DXXANLM,
      gynoid_g = DXXGYTOM,
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
      survey_year = SDDSRVYR,
      age = RIDAGEYR,
      gender = RIAGENDR,
      race = RIDRETH3,
      education = if_else(DMDEDUC2 %in% c("Don't Know", "Refused"), NA, DMDEDUC2),
      .keep = "none"
    )
  
  file <- append_survey_year("INQ")
  income <- tibble(nhanes(file))
  cat(file, "done\n")
  income <- income |>
    mutate(
      seqn = SEQN,
      poverty_index = INDFMMPI,
      .keep = "none"
    )
  
  file <- append_survey_year("PAQ")
  physical_activity <- tibble(nhanes(file))
  cat(file, "done\n")
  physical_activity <- physical_activity |>
    mutate(
      seqn = SEQN,
      vigorous_phys_activity = if_else(PAQ650 %in% c("Don't know", "Refused"), NA, PAQ650),
      vigorous_phys_activity_days = if_else(PAQ655 %in% c(77, 99), NA, PAQ655),
      moderate_phys_activity = if_else(PAQ665 %in% c("Don't know", "Refused"), NA, PAQ665),
      moderate_phys_activity_days = if_else(PAQ670 %in% c(77, 99), NA, PAQ670),
      .keep = "none"
    )
  
  file <- append_survey_year("SLQ")
  sleep <- tibble(nhanes(file))
  cat(file, "done\n")
  if ("SLD012" %in% names(sleep)) {
    sleep <- sleep |>
      mutate(
        seqn = SEQN,
        sleep = SLD012,
        .keep = "none"
      )
  } else {
    sleep <- sleep |>
      mutate(
        seqn = SEQN,
        sleep = if_else(SLD010H %in% c(77, 99), NA, SLD010H),
        .keep = "none"
      )
  }

  file <- append_survey_year("SMQ")
  smoking <- tibble(nhanes(file))
  cat(file, "done\n")
  smoking <- smoking |>
    mutate(
      seqn = SEQN,
      smoking_now = if_else(SMQ040 %in% c("Refused", "Don't know"), NA, SMQ040),
      smoked_100_cigs = if_else(SMQ020 %in% c("Refused", "Don't know"), NA, SMQ020),
      .keep = "none"
    )
  
  file <- append_survey_year("DBQ")
  diet_quality <- tibble(nhanes(file))
  cat(file, "done\n")
  diet_quality <- diet_quality |>
    mutate(
      seqn = SEQN,
      diet_quality = if_else(DBQ700 %in% c("Refused", "Don't know"), NA, DBQ700),
      .keep = "none"
    )
  
  tables <- list(
    blood_pressure, 
    body_measures,
    dxxag,
    demographics,
    income,
    physical_activity,
    sleep,
    smoking,
    diet_quality
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
write_csv(data, "data/data.csv")
