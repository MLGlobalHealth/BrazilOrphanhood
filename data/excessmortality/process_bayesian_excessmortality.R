
rm(list=ls())

library(tidyverse)

IN_DIR = "data/excessmortality/samples_bayesian/"
OUT_DIR = "data/excessmortality/"

load(paste0(IN_DIR, "05_female_2020 - Bayesian 1000 simulation results_FINAL_ANN.rds"))
load(paste0(IN_DIR, "05_female_2021 - Bayesian 1000 simulation results_FINAL_ANN.rds"))
load(paste0(IN_DIR, "05_male_2020 - Bayesian 1000 simulation results_FINAL_ANN.rds"))
load(paste0(IN_DIR, "05_male_2021 - Bayesian 1000 simulation results_FINAL_ANN.rds"))

df_all = df.data.2020.f.ppred_final_ANNUAL %>% rbind(df.data.2020.m.ppred_final_ANNUAL) %>% rbind(df.data.2021.f.ppred_final_ANNUAL) %>% rbind(df.data.2021.m.ppred_final_ANNUAL)

df_tidy = df_all %>%
  rename(sex=sexo) %>%
  mutate(sex=gsub("^f$", "female", sex),
         sex=gsub("^m$", "male", sex),
         age_group=gsub(" ", "", age_group),
         age_group=gsub("y.*", "", age_group),
         age_group=gsub(">70", "70+", age_group)
         )

write.csv(df_tidy, paste0(OUT_DIR, "excess_mortality_bayesian.csv"), row.names=FALSE)
