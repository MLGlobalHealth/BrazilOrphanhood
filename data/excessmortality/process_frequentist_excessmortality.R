
rm(list=ls())

library(tidyverse)

IN_DIR = "data/excessmortality/samples_frequentist/"
OUT_DIR = "data/excessmortality/"

load(paste0(IN_DIR, "04b_1000simulations_byUF_2020_FEMALES_wpop.rda"))
load(paste0(IN_DIR, "04b_1000simulations_byUF_2020_MALES_wpop.rda"))
load(paste0(IN_DIR, "04b_1000simulations_byUF_2021_FEMALES_wpop.rda"))
load(paste0(IN_DIR, "04b_1000simulations_byUF_2021_MALES_wpop.rda"))

df_all = dat_2020.f %>% rbind(dat_2020.m) %>% rbind(dat_2021.f) %>% rbind(dat_2021.m)

df_tidy = df_all %>%
  rename(sex=sexo) %>%
  mutate(sex=gsub("^f$", "female", sex),
         sex=gsub("^m$", "male", sex),
         age_group=gsub(" ", "", age_group),
         age_group=gsub("y.*", "", age_group),
         age_group=gsub(">70", "70+", age_group),
         excess_deaths=deaths-fitted_deaths)

write.csv(df_tidy, paste0(OUT_DIR, "excess_mortality_frequentist.csv"), row.names=FALSE)
