
rm(list=ls())

library(tidyverse)
library(zoo)
library(cowplot)

source("R/calculateMonthlyOrphanhood.R")

df = read.csv("data/civilreg/orfaos_clean.csv") %>%
  mutate(double_orphanhood = double_orphanhood=="SIM",
         child_years = as.numeric(case_when(child_years=="-" ~ "0",
                                            TRUE ~ child_years)),
         date_of_death = as.Date(date_of_death, origin = "1899-12-30")) %>%
  filter(child_years<=6)

uf_lbls = read.csv("data/UF_labels.csv")

df_popn = read.csv("data/population/popests_simple_2010_2020.csv") %>%
  filter(age_group %in% paste0(seq(0, 6)), year==2020, !is.na(uf), sex!="both") %>% # Filter out relevant counts, ...
  group_by(uf) %>% # ...
  summarise(n_children=sum(n_pop)) %>% #..., group by federative unit
  left_join(uf_lbls, by="uf") # append the federative unit labels



# ------------------------------------------------------------------------------
# ----------------------- Print descriptive data -------------------------------
# ------------------------------------------------------------------------------

# By cause of death
print(df %>% group_by(illness_of_death) %>% summarise(n=n()))

# By sex of parent
print(df %>% filter(!double_orphanhood, illness_of_death=="COVID") %>% group_by(parent_sex) %>% summarise(n=n()) %>% rbind(data.frame(parent_sex="both", n=sum(df$double_orphanhood[df$illness_of_death=="COVID"]))) %>% mutate(p=n/sum(n)))

# By state of death
df_stateofdeath = df %>% group_by(state_of_death) %>% summarise(n=n()) %>% mutate(p=n/sum(n))
print(df %>% group_by(state_of_death) %>% summarise(n=n()) %>% mutate(p=n/sum(n)))

# By age of child
print(df %>% group_by(child_years) %>% summarise(n=n()) %>% mutate(p=n/sum(n)))

# Between April 2020 and August 2021
inwindow = df %>% filter(illness_of_death=="COVID") %>% filter(date_of_death >= as.Date("2020-04-01"), date_of_death <= as.Date("2021-08-31"))


# ------------------------------------------------------------------------------
# ----------------------- Compare with models ----------------------------------
# ------------------------------------------------------------------------------

total_orphanhood_full_model = readRDS("samples/orphanhood_age_group_child_age_sex_uf_excess.RDS") %>%
  group_by(age_group, uf, sex, child_age) %>%
  summarise(orphanhood=mean(orphanhood)) %>%
  ungroup() %>%
  summarise(total=sum(orphanhood)) %>%
  pull()

df_orphanhood_monthly = calculateMonthlyOrphanhood(by_child_age=TRUE)
scaling_factor = total_orphanhood_full_model / sum(df_orphanhood_monthly$orphanhood)

df_orphanhood_monthly$orphanhood = df_orphanhood_monthly$orphanhood * scaling_factor

df_estimates = df_orphanhood_monthly %>%
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  filter(child_age<=6, date >= as.yearmon("2020-04", "%Y-%m"), date <= as.yearmon("2021-08", "%Y-%m")) %>%
  group_by(sex) %>%
  summarise(n = sum(orphanhood))

total = sum(df_estimates$n)

