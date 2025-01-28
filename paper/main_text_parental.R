rm(list=ls())

library(tidyverse)
library(zoo)
library(clipr)

source("R/supportFunctions.R")
source("R/calculateMonthlyOrphanhood.R")

uf_lbls = read.csv("data/UF_labels.csv")

# Load population data
df_popn = read.csv("data/population/popests_simple_2010_2020.csv") %>%
  filter(age_group %in% paste0(seq(0, 17)), year==2020, !is.na(uf), sex!="both") %>%
  group_by(uf) %>%
  summarise(n_children=sum(n_pop)) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  select(Region, n_children)

# Append the national total number of children as a new row
df_popn = df_popn %>% rbind(data.frame(Region="Total", n_children=sum(df_popn$n_children)))

# Load samples
COVID19_ASSOCIATED = readRDS("samples/orphanhood_age_group_sex_uf_excess.RDS") # Load samples of COVID-19-associated orphanhood
ALLCAUSE = readRDS("samples/orphanhood_age_group_sex_uf_allcause.RDS") # Load samples of all-cause orphanhood

# Choose which set of samples to work with
df = COVID19_ASSOCIATED # Use "COVID19_ASSOCIATED" or "ALLCAUSE" (as above)



# ------------------------------------------------------------------------------
# -------------------------- Text-based results --------------------------------
# ------------------------------------------------------------------------------

# National level results:
df_results = df %>% summariseByGroup(group_vars=c())
print(paste0("Total orphanhood = ", signif(df_results$mean, 3), " (", signif(df_results$lower, 3), ", ", signif(df_results$upper, 3), ")"))
total = df_results$mean

df_results = df %>% summariseByGroup(c("sex")) %>% tidydf()
df_print = df_results %>% filter(sex=="Male")
print(paste0("From loss of father = ", signif(df_print$mean, 3), " (", signif(df_print$lower, 3), ", ", signif(df_print$upper, 3), ")"))
male = df_results %>% filter(sex=="Male") %>% pull(mean)
df_print = df_results %>% filter(sex=="Female")
print(paste0("From loss of mother = ", signif(df_print$mean, 3), " (", signif(df_print$lower, 3), ", ", signif(df_print$upper, 3), ")"))
female = df_results %>% filter(sex=="Female") %>% pull(mean)
df_print = df_results %>% filter(sex=="Both")
print(paste0("From loss of both parents = ", signif(df_print$mean, 3), " (", signif(df_print$lower, 3), ", ", signif(df_print$upper, 3), ")"))
both = df_results %>% filter(sex=="Both") %>% pull(mean)

# Regional results (non-standardised):
df_results = df %>%
  summariseByGroup(group_vars=c("uf")) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  ungroup() %>%
  arrange(desc(mean))

print("The three worst-affected regions (in absolute terms) were:")
for (ii in seq(1, 3)) {
  print(paste0(df_results$Region[ii],", with a total of ", signif(df_results$mean[ii], 4), " (", signif(quantile(df_results$lower[ii], 0.025), 4), ", ", signif(quantile(df_results$upper[ii], 0.975), 4), ")"))
}

# Regional results (standardised):
df_results = df %>%
  summariseByGroup(group_vars=c("uf")) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  ungroup() %>%
  left_join(df_popn, by="Region") %>%
  mutate(mean=1000*mean/n_children, lower=1000*lower/n_children, upper=1000*upper/n_children) %>%
  arrange(desc(mean))

print("The three worst-affected regions (per 1000 children) were:")
for (ii in seq(1, 3)) {
  print(paste0(df_results$Region[ii],", with a total of ", round(df_results$mean[ii], digits=1), " (", round(quantile(df_results$lower[ii], 0.025), digits=1), ", ", round(quantile(df_results$upper[ii], 0.975), digits=1), ")"))
}








