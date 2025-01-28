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
mortality = "excess" # Choose excess or allcause
methods = c("any", "multiple", "noadults")
fnames = paste0("samples/coresorphanhood_", methods, "_age_group_sex_uf_", mortality, ".RDS")

df = foreach(ii=seq(1, length(fnames)), .combine=rbind) %do% {
  readRDS(fnames[ii]) %>% mutate(method=methods[ii])
}



# ------------------------------------------------------------------------------
# -------------------------- Text-based results --------------------------------
# ------------------------------------------------------------------------------

# National level results:
df_results = df %>% filter(method=="any") %>% summariseByGroup(group_vars=c())
print(paste0("Loss of any coresiding elderly = ", signif(df_results$mean, 3), " (", signif(df_results$lower, 3), ", ", signif(df_results$upper, 3), ")"))

df_results = df %>% filter(method=="multiple") %>% summariseByGroup(group_vars=c())
print(paste0("Loss of multiple coresiding elderly = ", signif(df_results$mean, 3), " (", signif(df_results$lower, 3), ", ", signif(df_results$upper, 3), ")"))

df_results = df %>% filter(method=="noadults") %>% summariseByGroup(group_vars=c())
print(paste0("Loss of direct elderly caregiver = ", signif(df_results$mean, 3), " (", signif(df_results$lower, 3), ", ", signif(df_results$upper, 3), ")"))


# Regional results (non-standardised):
df_results = df %>% filter(method=="any") %>%
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
df_results = df %>% filter(method=="any") %>%
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








