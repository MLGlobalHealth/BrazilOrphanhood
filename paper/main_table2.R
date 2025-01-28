
rm(list=ls())

library(tidyverse)
library(zoo)
library(geobr)
library(viridis)
library(cowplot)
library(clipr)

source("R/supportFunctions.R")

uf_lbls = read.csv("data/UF_labels.csv")

# Load population data
df_popn = read.csv("data/population/popests_simple_2010_2020.csv") %>%
  filter(age_group %in% paste0(seq(0, 17)), year==2020, !is.na(uf), sex!="both") %>%
  group_by(uf) %>%
  summarise(n_children=sum(n_pop)) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  select(Region, uf, n_children)

# Append the national total number of children as a new row
df_popn = df_popn %>% rbind(data.frame(Region="Total", uf=NA, n_children=sum(df_popn$n_children)))

# Load samples
df_parental = readRDS("samples/orphanhood_age_group_sex_uf_excess.RDS")
df_elderly = readRDS("samples/coresorphanhood_any_age_group_sex_uf_excess.RDS") 
df_both = readRDS(paste0("samples/parentalandcoresorphanhood_excess.RDS")) %>%
  left_join(df_popn, by="uf") %>%
  mutate(orphanhood=p_orphanhood*n_children) %>%
  select(uf, orphanhood, iter)

# ------------------------------------------------------------------------------
# --------------------------- Process results ----------------------------------
# ------------------------------------------------------------------------------

# Process parental orphanhood results by region
df_parental_tmp_a = df_parental %>%
  summariseByGroup(group_vars=c("uf")) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  ungroup() %>%
  select(-uf, -uf_code)

# Process parental orphanhood results nationally
df_parental_tmp_b = df_parental %>%
  summariseByGroup(group_vars=c()) %>%
  mutate(Region="Total")

# Combine the results and calculate standardised numbers
df_parental_results = rbind(df_parental_tmp_a, df_parental_tmp_b) %>%
  left_join(df_popn, by="Region") %>%
  mutate(mean_st = 1000*mean/n_children, lower_st = 1000*lower/n_children, upper_st = 1000*upper/n_children)

# Tidy up
rm(df_parental_tmp_a, df_parental_tmp_b)


# Process elderly orphanhood results by region
df_elderly_tmp_a = df_elderly %>%
  summariseByGroup(group_vars=c("uf")) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  ungroup() %>%
  select(-uf, -uf_code)

# Process elderly orphanhood results nationally
df_elderly_tmp_b = df_elderly %>%
  summariseByGroup(group_vars=c()) %>%
  mutate(Region="Total")

# Combine the results and calculate standardised numbers
df_elderly_results = rbind(df_elderly_tmp_a, df_elderly_tmp_b) %>%
  left_join(df_popn, by="Region") %>%
  mutate(mean_st = 1000*mean/n_children, lower_st = 1000*lower/n_children, upper_st = 1000*upper/n_children)

# Tidy up
rm(df_elderly_tmp_a, df_elderly_tmp_b)




# Process combined results
df_both_stub = df_both %>%
  rename(both_orphanhood = orphanhood) %>%
  left_join(df_parental %>% select(uf, orphanhood, iter) %>% group_by(uf, iter) %>% summarise(parental_orphanhood=sum(orphanhood)), by=c("uf", "iter")) %>%
  left_join(df_elderly %>% select(uf, orphanhood, iter) %>% group_by(uf, iter) %>% summarise(elderly_orphanhood=sum(orphanhood)), by=c("uf", "iter")) %>%
  mutate(orphanhood = parental_orphanhood + elderly_orphanhood - both_orphanhood)

df_both_tmp_a = df_both_stub %>%
  summariseByGroup(group_vars=c("uf")) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  ungroup() %>%
  select(-uf, -uf_code)

df_both_tmp_b = df_both_stub %>%
  summariseByGroup(group_vars=c()) %>%
  mutate(Region="Total")

df_both_results = rbind(df_both_tmp_a, df_both_tmp_b) %>%
  left_join(df_popn, by="Region") %>%
  mutate(mean_st = 1000*mean/n_children, lower_st = 1000*lower/n_children, upper_st = 1000*upper/n_children)



# ------------------------------------------------------------------------------
# ---------------------------- Format results ----------------------------------
# ------------------------------------------------------------------------------

df_parental_table = df_parental_results %>%
  mutate(mean=signif(mean, 3), lower=signif(lower, 3), upper=signif(upper,3),
         mean_st = round(mean_st, digits=1), lower_st = round(lower_st, digits=1), upper_st = round(upper_st, digits=1),
         Total = paste0(mean, " (", lower, ", ", upper, ")"),
         Standardised = paste0(mean_st, " (", lower_st, ", ", upper_st, ")")) %>%
  select(Region, Total, Standardised)

df_elderly_table = df_elderly_results %>%
  mutate(mean=round(signif(mean, 3)), lower=round(signif(lower, 3)), upper=round(signif(upper,3)),
         mean_st = round(mean_st, digits=1), lower_st = round(lower_st, digits=1), upper_st = round(upper_st, digits=1),
         TotalElderly = paste0(mean, " (", lower, ", ", upper, ")"),
         StandardisedElderly = paste0(mean_st, " (", lower_st, ", ", upper_st, ")")) %>%
  select(Region, TotalElderly, StandardisedElderly)

df_both_table = df_both_results %>%
  mutate(mean=signif(mean, 3), lower=signif(lower, 3), upper=signif(upper,3),
         mean_st = round(mean_st, digits=1), lower_st = round(lower_st, digits=1), upper_st = round(upper_st, digits=1),
         TotalBoth = paste0(mean, " (", lower, ", ", upper, ")"),
         StandardisedBoth = paste0(mean_st, " (", lower_st, ", ", upper_st, ")")) %>%
  select(Region, TotalBoth, StandardisedBoth)



# Create empty dataframe with a row for each region
table_two = data.frame(Region=c(sort(uf_lbls$uf_name), "Total"))
table_two = table_two %>% left_join(df_parental_table, by="Region")
table_two = table_two %>% left_join(df_elderly_table, by="Region")
table_two = table_two %>% left_join(df_both_table, by="Region")

write_clip(table_two)



