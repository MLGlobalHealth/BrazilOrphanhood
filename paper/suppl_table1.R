
rm(list=ls())

library(tidyverse)
library(clipr)

source("R/sampleDoubleCoresAndParental.R")

uf_lbls = read.csv("data/UF_labels.csv")

df_popn = read.csv("data/population/popests_simple_2010_2020.csv") %>%
  filter(age_group %in% paste0(seq(0, 17)), year==2020, !is.na(uf), sex!="both") %>%
  group_by(uf) %>%
  summarise(n_children=sum(n_pop)) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  select(Region, uf, n_children)

df_excess = loadCoresAndParentalOrphanhood(mortality="excess") %>%
  left_join(df_popn, by="uf") %>%
  mutate(mean_st = 100000*mean/n_children, lower_st = 100000*lower/n_children, upper_st = 100000*upper/n_children)
  
df_allcause = loadCoresAndParentalOrphanhood(mortality="allcause") %>%
  left_join(df_popn, by="uf") %>%
  mutate(mean_st = 100000*mean/n_children, lower_st = 100000*lower/n_children, upper_st = 100000*upper/n_children)

df_excess_table = df_excess %>%
  mutate(mean=signif(mean, 3), lower=signif(lower, 3), upper=signif(upper,3),
         mean_st = round(mean_st, digits=1), lower_st = round(lower_st, digits=1), upper_st = round(upper_st, digits=1),
         Total = paste0(mean, " (", lower, ", ", upper, ")"),
         Standardised = paste0(mean_st, " (", lower_st, ", ", upper_st, ")")) %>%
  select(Region, Total, Standardised)

df_allcause_table = df_allcause %>%
  mutate(mean=signif(mean, 3), lower=signif(lower, 3), upper=signif(upper,3),
         mean_st = round(mean_st, digits=1), lower_st = round(lower_st, digits=1), upper_st = round(upper_st, digits=1),
         TotalAllcause = paste0(mean, " (", lower, ", ", upper, ")"),
         StandardisedAllcause = paste0(mean_st, " (", lower_st, ", ", upper_st, ")")) %>%
  select(Region, TotalAllcause, StandardisedAllcause)

table = data.frame(Region=sort(uf_lbls$uf_name))
table = table %>% left_join(df_excess_table, by="Region")
table = table %>% left_join(df_allcause_table, by="Region")

write_clip(table)
