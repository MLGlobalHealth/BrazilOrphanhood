rm(list=ls())

library(tidyverse)
library(zoo)
library(geobr)
library(viridis)
library(cowplot)
library(clipr)

source("R/supportFunctions.R")

uf_lbls = rbind(
  read.csv("data/UF_labels.csv"),
  data.frame(uf=-99, uf_code="Total", uf_name="Total")
)


# Load population data
df_popn = read.csv("data/population/popests_simple_2010_2020.csv") %>%
  filter(age_group %in% paste0(seq(0, 17)), year==2020, !is.na(uf), sex!="both") %>%
  group_by(uf) %>%
  summarise(n_children=sum(n_pop)) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  select(Region, uf, n_children)

# Append the national total number of children as a new row
df_popn = df_popn %>% rbind(data.frame(Region="Total", uf=-99, n_children=sum(df_popn$n_children)))



# ------------------------------------------------------------------------------
# --------------------------- Parental all causes ----------------------------------
# ------------------------------------------------------------------------------

# Load samples
df_parental = readRDS("samples/orphanhood_age_group_sex_uf_allcause.RDS")

# Process parental orphanhood results by region
df_parental_summaries = rbind(
  df_parental %>% summariseByGroup(group_vars=c("uf", "sex")),
  df_parental %>% summariseByGroup(group_vars=c("sex")) %>% mutate(uf=-99),
  df_parental %>% summariseByGroup(group_vars=c("uf")) %>% mutate(sex="Total"),
  df_parental %>% summariseByGroup(group_vars=c()) %>% mutate(sex="Total", uf=-99)
) %>%
  ungroup() %>%
  left_join(uf_lbls, by="uf") %>%
  left_join(df_popn, by="uf") %>%
  mutate(mean_st = 1000*mean/n_children, lower_st = 1000*lower/n_children, upper_st = 1000*upper/n_children) %>%
  mutate(mean=signif(mean, 3), lower=signif(lower, 3), upper=signif(upper,3),
         mean_st = round(mean_st, digits=1), lower_st = round(lower_st, digits=1), upper_st = round(upper_st, digits=1),
         Total = paste0(mean, " (", lower, ", ", upper, ")"),
         Standardised = paste0(mean_st, " (", lower_st, ", ", upper_st, ")")) %>%
  select(sex, Region, Total, Standardised) %>%
  pivot_wider(names_from="sex", values_from=c("Total", "Standardised")) %>%
  arrange(Region) %>%
  select(Region, Total_female, Total_male, Total_both, Total_Total, Standardised_female, Standardised_male, Standardised_both, Standardised_Total) %>%
  rename(Maternal = Total_female, Paternal = Total_male, Double = Total_both, Total = Total_Total,
         Maternalstandardised = Standardised_female, Paternalstandardised=Standardised_male, Doublestandardised=Standardised_both, Totalstandardised=Standardised_Total)

# Write to clipboard
write_clip(df_parental_summaries)



# ------------------------------------------------------------------------------
# --------------------------- Parental COVID-19 associated ----------------------------------
# ------------------------------------------------------------------------------

# Load samples
df_parental = readRDS("samples/orphanhood_age_group_sex_uf_excess.RDS")

# Process parental orphanhood results by region
df_parental_summaries = rbind(
  df_parental %>% summariseByGroup(group_vars=c("uf", "sex")),
  df_parental %>% summariseByGroup(group_vars=c("sex")) %>% mutate(uf=-99),
  df_parental %>% summariseByGroup(group_vars=c("uf")) %>% mutate(sex="Total"),
  df_parental %>% summariseByGroup(group_vars=c()) %>% mutate(sex="Total", uf=-99)
) %>%
  ungroup() %>%
  left_join(uf_lbls, by="uf") %>%
  left_join(df_popn, by="uf") %>%
  mutate(mean_st = 1000*mean/n_children, lower_st = 1000*lower/n_children, upper_st = 1000*upper/n_children) %>%
  mutate(mean=signif(mean, 3), lower=signif(lower, 3), upper=signif(upper,3),
         mean_st = round(mean_st, digits=1), lower_st = round(lower_st, digits=1), upper_st = round(upper_st, digits=1),
         Total = paste0(mean, " (", lower, ", ", upper, ")"),
         Standardised = paste0(mean_st, " (", lower_st, ", ", upper_st, ")")) %>%
  select(sex, Region, Total, Standardised) %>%
  pivot_wider(names_from="sex", values_from=c("Total", "Standardised")) %>%
  arrange(Region) %>%
  select(Region, Total_female, Total_male, Total_both, Total_Total, Standardised_female, Standardised_male, Standardised_both, Standardised_Total) %>%
  rename(Maternal = Total_female, Paternal = Total_male, Double = Total_both, Total = Total_Total,
         Maternalstandardised = Standardised_female, Paternalstandardised=Standardised_male, Doublestandardised=Standardised_both, Totalstandardised=Standardised_Total)

# Write to clipboard
write_clip(df_parental_summaries)



# ------------------------------------------------------------------------------
# --------------------------- Coresident COVID-19 associated ----------------------------------
# ------------------------------------------------------------------------------

# Load samples
df_elderly_in = rbind(
  readRDS("samples/coresorphanhood_any_age_group_sex_uf_excess.RDS") %>% mutate(type="Any"),
  readRDS("samples/coresorphanhood_multiple_age_group_sex_uf_excess.RDS") %>% mutate(type="Multiple"),
  readRDS("samples/coresorphanhood_single_age_group_sex_uf_excess.RDS") %>% mutate(type="Single"),
  readRDS("samples/coresorphanhood_noadults_age_group_sex_uf_excess.RDS") %>% mutate(type="NoAdults")
)

df_elderly_summaries = rbind(
  df_elderly_in %>% summariseByGroup(group_vars=c("uf", "type")),
  df_elderly_in %>% summariseByGroup(group_vars=c("type")) %>% mutate(uf=-99)
) %>%
  ungroup() %>%
  left_join(uf_lbls, by="uf") %>%
  left_join(df_popn, by="uf") %>%
  mutate(mean_st = 1000*mean/n_children, lower_st = 1000*lower/n_children, upper_st = 1000*upper/n_children) %>%
  mutate(mean=round(signif(mean, 3)), lower=round(signif(lower, 3)), upper=round(signif(upper,3)),
         mean_st = round(mean_st, digits=1), lower_st = round(lower_st, digits=1), upper_st = round(upper_st, digits=1),
         Total = paste0(mean, " (", lower, ", ", upper, ")"),
         Standardised = paste0(mean_st, " (", lower_st, ", ", upper_st, ")")) %>%
  select(type, Region, Total, Standardised) %>%
  pivot_wider(names_from="type", values_from=c("Total", "Standardised")) %>%
  arrange(Region) %>%
  select(Region, Total_Single, Total_Multiple, Total_Any, Total_NoAdults, Standardised_Single, Standardised_Multiple, Standardised_Any, Standardised_NoAdults) %>%
  rename(Single = Total_Single, Multiple = Total_Multiple, Any = Total_Any, NoAdults = Total_NoAdults,
         Singlestandardised = Standardised_Single, Multiplestandardised=Standardised_Multiple, Anystandardised=Standardised_Any, NoAdultsstandardised=Standardised_NoAdults)
  
write_clip(df_elderly_summaries)



# ------------------------------------------------------------------------------
# --------------------------- Coresident all cause ----------------------------------
# ------------------------------------------------------------------------------

# Load samples
df_elderly_in = rbind(
  readRDS("samples/coresorphanhood_any_age_group_sex_uf_allcause.RDS") %>% mutate(type="Any"),
  readRDS("samples/coresorphanhood_multiple_age_group_sex_uf_allcause.RDS") %>% mutate(type="Multiple"),
  readRDS("samples/coresorphanhood_single_age_group_sex_uf_allcause.RDS") %>% mutate(type="Single"),
  readRDS("samples/coresorphanhood_noadults_age_group_sex_uf_allcause.RDS") %>% mutate(type="NoAdults")
)

df_elderly_summaries = rbind(
  df_elderly_in %>% summariseByGroup(group_vars=c("uf", "type")),
  df_elderly_in %>% summariseByGroup(group_vars=c("type")) %>% mutate(uf=-99)
) %>%
  ungroup() %>%
  left_join(uf_lbls, by="uf") %>%
  left_join(df_popn, by="uf") %>%
  mutate(mean_st = 1000*mean/n_children, lower_st = 1000*lower/n_children, upper_st = 1000*upper/n_children) %>%
  mutate(mean=round(signif(mean, 3)), lower=round(signif(lower, 3)), upper=round(signif(upper,3)),
         mean_st = round(mean_st, digits=1), lower_st = round(lower_st, digits=1), upper_st = round(upper_st, digits=1),
         Total = paste0(mean, " (", lower, ", ", upper, ")"),
         Standardised = paste0(mean_st, " (", lower_st, ", ", upper_st, ")")) %>%
  select(type, Region, Total, Standardised) %>%
  pivot_wider(names_from="type", values_from=c("Total", "Standardised")) %>%
  arrange(Region) %>%
  select(Region, Total_Single, Total_Multiple, Total_Any, Total_NoAdults, Standardised_Single, Standardised_Multiple, Standardised_Any, Standardised_NoAdults) %>%
  rename(Single = Total_Single, Multiple = Total_Multiple, Any = Total_Any, NoAdults = Total_NoAdults,
         Singlestandardised = Standardised_Single, Multiplestandardised=Standardised_Multiple, Anystandardised=Standardised_Any, NoAdultsstandardised=Standardised_NoAdults)

write_clip(df_elderly_summaries)




