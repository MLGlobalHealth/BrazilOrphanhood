
rm(list=ls())

library(tidyverse)
library(readxl)
library(reshape2)

# Prevent messages that are known and annoying in a for-loop
options(dplyr.summarise.inform = FALSE)

# Load region labels
uf_lbls = read.csv("data/UF_labels.csv")
ufs = c("BR", uf_lbls$uf_code) # We have manually added "BR" for national numbers

# Specify the indices in the .xls files for each set of data
male_inds = 5:96
male_lbl = 4
female_inds = 101:192
female_lbl = 100
both_inds = 196:287
both_lbl = 195

# This is used in the for-loop to check that sums over age groups match the reported totals
check_sumtotal = function(df) {
  
  df_manual = df %>% filter(age_group!="TOTAL") %>% group_by(year, sex, uf) %>% summarise(n_manual=sum(n_pop))
  df_total = df %>% filter(age_group == "TOTAL")
  df_test = full_join(df_manual, df_total, by=c("year", "sex", "uf"))
  if (sum(df_test$n_manual != df_test$n_pop, na.rm=TRUE) > 0) {
    print(paste0("Current UF = ", df$uf[1]))
    print(paste0("Current source year = ", df$source_year[1]))
    stop("Totals do not add up!!")
  }
  
}

# Name our output dataframe
df_all = NULL

# Iterate over all regions
for (current_uf in ufs) {
  
  # Load .xls file
  df_in = read_excel("data/population/projections2018_simple.xls", sheet=current_uf, .name_repair = "unique_quiet") %>% data.frame()
  
  # Extract male dataframe 
  df_male = df_in[male_inds,]
  colnames(df_male) = df_in[male_lbl,]
  colnames(df_male)[1] = "age_group"
  
  # Melt into long-format
  df_male = df_male %>%
    melt(id.vars=c("age_group")) %>%
    rename(year=variable, n_pop=value) %>%
    mutate(sex="male", uf=current_uf, source_year=2018)
  
  # Extract female dataframe
  df_female = df_in[female_inds,]
  colnames(df_female) = df_in[female_lbl,]
  colnames(df_female)[1] = "age_group"
  
  # Melt into long-format
  df_female = df_female %>%
    melt(id.vars=c("age_group")) %>%
    rename(year=variable, n_pop=value) %>%
    mutate(sex="female", uf=current_uf, source_year=2018)
  
  # Extract combined population
  df_both = df_in[both_inds,]
  colnames(df_both) = df_in[both_lbl,]
  colnames(df_both)[1] = "age_group"
  
  # Melt into long-format
  df_both = df_both %>%
    melt(id.vars=c("age_group")) %>%
    rename(year=variable, n_pop=value) %>%
    mutate(sex="both", uf=current_uf, source_year=2018)
  
  # Check that the totals add up and match (this is a way of checking we have used the correct indices)
  check_sumtotal(df_male)
  check_sumtotal(df_female)
  check_sumtotal(df_both)
  
  # Append new data to the output dataframe and tidy
  df_all = df_all %>% rbind(df_male) %>% rbind(df_female) %>% rbind(df_both)
  rm(df_in, df_male, df_female, df_both)
  
}


# And we can also check sum of male + female = both
df_both = df_all %>% filter(sex=="both")
df_sum = df_all %>% filter(sex!="both") %>% group_by(age_group, year, source_year, uf) %>% summarise(n_pop_sum=sum(n_pop))
df_test = full_join(df_both, df_sum, by=c("age_group", "year", "source_year", "uf")) %>% mutate(nums_match=n_pop==n_pop_sum)
if (sum(df_test$n_pop != df_test$n_pop_sum, na.rm=TRUE) > 0) {
  stop("Unexpected mis-match in population counts")
}

# Finally we extract the years we need
df_save = df_all %>%
  mutate(year=as.numeric(as.character(year))) %>%
  filter(!is.na(year)) %>%
  filter(year>=2010, year<=2020) %>%
  rename(uf_code=uf) %>%
  left_join(uf_lbls %>% select(uf, uf_code), by="uf_code")

# Save the data
write.csv(df_save, "data/population/popests_simple_2010_2020.csv", row.names=FALSE)

# And reset R's message behaviour
options(dplyr.summarise.inform = TRUE)
