
rm(list=ls())

library(tidyverse)
library(readxl)
library(reshape2)

# Prevent messages that are known and annoying in a for-loop
options(dplyr.summarise.inform = FALSE)

# Load region labels
uf_lbls = read.csv("data/UF_labels.csv")
ufs = uf_lbls$uf_code

# Specify the indices in the .xls files for each set of data
male_inds = 5:24
male_lbl = 4
female_inds = 28:47
female_lbl = 27
both_inds = 51:70
both_lbl = 50

# Specify source years (for filenames and output labels)
source_years = c(2013, 2018)

# This is used in the for-loop to check that sums over age groups match the reported totals
check_sumtotal = function(df) {
  
  df_manual = df %>% filter(age_group!="Total") %>% group_by(year, sex, uf) %>% summarise(n_manual=sum(n_pop))
  df_total = df %>% filter(age_group == "Total")
  df_test = full_join(df_manual, df_total, by=c("year", "sex", "uf"))
  if (sum(df_test$n_manual != df_test$n_pop) > 0) {
    print(paste0("Current UF = ", df$uf[1]))
    print(paste0("Current source year = ", df$source_year[1]))
    stop("Totals do not add up!!")
  }
  
}

# Name our output dataframe
df_all = NULL

# Iterate over both files
for (current_source_year in source_years) {
  
  # Iterate over individual states
  for (current_uf in ufs) {
    
    # Load data for the current source year and state
    df_in = read_excel(paste0("data/population/projections", current_source_year, ".xls"), sheet=current_uf, .name_repair = "unique_quiet") %>% data.frame()
    
    # Extract male dataframe 
    df_male = df_in[male_inds,]
    colnames(df_male) = df_in[male_lbl,] # Fix up column-names
    colnames(df_male)[1] = "age_group" # Convert to standardised name
    
    # Melt into long-format
    df_male = df_male %>%
      melt(id.vars=c("age_group")) %>%
      rename(year=variable, n_pop=value) %>%
      mutate(sex="male", uf=current_uf, source_year=current_source_year)
    
    # Extract female dataframe
    df_female = df_in[female_inds,]
    colnames(df_female) = df_in[female_lbl,]
    colnames(df_female)[1] = "age_group"
    
    # Melt into long-format
    df_female = df_female %>%
      melt(id.vars=c("age_group")) %>%
      rename(year=variable, n_pop=value) %>%
      mutate(sex="female", uf=current_uf, source_year=current_source_year)
    
    # Extract combined population
    df_both = df_in[both_inds,]
    colnames(df_both) = df_in[both_lbl,]
    colnames(df_both)[1] = "age_group"
    
    # Melt into long-format
    df_both = df_both %>%
      melt(id.vars=c("age_group")) %>%
      rename(year=variable, n_pop=value) %>%
      mutate(sex="both", uf=current_uf, source_year=current_source_year)
    
    # Check that the totals add up and match (this is a way of checking we have used the correct indices)
    check_sumtotal(df_male)
    check_sumtotal(df_female)
    check_sumtotal(df_both)
    
    # Append new data to the output dataframe and tidy
    df_all = df_all %>% rbind(df_male) %>% rbind(df_female) %>% rbind(df_both)
    rm(df_in, df_male, df_female, df_both)
    
  }
  
}


# We also check that the sum of male + female = both
df_both = df_all %>% filter(sex=="both")
df_sum = df_all %>% filter(sex!="both") %>% group_by(age_group, year, source_year, uf) %>% summarise(n_pop_sum=sum(n_pop))
df_test = full_join(df_both, df_sum, by=c("age_group", "year", "source_year", "uf")) %>% mutate(nums_match=n_pop==n_pop_sum)
# If there are more than the 12 expected discrepancies, then something has gone wrong in processing.
# These 12 known errors (out of 44,280 possible errors) that feature a discrepancy of a single individual each
# These errors are inherent in the data, and not a result of our processing.
if (sum(df_test$n_pop != df_test$n_pop_sum) != 12) {
  stop("Unexpected mis-match in population counts")
}

# Finally we extract the years we need
df_save = df_all %>%
  mutate(year=as.numeric(levels(year))[year]) %>%
  filter(year>=2002, year<=2020) %>%
  filter( (year<=2009 & source_year==2013) | (year >=2010 & source_year==2018) ) %>%
  rename(uf_code=uf) %>%
  left_join(uf_lbls %>% select(uf, uf_code), by="uf_code")

# Save the data
write.csv(df_save, "data/population/popests_2002_2020.csv", row.names=FALSE)

# And reset R's message behaviour
options(dplyr.summarise.inform = TRUE)
