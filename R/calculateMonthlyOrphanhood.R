
library(tidyverse)
library(zoo)

source("R/sampleExcessMortality.R")


#' Calculate monthly orphanhood estimates
#'
#' Computes orphanhood due to parental mortality on a monthly basis using mortality and fertility data.
#' No uncertainty is provided, as monthly excess mortality estimates lack uncertainty measures.
#'
#' @param mortality Character; either "excess" or "allcause" (default: "excess").
#' @param by_child_age Logical; if TRUE, computes orphanhood disaggregated by child age (default: FALSE).
#' @return A dataframe with monthly orphanhood estimates.
calculateMonthlyOrphanhood = function(mortality="excess", by_child_age=FALSE) {
  
  df_em_in = read.csv("data/excessmortality/excess_mortality_frequentist.csv") %>% select(age_group, sex, uf, month, year, age_sex_pop_uf, deaths, fitted_deaths, excess_deaths)
  
  if (mortality=="excess") {
    df_em_in2 = df_em_in %>% rename(parental_deaths = excess_deaths)
  } else if (mortality=="allcause") {
    df_em_in2 = df_em_in %>% rename(parental_deaths = deaths)
  }
  
  df_em = df_em_in2 %>%
    mutate(parental_deaths = pmax(parental_deaths, 0)) %>%
    group_by(age_group, uf, sex, month, year) %>%
    summarise(parental_deaths=sum(parental_deaths))
  
  # Load female fertility data and find the average
  if (by_child_age) {
    
    df_female_fertility = readRDS("samples/femalefertility_age_group_child_age_uf.RDS") %>%
      group_by(age_group, uf, child_age) %>%
      summarise(mean_child=mean(mean_child)) %>%
      mutate(sex="female")
    
    df_male_fertility = readRDS("samples/malefertility_age_group_child_age_uf.RDS") %>%
      group_by(age_group, uf, child_age) %>%
      summarise(mean_child=mean(n_child)) %>%
      mutate(sex="male", uf = as.numeric(as.character(uf)))
    
    df_fertility = rbind(df_female_fertility, df_male_fertility) %>% ungroup()
    
    df_orphanhood = df_em %>%
      inner_join(df_fertility, by=c("uf", "age_group", "sex"), relationship="many-to-many") %>%
      mutate(orphanhood = parental_deaths * mean_child) %>%
      replace_na(list(orphanhood=0, mean_child=0))
    
  } else {
    
    df_female_fertility = readRDS("samples/femalefertility_age_group_uf.RDS") %>%
      group_by(age_group, uf) %>%
      summarise(mean_child=mean(mean_child)) %>%
      mutate(sex="female")
    
    df_male_fertility = readRDS("samples/malefertility_age_group_uf.RDS") %>%
      group_by(age_group, uf) %>%
      summarise(mean_child=mean(n_child)) %>%
      mutate(sex="male", uf = as.numeric(as.character(uf)))
    
    df_fertility = rbind(df_female_fertility, df_male_fertility) %>% ungroup()
    
    df_orphanhood = df_em %>%
      left_join(df_fertility, by=c("uf", "age_group", "sex")) %>%
      mutate(orphanhood = parental_deaths * mean_child) %>%
      replace_na(list(orphanhood=0, mean_child=0))
    
  }
  
  
  return(df_orphanhood)
  
}


