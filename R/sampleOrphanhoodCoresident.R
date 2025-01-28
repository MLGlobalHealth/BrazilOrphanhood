
library(tidyverse)
library(foreach)
library(survey)
library(PNSIBGE)
library(doParallel)
library(PNSIBGE)
library(progress)

source("R/supportFunctions.R")
source("R/sampleOrphanhoodHelpers.R") # We use the loadPopnData() function from this file

options(dplyr.summarise.inform = FALSE)

#' Generate samples of co-resident orphanhood from PNS 2019 data
#' 
#' Multiple types of output can be generated, including:
#'  - "any" : The number of children that lost an adult aged >= adult_min_age in the household
#'  - "female": The number of children that lost a female aged >= adult_min_age in the household
#'  - "male": The number of children that lost a male aged >= adult_min_age in the household
#'  - "single": The number of children that lost exactly one adult aged >= adult_min_age in the household
#'  - "multiple": The number of children that lost at least two adults aged >= adult_min_age in the household
#'  - "noadults": The number of children that lost an adult aged >= adult_min_age in the household, where no adults aged 18-adult_min_age are present
#'  - "only": The number of children that lost all co-resident adults aged >= adult_min_age in the household, where no adults aged 18-adult_min_age are present
#'  
#'  @param N The number of samples to generate. Default N = 1000.
#'  @param output The type of output to generate (see above). Default output = "any".
#'  @param mortality The type of mortality to use. Options are "excess" or "allcause". Defaults to "excess".
#'  @param by_child_age If this is TRUE then the method will produce orphanhood estimates disaggregated by the age of the child. Defaults to FALSE.
#'  @param current_child_age If by_child_age is TRUE, then this parameter specifies the age of the child to disaggregate by. Defaults to NA.
#'  @param adult_min_age The minimum age of an adult to be considered in the analysis. Default adult_min_age = 60.
#'  @return A dataframe containing co-resident orphanhood estimates.
sampleCoresidentOrphanhood = function(N=1000,
                                      output="any", # Options: "any", "female", "male", "single", "multiple", "noadults", "only"
                                      mortality="excess",
                                      by_child_age=FALSE,
                                      current_child_age=NA,
                                      adult_min_age=60) {
  
  # Set and sort grouping variables
  group_vars = sort(c("uf", "age_group", "sex"))
  
  # Load population data
  df_popn = loadPopnDataForOrphanhood(group_vars)
  
  # Load population data of children
  df_popn_children = read.csv("data/population/popests_simple_2010_2020.csv") %>%
    filter(age_group %in% paste0(seq(0, 17)), year==2020, !is.na(uf), sex!="both") %>%
    group_by(uf) %>%
    summarise(n_children=sum(n_pop))
  
  # Load excess mortality samples
  if (mortality == "excess") {
    df_em = readRDS(paste0("samples/excessmortality_", paste0(group_vars, collapse="_"), ".RDS")) %>% filter(age_group %in% c("60-69", "70+"))
  } else if (mortality == "allcause") {
    df_em = readRDS(paste0("samples/allcausemortality_", paste0(group_vars, collapse="_"), ".RDS")) %>% filter(age_group %in% c("60-69", "70+"))
  }
  
  # Get number of iterations of excess mortality samples
  N_em = max(df_em$iter)
  
  # Load and prepare PNS data
  df_pns_in = readRDS("data/pns2019/pns2019_processed.rds")
  df_pns = df_pns_in %>%
    appendAgeGroups() %>%
    group_by(household_id) %>%
    mutate(n_children_in_house=sum(age <= 17),
           n_older_adults_in_house = sum(age >= adult_min_age),
           uf = as.numeric(as.character(uf)),
           n_adults_in_house = sum((age >= 18) & (age <=59)),
           n_elderly_in_house = sum(age>=60)) %>%
    ungroup()
  
  # Fetch all combinations of variables
  df_combinations = expand.grid(uf=unique(df_pns$uf))
  
  # And now we bootstrap
  print("Starting bootstrap...")
  pb = progress_bar$new(format="Bootstrapping co-resident orphanhood... :bar :percent eta: :eta", total=N, clear=FALSE, width=100)
  df_allsamples = foreach(ii = seq(1, N), .combine=rbind, .packages=c("tidyverse", "survey", "foreach", "PNSIBGE")) %dopar% {
    
    # Sample probability that any given older person died
    ii_em = sample(1:N_em, 1)
    df_em_sub = df_em %>% subset(iter==ii_em) %>% select(-iter)
    df_pexcessdeath = df_em_sub %>%
      left_join(df_popn, by=group_vars) %>%
      mutate(p_died=excess_deaths/n_pop) %>%
      select(all_of(c(group_vars, "p_died")))
    
    # Stack this next to the PNS data, sample those that died, and count number of deaths by household
    df_numdeathsbyhousehold = df_pns %>%
      left_join(df_pexcessdeath, by=group_vars) %>%
      ungroup() %>%
      mutate(did_die = runif(nrow(df_pns)) < p_died) %>%
      group_by(household_id) %>%
      summarise(n_elderly_deaths = sum(did_die, na.rm=TRUE),
                n_male_elderly_deaths = sum(did_die[sex=="male"], na.rm=TRUE),
                n_female_elderly_deaths = sum(did_die[sex=="female"], na.rm=TRUE))
    
    # Join this back onto the main PNS object and calculate variable of interest
    df_pns_tmp = df_pns %>%
      left_join(df_numdeathsbyhousehold, by="household_id") %>%
      mutate(outputvar = case_when(output == "any" ~ (n_elderly_deaths>0)*1,
                                   output == "female" ~ (n_female_elderly_deaths>0)*1,
                                   output == "male" ~ (n_male_elderly_deaths>0)*1,
                                   output == "single" ~ (n_elderly_deaths==1)*1,
                                   output == "multiple" ~ (n_elderly_deaths>=2)*1,
                                   output == "noadults" ~ ((n_elderly_deaths>0)&(n_adults_in_house==0))*1,
                                   output == "only" ~ ((n_elderly_deaths==n_elderly_in_house)&(n_adults_in_house==0)&(n_elderly_deaths>0))*1)
      )
    
    
    # Turn it into a survey design object and subset so only children are considered
    died_svydes_child = df_pns_tmp %>% pns_design() %>% subset(age <= 17)
    
    # and further subset by current child-age if necessary
    if (by_child_age) {
      died_svydes_child = died_svydes_child %>% subset(age==current_child_age)
    }
    
    # And now we calculate for each group
    df_out = foreach(jj = seq(1, nrow(df_combinations)), .combine=rbind) %do% {
      
      # Subset to current uf
      current_uf = df_combinations$uf[jj]
      
      died_svydes_childuf = died_svydes_child %>% subset(uf==current_uf)
      current_child_popn = df_popn_children %>% filter(uf==current_uf) %>% pull(n_children)
      
      # Calculate the survey results
      u = runif(1)
      
      if (u > 0.5) {
        lvl = 2*u - 1
        p_children_lost = svyciprop(~outputvar>0, died_svydes_childuf, level=lvl)
        sample = confint(p_children_lost)[2] * current_child_popn
      } else {
        lvl = 1-2*u
        p_children_lost = svyciprop(~outputvar>0, died_svydes_childuf, level=lvl)
        sample = confint(p_children_lost)[1] * current_child_popn
      }
      
      df_tmp = data.frame(uf=current_uf,
                          child_age = current_child_age,
                          orphanhood = sample)
      
      
      return(df_tmp)
      
    }
    
    pb$tick()
    return(df_out %>% mutate(iter=ii))
    
  }
  
  
  # Write files to the /samples/ folder
  if (by_child_age) {
    fname = paste0("samples/interim/coresorphanhood_", output, "_", paste0(group_vars, collapse="_"), "_", mortality, "_CHILDAGE", current_child_age, ".RDS")
  } else {
    fname = paste0("samples/coresorphanhood_", output, "_", paste0(group_vars, collapse="_"), "_", mortality, ".RDS")
  }
  saveRDS(df_allsamples, fname)
  
  return(df_allsamples)
  
}

