
library(tidyverse)
library(foreach)
library(DemoTools)
library(progress)

options(dplyr.summarise.inform = FALSE)

source("R/supportFunctions.R")

#' Generate samples of female fertility from live births data
#'
#' Estimates female fertility for each region and (mother) age-group. 
#' Optionally disaggregates by child age.
#'
#' @param Niter Number of iterations (default: 100).
#' @param Nsamples Number of Poisson samples per iteration (default: 100).
#' @param by_child_age Logical; if TRUE, estimates fertility disaggregated by child age.
#' @param outdir Directory to save the results (default: "samples/").
#' @param savefile Logical; if TRUE, saves results to outdir.
#' @return A dataframe of fertility estimates with resampled births.
sampleFemaleFertility = function(Niter=100,
                                 Nsamples=100,
                                 by_child_age=FALSE,
                                 outdir="samples/",
                                 savefile=TRUE) {
  
  # Load data
  df = loadFemaleFertilityData()
  df_popn_2020 = loadFemalePopulationData() %>% filter(year==2020) %>% rename(mother_age_today=mother_age) %>% select(-year)
  df_mortality = loadChildMortalityData()
  if (by_child_age) {
    group_vars = sort(c("uf", "age_group", "child_age"))
  } else {
    group_vars = sort(c("uf", "age_group"))
  }
  
  # Iterate over samples to get single-year estimates
  pb = progress_bar$new(format="Sampling female fertility... :bar :percent eta: :eta", total=Niter, clear=FALSE, width=100)
  df_results = foreach(ii=seq(1, Niter), .combine=rbind) %do% {
    
    # Sample the underreporting adjustment factor and apply it to the number of children
    u = runif(1)
    df$ra = u * (df$under_max - df$under_min) + df$under_min
    df$n_adj = df$n * (1 + df$ra)/(1-df$p_missing)
    
    # Calculate the mean number of children per mother for every possible combination
    df_tmp = df %>%
      mutate(child_age_today = 2020 - year, mother_age_today = mother_age + (2020 - year)) %>% # Calculate the age of the child and mother today (rather than at-birth)
      filter(child_age_today <= 17) %>% # Filter out children older than 17
      group_by(mother_age_today, child_age_today, uf) %>%
      summarise(mean_child = sum(n_adj / n_pop)) %>% # For each mother_age/child_age/region, calculate the mean number of children
      left_join(df_popn_2020, by = c("mother_age_today", "uf")) %>% # Append the population count of mothers
      appendAgeGroups("mother_age_today") %>% # Append the age group of the mother
      rename(age = mother_age_today, child_age = child_age_today) # And standardise column names
    
    # Now adjust for child mortality
    df_tmpb = df_tmp %>%
      left_join(df_mortality %>% rename(child_age=age), by="child_age") %>%
      mutate(mean_child = mean_child*p_survive)
    
    # Aggregate by our group_vars
    # If we are disaggregating by child_age, we can simply take the group-wise population-weighted average to convert from single-year-age to age-groups
    if (by_child_age) {
      df_out = df_tmpb %>%
        group_by_at(group_vars) %>%
        summarise(mean_child=sum(mean_child*n_pop)/sum(n_pop))
    } else { # If we are not disaggregating by child_age, we need to first integrate this out
      df_out = df_tmpb %>%
        group_by(age, uf, n_pop, age_group) %>%
        summarise(mean_child=sum(mean_child)) %>% # First we sum over all children's ages
        group_by_at(group_vars) %>% # And now we aggregate by our chosen group_vars
        summarise(mean_child=sum(mean_child*n_pop)/sum(n_pop))
    }
    
    # Poisson sample Nsample times
    df_out = foreach(jj=1:Nsamples, .combine=rbind) %do% {
      df_out %>% mutate(n_child=rpois(n(), mean_child), sample=jj)
    }
    
    # Finally we ensure that any missing combinations are assigned a fertility rate of 0, instead of being ignored
    if (by_child_age) {
      df_combinations = expand.grid(age_group=unique(df_out$age_group), child_age=unique(df_out$child_age), uf=unique(df_out$uf), sample=unique(df_out$sample))
    } else {
      df_combinations = expand.grid(age_group=unique(df_out$age_group), uf=unique(df_out$uf), sample=unique(df_out$sample))
    }
    df_out = df_out %>% mutate(existing=TRUE) %>% right_join(df_combinations, by=c(group_vars, "sample"))
    df_out$existing[is.na(df_out$existing)] = FALSE
    df_out$mean_child[!df_out$existing] = 0
    df_out$n_child[!df_out$existing] = 0
    df_out = df_out %>% mutate(iter=ii) %>% select(-existing)
    
    pb$tick()
    return(df_out)
    
  }
  
  # Save and return results
  if (savefile) {
    saveRDS(df_results, paste0(outdir, "femalefertility_", paste0(group_vars, collapse="_"), ".RDS"))
  }
  return(df_results)
  
}



#' Load the pre-processed live births dataset
#'
#' Reads live births data, applies underreporting adjustments, 
#' and merges with population data.
#'
#' @return A dataframe of birth counts with adjusted underreporting estimates.
loadFemaleFertilityData = function() {
  
  # Load population data
  df_popn = loadFemalePopulationData()
  
  # Load total births data and append the population data
  df_births = read.csv("data/livebirths/livebirths_processed_2002_2020.csv") %>%
    group_by(mother_age, uf, year) %>% # This group_by->summarise sums out the father_ages also present in the data
    summarise(n=sum(n)) %>%
    left_join(df_popn, by=c("mother_age", "uf", "year")) %>%
    filter(mother_age <= 95 | is.na(mother_age), uf!=5) # Filter out a handful of invalid observations
  
  # Load underreporting data
  df_in = read.csv("data/livebirths/underreporting.csv")
  df_otherages = data.frame(mother_age=c(seq(0, 14), seq(50, 99)), underreporting=mean(df_in$underreporting)) # Assign the average underreporting to all ages not in the underreporting data
  df_underreporting = rbind(df_in, df_otherages) %>% mutate(under_min = 0, under_max = underreporting*2) # Specify lower and upper bounds to artifically add uncertainty
  
  # Calculate the proportion of missing ages within each group
  df_noage = df_births %>%
    group_by(uf, year) %>%
    summarise(n_na=sum(is.na(mother_age)*n), ntotal=sum(n)) %>%
    mutate(p_missing = n_na/ntotal) %>%
    select(uf, year, p_missing)
  
  # And append the underreporting and missing ages onto our main dataframe
  df_births = df_births %>%
    left_join(df_underreporting, by="mother_age") %>%
    left_join(df_noage, by=c("uf", "year")) %>%
    filter(uf!=5, !is.na(mother_age))
  
  return(df_births)
  
}



#' Load female population data for fertility estimates
#'
#' Reads female population estimates, interpolates single-year age groups using Sprague’s method.
#'
#' @return A dataframe with estimated female population by single-year ages.
loadFemalePopulationData = function() {
  
  # Load population data, we want only age-grouped data for females
  df_popn = read.csv("data/population/popests_2002_2020.csv") %>%
    filter(age_group!="Total", sex=="female") %>%
    group_by(age_group, sex, year, uf) %>%
    summarise(n_pop=sum(n_pop)) %>%
    data.frame()
  
  # Extract the minimum age from the age_group column
  # If the age_group ends with "+", replace "+" with empty string
  df_popn$min_age <- sapply(strsplit(as.character(df_popn$age_group), '-'), '[', 1)
  df_popn$min_age[df_popn$min_age=="90+"] = "90"
  df_popn$min_age = as.numeric(df_popn$min_age)
  
  # Create a data frame with all combinations of uf and year
  df_combinations = expand.grid(uf=unique(df_popn$uf), year=unique(df_popn$year))
  
  # For each combination of uf and year, interpolate population data using Sprague's method
  df_out = foreach(ii = seq(1, nrow(df_combinations)), .combine=rbind) %do% {
    
    df_current = df_popn %>% filter(uf==df_combinations$uf[ii], year==df_combinations$year[ii]) %>% arrange(min_age)
    pop_interp = DemoTools::graduate_sprague(Value=df_current$n_pop, Age=as.integer(df_current$min_age), OAG=FALSE)
    return(data.frame(mother_age=as.integer(names(pop_interp)), n_pop=unname(pop_interp), uf=df_combinations$uf[ii], year=df_combinations$year[ii]))
    
  }
  
  return(df_out)
  
}


#' Load child mortality data and compute survival probabilities
#'
#' Reads life tables, extracts child mortality rates, and calculates survival probabilities to age 17.
#'
#' @return A dataframe with child ages and corresponding survival probabilities.
loadChildMortalityData = function() {
  
  # Load data and convert into long format
  df_in = read.csv("data/childmortality/lifetables.csv") %>%
    pivot_longer(cols = -Age, names_to = "year", values_to = "value") %>%
    mutate(year = as.numeric(substr(year, 5, 8))) %>%
    rename(age=Age)
  
  # p of surviving to today
  age_today = 0:17
  p_survive = age_today*0 + 1
  for (ii in 1:length(age_today)) {
    current_age = age_today[ii]
    for (a in 0:current_age) {
      p_death = (df_in %>% filter(age==a, year==2020-current_age+a) %>% pull(value))/1000
      p_survive[ii] = p_survive[ii] * (1 - p_death)
    }
  }
  
  df = data.frame(age=age_today, p_survive = p_survive)
  
  return(df)
  
}








