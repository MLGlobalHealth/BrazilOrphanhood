
library(tidyverse)
library(zoo)
library(foreach)
library(survey)
library(PNSIBGE)
library(progress)


source("R/supportFunctions.R")

options(dplyr.summarise.inform = FALSE)


# Warning: other scripts rely on the PNS specific methods defined in the labelled section below. Edit these with caution.

#' Generate samples of male fertility from PNS 2019 data
#'
#' Estimates male fertility for each state and age-group using PNS survey data.
#'
#' @param N Number of times to resample survey data (default: 100).
#' @param by_child_age Logical; if TRUE, estimates fertility disaggregated by child age. Only applies to realised orphanhood.
#' @return A dataframe with fertility estimates.
sampleMaleFertility = function(N=100, by_child_age=FALSE) {
  
  # Load and process data
  print("Loading PNS data...")
  df = readRDS("data/pns2019/pns2019_processed.rds")
  printPNSSummary(df)
  
  # Derive the initial number of children
  print("Deriving number of children (first pass)...")
  df = df %>% deriveChildrenAges()
  
  # Append parent age-groups
  print("Appending age-groups...")
  df = df %>% appendAgeGroups("age")
  
  # Get table of all unique combinations of uf and parent age-group
  group_vars = c("uf", "age_group")
  unique_vals_in_cols = lapply(group_vars, function(x) unique(df[[x]]))
  df_combinations = do.call(expand.grid, unique_vals_in_cols)
  colnames(df_combinations) = group_vars
  
  # Add child_age to group_vars if needed
  if (by_child_age) { group_vars = union(group_vars, "child_age") }
  group_vars = sort(group_vars)
  
  # Set output filenames
  fname_final = paste0("samples/malefertility_", paste0(group_vars, collapse="_"), ".RDS")
  fname_interim = paste0("samples/interim/malefertility_", paste0(group_vars, collapse="_"), "_interim.RDS")
  
  # Iterate over individual resamplings of the survey data
  print("Starting main sampler...")
  results = list()
  for (ii in 1:N) {
    
    print(paste0("Iteration ", ii, " of ", N))
    
    # If we are on the 2nd (or greater) iteration then resample children ages
    if (ii >= 2) {
      df = resampleChildrenAges(df)
    }
    
    # Construct survey design object
    svydes = createPNSSurveyDesign(df)
    
    # And create samples
    if (!by_child_age) {
      df_new = sampleRealisedFertility(svydes, df_combinations, group_vars)
    } else {
      df_new = sampleRealisedFertilityByChildAge(svydes, df_combinations, group_vars)
    } 
    df_new$iter = ii
    
    # Store new results in a list
    results[[ii]] = df_new
    
    # And save interim results
    saveRDS(results, fname_interim)
    
  }
  
  # Combine results into one large dataframe and adjust for child mortality
  df_out = adjustMaleFertilityForChildMortality(do.call(rbind, results), by_child_age) %>% ungroup()
  
  # Save files and return
  saveRDS(df_out, fname_final)
  return(df_out)
  
}




#' Generates samples of realised male fertility
#'
#' Computes male fertility estimates for each combination of variables in df_combinations using survey design weights.
#'
#' @param svydes A survey design object constructed from PNS data.
#' @param df_combinations A dataframe describing the variable combinations for sampling.
#' @param group_vars Character vector specifying grouping variables.
#' @param pb_enabled Logical; if TRUE, displays a progress bar.
#' @return A dataframe with sampled fertility estimates.
sampleRealisedFertility = function(svydes,
                                   df_combinations,
                                   group_vars,
                                   pb_enabled=TRUE) {
  
  # Iterate over all unique combinations of uf and age-group
  if (pb_enabled) {
    pb = progress_bar$new(format="Running main sampler... :bar :percent eta: :eta", total=nrow(df_combinations), clear=FALSE, width=100)
  } else {
    pb = NULL
  }
  df_sample = foreach(jj=seq(1, nrow(df_combinations)), .combine=rbind) %do% {
    
    # Subset the survey design according to the current combination
    svydesign_sub = svydes
    for (var in group_vars) {
      svydesign_sub = svydesign_sub %>% subset(get(var)==df_combinations[jj,var])
    }
    
    # Extract sampling weights
    IPW = weights(svydesign_sub, type="sampling")
    
    # If non-zero weight, sample neff times from the number of children, weighted by IPW
    if (sum(IPW > 0) > 0) {
      
      # Calculate sample size and effective sample size
      IPWclean = IPW[IPW>0]
      n = length(IPWclean)
      neff = max(round(sum(IPW)^2/sum(IPW^2)), 1)
      
      df_sub = svydesign_sub$variables
      chosen_inds = sample(1:nrow(df_sub), neff, replace=TRUE, prob=IPW)
      df_tmp = data.frame(n_child=df_sub$n_children[chosen_inds], sample=1:neff)
    } else {
      df_tmp = data.frame(n_child=NA, sample=1)
    }
    
    # Append combination variables
    for (var in group_vars) {
      df_tmp[,var] = df_combinations[jj,var]
    }
    
    if (pb_enabled) { pb$tick() }
    return(df_tmp)
    
  }
  
  return(df_sample)
  
}



#' Generates samples of realised male fertility by child age
#'
#' Computes male fertility estimates stratified by child age for each combination of variables in df_combinations.
#'
#' @param svydes A survey design object constructed from PNS data.
#' @param df_combinations A dataframe describing the variable combinations for sampling.
#' @param group_vars Character vector specifying grouping variables.
#' @return A dataframe with fertility estimates stratified by child age.
sampleRealisedFertilityByChildAge = function(svydes,
                                             df_combinations,
                                             group_vars) {
  
  child_ages = seq(0,17)
  
  pb = progress_bar$new(format="Running main sampler... :bar :percent eta: :eta", total=length(child_ages), clear=FALSE, width=100)
  df_childages = foreach(current_child_age = child_ages, .combine=rbind) %do% {
    
    svydes_childage = svydes
    svydes_childage$variables$n_children = sapply(svydes_childage$variables$children_ages, function(x) sum(x==current_child_age))
    df_childageresults = sampleRealisedFertility(svydes_childage, df_combinations, setdiff(group_vars, "child_age"), pb_enabled=FALSE)
    df_childageresults$child_age = current_child_age
    
    pb$tick()
    return(df_childageresults)
    
  }
  
  return(df_childages)
  
}





# PNS-specific methods -------------------------------------------------------------------------------------

#' Derives the number of children and their ages for each father in the dataset
#'
#' Uses reported number of children, ages of youngest/oldest children, and a random allocation method for large families.
#'
#' @param df A dataframe containing PNS survey data.
#' @return The dataframe with additional columns for number of children and their ages.
deriveChildrenAges = function(df) {
  
  # Pre-allocate column
  df$n_children = rep(NA, nrow(df))
  df$children_ages = rep(NA, nrow(df))
  df$children_birthyears = rep(NA, nrow(df))
  df$children_randomly_allocated = rep(FALSE, nrow(df)) # This stores which rows we randomly allocated the no. of children in. Can be used later to ensure we only bootstrap over the changing things.
  
  # Get indices of all non-na children variables and iterate over these
  inds = which(!is.na(df$total_children))
  
  pb = progress_bar$new(format="Deriving initial number of children... :bar :percent eta: :eta", total=length(inds), clear=FALSE, width=100)
  for (ind in inds) {
    
    if (df$total_children[ind] == 0) { # If there are no children
      df$n_children[ind] = 0
      df$children_ages[ind] = list(c())
      df$children_birthyears[ind] = list(c())
    } else if (df$total_children[ind] == 1) { # If there is only one child
      df$n_children[ind] = (df$age_of_youngest_child[ind] < 18)*1
      df$children_ages[ind] = list(c(df$age_of_youngest_child[ind]))
      df$children_birthyears[ind] = list(c(2019-df$age_of_youngest_child[ind]))
    } else if (df$total_children[ind] == 2) { # If there are two children
      children_ages = c(df$age_of_youngest_child[ind], df$age_of_oldest_child[ind])
      df$n_children[ind] = sum(children_ages < 18)
      df$children_ages[ind] = list(children_ages)
      df$children_birthyears[ind] = list(2019-children_ages)
    } else if (df$total_children[ind] >= 3) { # If there are three or more children (only in this case is there randomness)
      children_ages_rnd = sample(df$age_of_youngest_child[ind]:df$age_of_oldest_child[ind], df$total_children[ind]-2, replace=T)
      children_ages = c(df$age_of_youngest_child[ind], df$age_of_oldest_child[ind], children_ages_rnd)
      df$children_ages[ind] = list(children_ages)
      df$children_birthyears[ind] = list(2019-children_ages)
      if (df$age_of_oldest_child[ind] < 18) {
        df$n_children[ind] = df$total_children[ind]
      } else if (df$age_of_youngest_child[ind] >= 18) {
        df$n_children[ind] = 0
      } else {
        df$n_children[ind] = sum(children_ages < 18)
        df$children_randomly_allocated[ind] = TRUE
      }
    }
    
    pb$tick()
    
  }
  
  return(df)
  
}



#' Resamples children's ages for fathers with uncertain child distributions
#'
#' Only modifies rows where child ages were randomly allocated in deriveChildrenAges().
#'
#' @param df A dataframe that has been processed by deriveChildrenAges().
#' @return The dataframe with resampled child ages.
resampleChildrenAges = function(df) {
  
  inds = which(df$children_randomly_allocated)
  
  pb = progress_bar$new(format="Resampling uncertain children's ages... :bar :percent eta: :eta", total=length(inds), clear=FALSE, width=100)
  for (ind in inds) {
    
    children_ages_rnd = sample(df$age_of_youngest_child[ind]:df$age_of_oldest_child[ind], df$total_children[ind]-2, replace=T)
    children_ages = c(df$age_of_youngest_child[ind], df$age_of_oldest_child[ind], children_ages_rnd)
    
    df$children_ages[ind] = list(children_ages)
    df$children_birthyears[ind] = list(2019-children_ages)
    df$n_children[ind] = sum(children_ages <= 18)
    df$children_randomly_allocated[ind] = TRUE
    
    pb$tick()
    
  }
  
  return(df)
  
}



#' Constructs a survey design object from PNS data
#'
#' Applies survey weights and filters for adult males selected for detailed questions.
#'
#' @param df A dataframe containing PNS survey data.
#' @return A survey design object for male fertility analysis.
createPNSSurveyDesign = function(df) {
  
  svydesign  = df %>%
    pns_design() %>% # Creates the survey design object
    subset(is_adult_male) %>% # Subsets by those that have the potential to be a father
    subset(selected_for_individual=="1") %>% # Only those selected to answer more detailed questions answer the relevant section
    subset(is_father %in% c("1", "2")) # And this ensures that they either said yes or no to being a father
  
  return(svydesign)
  
}



#' Prints a summary of PNS survey data
#'
#' Displays summary statistics on the number of fathers, missing responses, and sample sizes.
#'
#' @param df A dataframe containing PNS survey data.
printPNSSummary = function(df) {
  
  df_temp = df %>% filter(is_adult_male) %>% filter(selected_for_individual=="1")
  print(paste0("No. of men aged 15+ that responded to individual questionaiire: ", nrow(df_temp)))
  
  print(paste0("No. saying don't know: ", sum(df_temp$is_father == "3", na.rm=TRUE)))
  print(paste0("No. ignored: ", sum(df_temp$is_father == "9", na.rm=TRUE)))
  print(paste0("No. NA: ", sum(is.na(df_temp$is_father))))
  
  df_temp = df_temp %>% filter(!is.na(is_father)) %>% filter(is_father %in% c("1", "2"))
  
  print(paste0("Leaving a sample size of: ", nrow(df_temp)))
  
  print(paste0("No. of possible fathers with at least one child: ", nrow(df_temp %>% filter(total_children>=1))))
  print(paste0("No. of possible fathers with at least three children: ", nrow(df_temp %>% filter(total_children>=3))))
  df_temp2 = df_temp %>% filter(total_children >= 3) %>% filter(age_of_youngest_child <= 18) %>% filter(age_of_oldest_child > 18)
  print(paste0("No. of parents with children of uncertain ages: ", nrow(df_temp2)))
  
}


#' Load child mortality data and compute survival probabilities
#'
#' Reads life tables, extracts child mortality rates, and calculates probabilities of survival up to age 17.
#'
#' @return A dataframe with child ages and survival probabilities.
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

#' Adjusts male fertility estimates for child mortality
#'
#' Applies survival probabilities to correct for child mortality, either at the individual or aggregate level.
#'
#' @param df_fertility A dataframe with male fertility estimates.
#' @param by_child_age Logical; if TRUE, adjusts estimates at the child-age level.
#' @return A dataframe with mortality-adjusted fertility estimates.
adjustMaleFertilityForChildMortality = function(df_fertility, by_child_age) {
  
  childmortality = loadChildMortalityData() %>% rename(child_age=age)
  
  if (by_child_age) {
    
    df_out = df_fertility %>% left_join(childmortality, by="child_age")
    df_out$n_child = rbinom(n=nrow(df_out), size=df_out$n_child, prob=df_out$p_survive)
    
  } else {
    
    # Calculate aggregated survival rates by uf
    df_popn = read.csv("data/population/popests_simple_2010_2020.csv") %>%
      filter(age_group %in% paste0(seq(0, 17)), year==2020, !is.na(uf), sex!="both") %>%
      rename(child_age=age_group) %>%
      mutate(child_age=as.numeric(child_age)) %>%
      group_by(uf) %>%
      mutate(p=n_pop/sum(n_pop)) %>%
      left_join(childmortality, by="child_age") %>%
      group_by(uf) %>%
      summarise(p_survive = sum(p*p_survive))
    
    df_out = df_fertility %>% mutate(uf=as.numeric(as.character(uf))) %>% left_join(df_popn, by="uf")
    df_out$n_child = rbinom(n=nrow(df_out), size=df_out$n_child, prob=df_out$p_survive)
    
  }
  
  return(df_out)
  
}



