
# These functions estimate annual age-specific male fertility rates instead of the standard mean number of children

library(tidyverse)
library(zoo)
library(foreach)
library(survey)
library(PNSIBGE)

source("R/sampleMaleFertility.R")

options(dplyr.summarise.inform = FALSE)


#' Estimate annual age-specific male fertility rates
#'
#' Computes age-specific fertility rates for men using PNS survey data, 
#' either nationally or at the regional level.
#'
#' @param regional Logical; if TRUE, estimates fertility rates at the regional level (default: FALSE).
#' @param outdir Directory to save the results (default: "samples/").
#' @param savefile Logical; if TRUE, saves results to outdir (default: TRUE).
#' @return A dataframe with annual age-specific male fertility estimates.
estimateMaleAgeSpecificFertilityRates = function(regional=FALSE,
                                               outdir="samples/",
                                               savefile=TRUE) {
  
  # Setup
  birth_years = seq(2003,2020)
  if (regional) {
    group_vars = c("age_group", "uf")
  } else {
    group_vars = c("age_group")
  }
  
  # Load and process data
  print("Loading PNS data...")
  df = readRDS("data/pns2019/pns2019_processed.rds")
  printPNSSummary(df)
  
  # Derive the initial number of children
  print("Deriving number of children (first pass)...")
  df = df %>% deriveChildrenAges() %>% appendAgeGroups(agecolname="age")
  
  # Get table of all unique combinations of our group vars (either age_group, or age_group and uf)
  unique_vals_in_cols = lapply(group_vars, function(x) unique(df[[x]]))
  df_combinations = do.call(expand.grid, unique_vals_in_cols)
  colnames(df_combinations) = group_vars
  
  # Iterate over birth years
  pb = progress_bar$new(format="Calculating age-specific male fertility rates... :bar :percent eta: :eta", total=length(birth_years), clear=FALSE, width=100)
  df_results = foreach(ii=1:length(birth_years), .combine=rbind) %do% {
    
    current_birth_year = birth_years[ii]
    
    # Append age-groups
    df_current = df %>%
      mutate(age = age - (2019-current_birth_year),
             n_children_in_year = sapply( , function(x) sum(x == current_birth_year))) %>%
      appendAgeGroups(agecolname="age")
      
    # Construct survey design object
    svydes = createPNSSurveyDesign(df_current)
    
    # Iterate over all combinations of group_vars
    df_year = foreach(jj=1:nrow(df_combinations), .combine=rbind) %do% {
      
      # Subset appropriately
      svydesign_sub = svydes
      for (var in group_vars) {
        svydesign_sub = svydesign_sub %>% subset(get(var)==df_combinations[jj,var])
      }
      
      # Calculate survey outputs
      est = svymean(~n_children_in_year, svydesign_sub, na.rm=TRUE)
      
      df_out = data.frame(age_group = df_combinations[jj,"age_group"],
                          birth_year=current_birth_year,
                          central = est[[1]],
                          lower = confint(est)[[1]],
                          upper = confint(est)[[2]],
                          n = sum(weights(svydesign_sub)>0))
      
      if (regional) {
        df_out[,"uf"] = df_combinations[jj,"uf"]
      }
      
      return(df_out)
      
    }
    
    pb$tick()
    return(df_year)
    
  }
  
  
  # Write files to the OUTDIR
  if (savefile) {
    if (regional){
      fname = paste0(outdir, "malefertility_annualagespecific_regional.RDS")
    } else {
      fname = paste0(outdir, "malefertility_annualagespecific.RDS") 
    }
    saveRDS(df_results, fname)
  }

  return(df_results)

}

  