
library(tidyverse)
library(zoo)
library(foreach)
library(progress)

options(dplyr.summarise.inform = FALSE)

#' Generate samples of excess mortality
#' 
#' Samples are computed using Bayesian estimates and saved with filenames based on grouping variables.
#'
#' @param group_vars Character vector specifying grouping variables (default: c("uf", "age_group", "sex")).
#' @param modifier_pre Function applied to excess mortality samples before aggregation (default: pmax(x, 0)).
#' @param modifier_post Function applied to excess mortality samples after aggregation (default: identity).
#' @param outdir Directory to save the samples (default: "samples/").
#' @param savefile Logical; if TRUE, saves samples to outdir.
#' @return A dataframe of excess mortality samples grouped by the specified variables.
sampleExcessMortality = function(group_vars=c("uf", "age_group", "sex"),
                                 modifier_pre=(function(x) pmax(x, 0)),
                                 modifier_post=(function(x) x),
                                 outdir="samples/",
                                 savefile=TRUE) {
  
  # Setup
  df = read.csv("data/excessmortality/excess_mortality_bayesian.csv") # Load data
  group_vars = sort(group_vars) # Arrange group_vars so file names are consistent
  n_samples = sum(grepl("^V", names(df)))
  
  # Iterate over all samples
  pb = progress_bar$new(format="Sampling excess mortality... :bar :percent eta: :eta", total=n_samples, clear=FALSE, width=100)
  df_out = foreach(ii = seq(1, n_samples), .combine="rbind") %do% {
    
    # Apply pre-processing
    df$excess_deaths = modifier_pre(df$deaths - df[, paste0("V", ii) ])
    
    # Select columns and sum deaths within each group
    df_tmp = df %>%
      select_at(c(group_vars, "excess_deaths")) %>%
      group_by_at(group_vars) %>%
      summarise(excess_deaths = sum(excess_deaths)) %>%
      mutate(iter = ii)
    
    # Increment progress bar and return current samples
    pb$tick()
    return(df_tmp)
    
  }
  
  # Perform post-processing and save
  df_out$excess_deaths = sapply(df_out$excess_deaths, modifier_post)
  if (savefile) {
    saveRDS(df_out, paste0(outdir, "excessmortality_", paste0(group_vars, collapse="_"), ".RDS"))
  }
  
  return(df_out)
  
}



#' Load and save all-cause mortality
#' 
#' Saves in the same format as excess mortality, with deaths incorrectly labeled as "excess_deaths".
#'
#' @param group_vars Character vector specifying grouping variables (default: c("uf", "age_group", "sex")).
#' @param modifier_pre Function applied to deaths before aggregation (default: pmax(x, 0)).
#' @param modifier_post Function applied to deaths after aggregation (default: identity).
#' @param outdir Directory to save the results (default: "samples/").
#' @param savefile Logical; if TRUE, saves results to outdir.
#' @return A dataframe of all-cause mortality grouped by the specified variables.
loadAllCauseMortality = function(group_vars=c("uf", "age_group", "sex"),
                                 modifier_pre=(function(x) pmax(x, 0)),
                                 modifier_post=(function(x) x),
                                 outdir="samples/",
                                 savefile=TRUE) {
  
  # Setup
  df = read.csv("data/excessmortality/excess_mortality_bayesian.csv")
  group_vars = sort(group_vars) # Arrange group_vars so file names are consistent
  
  # Apply pre-processing
  df$excess_deaths = modifier_pre(df$deaths)
  
  # Select the relevant columns, group by chosen variables, and aggregate
  df_out = df %>%
    select_at(c(group_vars, "excess_deaths")) %>%
    group_by_at(group_vars) %>%
    summarise(excess_deaths = sum(excess_deaths)) %>%
    mutate(iter = 1)
  
  # Apply post-processing and save
  df_out$excess_deaths = sapply(df_out$excess_deaths, modifier_post)
  if (savefile) {
    saveRDS(df_out, paste0(outdir, "allcausemortality_", paste0(group_vars, collapse="_"), ".RDS"))
  }
  
  return(df_out)
  
}



#' Load and save monthly excess mortality
#' 
#' NOTE: Uncertainty is unavailable for monthly mortality.
#' Saves in the same format as excess mortality, with deaths incorrectly labeled as "excess_deaths".
#'
#' @param group_vars Character vector specifying grouping variables (default: c("uf", "age_group", "sex")).
#' @param modifier_pre Function applied to excess mortality before aggregation (default: pmax(x, 0)).
#' @param modifier_post Function applied to excess mortality after aggregation (default: identity).
#' @param outdir Directory to save the results (default: "samples/").
#' @param savefile Logical; if TRUE, saves results to outdir.
#' @return A dataframe of monthly excess mortality grouped by the specified variables.
loadMonthlyExcessMortality = function(group_vars=c("uf", "age_group", "sex"),
                                      modifier_pre=(function(x) pmax(x, 0)),
                                      modifier_post=(function(x) x),
                                      outdir="samples/",
                                      savefile=TRUE) {
  
  # Load data
  df = read.csv("data/excessmortality/excess_mortality_frequentist.csv")
  df$date = as.yearmon(paste0(df$year, "-", df$month))
  group_vars = sort(union(group_vars, "date")) # Ensure that date is included as a grouped variable
  
  # Apply pre-processing
  df$excess_deaths = modifier_pre(df$deaths - df[,"fitted_deaths"])
  
  # Select the relevant columns, group by chosen variables, and aggregate
  df_out = df %>%
    select_at(c(group_vars, "excess_deaths")) %>%
    group_by_at(group_vars) %>%
    summarise(excess_deaths = sum(excess_deaths)) %>%
    mutate(iter = 1)
  
  # Apply post-processing and save
  df_out$excess_deaths = sapply(df_out$excess_deaths, modifier_post)
  if (savefile) {
    saveRDS(df_out, paste0(outdir, "excessmortality_", paste0(group_vars, collapse="_"), ".RDS"))
  }
  return(df_out)
  
}



#' Load and save monthly all-cause mortality
#' 
#' NOTE: Uncertainty is unavailable for monthly mortality.
#' Saves in the same format as excess mortality, with deaths incorrectly labeled as "excess_deaths".
#'
#' @param group_vars Character vector specifying grouping variables (default: c("uf", "age_group", "sex")).
#' @param modifier_pre Function applied to mortality data before aggregation (default: pmax(x, 0)).
#' @param modifier_post Function applied to mortality data after aggregation (default: identity).
#' @param outdir Directory to save the results (default: "samples/").
#' @param savefile Logical; if TRUE, saves results to outdir.
#' @return A dataframe of monthly all-cause mortality grouped by the specified variables.
loadMonthlyAllCauseMortality = function(group_vars=c("uf", "age_group", "sex"),
                                        modifier_pre=(function(x) pmax(x, 0)),
                                        modifier_post=(function(x) x),
                                        outdir="samples/",
                                        savefile=TRUE) {
  
  # Load data
  df = read.csv("data/excessmortality/excess_mortality_frequentist.csv")
  df$date = as.yearmon(paste0(df$year, "-", df$month))
  group_vars = sort(union(group_vars, "date")) # Ensure that date is included as a grouped variable
  
  # Apply pre-processing
  df$excess_deaths = modifier_pre(df$deaths)
  
  # Select the relevant columns, group by chosen variables, and aggregate
  df_out = df %>%
    select_at(c(group_vars, "excess_deaths")) %>%
    group_by_at(group_vars) %>%
    summarise(excess_deaths = sum(excess_deaths)) %>%
    mutate(iter = 1)
  
  # Apply post-processing and save
  df_out$excess_deaths = sapply(df_out$excess_deaths, modifier_post)
  if (savefile) {
    saveRDS(df_out, paste0(outdir, "allcausemortality_", paste0(group_vars, collapse="_"), ".RDS"))
  }
  return(df_out)
  
}

