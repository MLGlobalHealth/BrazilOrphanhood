
library(foreach)
library(progress)
library(parallel)
library(doParallel)

source("R/supportFunctions.R")
source("R/sampleOrphanhoodHelpers.R")

options(dplyr.summarise.inform = FALSE)


#' Generate samples of parental orphanhood using fertility and mortality samples
#'
#' Estimates the number of children orphaned due to parental mortality.
#'
#' @param N Number of samples to generate (default: 1000).
#' @param mortality Type of mortality to use: "excess", "allcause", or "covid" (default: "excess").
#' @param by_child_age Logical; if TRUE, estimates orphanhood disaggregated by child age (default: FALSE).
#' @param current_child_age If by_child_age is TRUE, specifies the child age to disaggregate by (default: NA).
#' @return A dataframe containing orphanhood estimates.
sampleParentalOrphanhood = function(N=1000,
                                    mortality="excess",
                                    by_child_age=FALSE,
                                    current_child_age=NA) {
  
  # Specify and sort group_vars
  group_vars = sort(c("age_group", "sex", "uf"))
  if (by_child_age) {
    fertility_vars = sort(c("age_group", "uf", "child_age"))
  } else {
    fertility_vars = sort(c("age_group", "uf"))
  }
  
  # Load mortality data
  if (mortality=="excess") {
    df_em = readRDS(paste0("samples/excessmortality_", paste0(group_vars, collapse="_"), ".RDS")) %>% filter(age_group != "0-9")
  } else if (mortality == "allcause") {
    df_em = readRDS(paste0("samples/allcausemortality_", paste0(group_vars, collapse="_"), ".RDS")) %>% filter(age_group != "0-9")
  } else if (mortality == "covid") {
    stop("COVID-19-specific mortality is no-longer supported.")
    df_em = readRDS(paste0("samples/covidmortality_", paste0(group_vars, collapse="_"), ".RDS")) %>% filter(age_group != "0-9")
  } else {
    stop("Invalid mortality type. Must be one of 'excess', 'allcause', or 'covid'.")
  }
  
  # Load and join fertility data
  df_female_fertility = readRDS(paste0("samples/femalefertility_", paste0(fertility_vars, collapse="_"), ".RDS")) %>% mutate(sex="female") %>% select(-mean_child)
  df_male_fertility = readRDS(paste0("samples/malefertility_", paste0(fertility_vars, collapse="_"), ".RDS")) %>% mutate(sex="male")
  df_male_fertility$uf = as.numeric(as.character(df_male_fertility$uf))
  
  if (by_child_age) {
    df_female_fertility = df_female_fertility %>% ungroup() %>% filter(child_age==current_child_age) %>% select(-child_age)
    df_male_fertility = df_male_fertility %>% ungroup() %>% filter(child_age==current_child_age) %>% select(-child_age)
  }
  
  # Remove the 0-9 year-old age-group
  df_em = df_em %>% filter(age_group!="0-9")
  df_female_fertility = df_female_fertility %>% filter(age_group!="0-9")
  df_male_fertility = df_male_fertility %>% filter(age_group!="0-9")
  
  # Load other data (these use support functions from doubleOrphanhoodHelpers.R)
  df_partner_age_dist = loadPartnerAgeDist(regional=TRUE)
  df_popn = loadPopnDataForOrphanhood(group_vars)
  
  # Get maximum iteration counts for each set of samples
  N_em = max(df_em$iter)
  N_f = max(df_female_fertility$iter)
  N_m = max(df_male_fertility$iter)
  
  print("Starting main sampler in parallel...")
  st_time = Sys.time()
  
  # And do the sampling
  pb = progress_bar$new(format="Sampling orphanhood... :bar :percent eta: :eta", total=N, clear=FALSE, width=100)
  df_results = foreach(ii=seq(1, N),
                       .combine=rbind,
                       .packages=c("tidyverse", "survey", "foreach"),
                       .export=c("sampleExcessDeathsByPartnerAge", "determinePartnerDeaths")) %dopar%
    {
      
      # Choose indices to sample from
      ii_em = sample(1:N_em, 1)
      ii_f = sample(1:N_f, 1)
      ii_m = sample(1:N_m, 1)
      
      df_em_sub = df_em %>% filter(iter==ii_em) %>% select(-iter) %>% ungroup()
      df_ff_sub = df_female_fertility %>% filter(iter==ii_f) %>% select(-iter) %>% ungroup()
      df_mf_sub = df_male_fertility %>% filter(iter==ii_m) %>% select(-iter) %>% ungroup()
      
      # Sample orphanhood for each group
      # The following foreach() loop implements the procedure defined in the "Estimating parental orphanhood" section of the supplementary material
      df_orphanhood = foreach(jj=seq(1, nrow(df_em_sub)), .combine=rbind) %do% {
        
        if (df_em_sub$sex[jj] == "female") {
          df_fertility_sub = df_ff_sub %>% filter(age_group==df_em_sub$age_group[jj], uf==df_em_sub$uf[jj])
        } else {
          df_fertility_sub = df_mf_sub %>% filter(age_group==df_em_sub$age_group[jj], uf==df_em_sub$uf[jj])
        }
        
        indices = sample(1:nrow(df_fertility_sub), df_em_sub$excess_deaths[jj], replace=TRUE)
        
        df_tmp = data.frame(age_group=df_em_sub$age_group[jj],
                            uf=df_em_sub$uf[jj],
                            sex=df_em_sub$sex[jj],
                            excess_deaths=df_em_sub$excess_deaths[jj],
                            orphanhood = sum(df_fertility_sub$n_child[indices]))
        
        return(df_tmp)
        
      }
      
      # The remaining code in this outer loop implements the procedure defined in the "Estimating double orphanhood" section of the supplementary material
      
      # First sample the number of excess deaths that occurred in women with partners of a given age-group
      df_em_by_partner_age = sampleExcessDeathsByPartnerAge(setdiff(group_vars, "sex"), df_em_sub, df_partner_age_dist)
      # then determine how many of these partners died
      df_doubleexcess = determinePartnerDeaths(group_vars, df_em_by_partner_age, df_em_sub, df_popn) # This is where we can perform sensitivity analysis on double orphanhood
      # and finally calculate how many children these double-deaths-partners had
      df_doubleorphanhood = foreach(jj=seq(1, nrow(df_doubleexcess)), .combine=rbind) %do% {
        
        df_fertility_sub = df_ff_sub %>% filter(age_group==df_doubleexcess$age_group[jj],
                                                uf==df_doubleexcess$uf[jj])
        
        indices = sample(1:nrow(df_fertility_sub), df_doubleexcess$n_both_died[jj], replace=TRUE)
        
        df_tmp = data.frame(age_group=df_doubleexcess$age_group[jj],
                            partner_age_group=df_doubleexcess$partner_age_group[jj],
                            sex="both",
                            uf=df_doubleexcess$uf[jj],
                            n_both_died=df_doubleexcess$n_both_died[jj],
                            orphanhood = sum(df_fertility_sub$n_child[indices]))
        
        return(df_tmp)
        
      }
      
      # Remove the double-ups
      df_doublefemale = df_doubleorphanhood %>% group_by_at(setdiff(group_vars, "sex")) %>% summarise(n_double = sum(orphanhood), .groups="drop") %>% mutate(sex="female")
      df_doublemale = df_doubleorphanhood %>% group_by_at(union(setdiff(group_vars, c("age_group", "sex")), "partner_age_group")) %>% summarise(n_double=sum(orphanhood), .groups="drop") %>% mutate(sex="male") %>% rename(age_group=partner_age_group)
      df_orphanhood = df_orphanhood %>% left_join(rbind(df_doublefemale, df_doublemale), by=group_vars) %>% mutate(orphanhood = orphanhood - n_double) %>% select(-n_double)
      
      # And finally append the doubleorphanghood rows
      df_out = rbind(df_orphanhood %>% mutate(partner_age_group=NA), df_doubleorphanhood %>% rename(excess_deaths=n_both_died)) %>% mutate(iter=ii)
      
      pb$tick()
      return(df_out)
      
    }
  
  en_time = Sys.time()
  print(paste0("Completed in ", round(en_time-st_time, digits=2), " time units"))
  
  # Write files to the /samples/ folder
  if (by_child_age) {
    fname = paste0("samples/interim/orphanhood_", paste0(group_vars, collapse="_"), "_", mortality, "_CHILDAGE", current_child_age, ".RDS")
  } else {
    fname = paste0("samples/orphanhood_", paste0(group_vars, collapse="_"), "_", mortality, ".RDS")
  }
  saveRDS(df_results, fname)
  
  return(df_results)
  
}
