
library(foreach)
library(tidyverse)
library(PNSIBGE)
library(survey)

# ------------------------------------------------------------------------------
# ---------- Functions that load other data ------------------------------------
# ------------------------------------------------------------------------------


#' Load partner age distribution data
#'
#' Reads the partner age distribution file, either at a national or regional level.
#'
#' @param regional Logical; if TRUE, loads the regional partner age distribution.
#' @return A dataframe containing partner age distribution probabilities.
loadPartnerAgeDist = function(regional=FALSE) {
  
  # Load raw file and tidy up
  if (!regional) {
    df_partner = read.csv("samples/partneragedist.csv") %>%
      select(mother_age_group, partner_age_group, p) %>%
      rename(age_group=mother_age_group) 
  } else {
    df_partner = read.csv("samples/partneragedist_regional.csv") %>%
      select(mother_age_group, partner_age_group, p, uf) %>%
      rename(age_group=mother_age_group) 
    
  }
  
  return(df_partner)
  
}

#' Load population estimates for orphanhood calculations
#'
#' Reads population estimates and groups by specified variables.
#'
#' @param group_vars Character vector specifying variables for disaggregation.
#' @return A dataframe with population counts.
loadPopnDataForOrphanhood = function(group_vars) {
  
  df_popn = read.csv("data/population/popests_simple_2010_2020.csv") %>%
    filter(year==2020, !is.na(uf), sex!="both", age_group!="TOTAL") %>%
    rename(age=age_group) %>%
    mutate(age=as.numeric(case_when(age=="90+"~"90", TRUE ~ age))) %>%
    appendAgeGroups() %>%
    group_by_at(group_vars) %>%
    summarise(n_pop=sum(n_pop), .groups="drop")
  
  return(df_popn)
  
}




# ------------------------------------------------------------------------------
# ---------- Functions that are useful for double orphanhood calculations ------
# ------------------------------------------------------------------------------


#' Sample excess deaths by partner age
#'
#' Uses age distribution of female partners to estimate excess deaths by partner age.
#'
#' @param group_vars_tmp Character vector specifying grouping variables.
#' @param df_em_sub Dataframe of excess mortality estimates.
#' @param df_partner_age_dist Dataframe with partner age distribution probabilities.
#' @return A dataframe with excess deaths by partner age.
sampleExcessDeathsByPartnerAge = function(group_vars_tmp, df_em_sub, df_partner_age_dist) {
  
  # First we link excess mortality estimates for women by age-group with the age-distribution of their partner
  # df_em_by_partner_age = df_partner_age_dist %>% left_join(df_em_sub %>% filter(sex=="female") %>% select(-sex), by=setdiff(group_vars_tmp, "date"), relationship="many-to-many")
  df_em_by_partner_age = df_partner_age_dist %>% left_join(df_em_sub %>% filter(sex=="female") %>% select(-sex), by=setdiff(group_vars_tmp, "date"))
  
  # We need to sample from a multinomial distribution for each unique combination of group_vars_tmp, we begin by finding all such unique combinations
  unique_vals_in_cols = lapply(group_vars_tmp, function(x) unique(df_em_by_partner_age[[x]]))
  df_combinations = do.call(expand.grid, unique_vals_in_cols)
  colnames(df_combinations) = group_vars_tmp
  
  # And now we iterate over all of these combinations
  df_result = foreach (jj=seq(1, nrow(df_combinations)), .combine=rbind) %do% {
    
    # Subset the data according to the current combination
    df_current = df_em_by_partner_age
    for (var in group_vars_tmp) {
      df_current = df_current %>% filter(get(var)==df_combinations[jj,var])
    }
    
    # Check that first and last excess deaths match
    if (df_current$excess_deaths[1] != df_current$excess_deaths[nrow(df_current)]) {
      stop("ERROR: EXCESS DEATHS DO NOT MATCH")
    }
    
    # Check that we have the expected number of rows
    if (nrow(df_current) != length(unique(df_em_by_partner_age$age_group))) {
      stop("ERROR: UNEXPECTED NUMBER OF ROWS")
    }
    
    # Sample from the distribution
    sample = rmultinom(1, df_current$excess_deaths[1], df_current$p)
    
    # Save our results
    df_tmp = data.frame(age_group=df_combinations$age_group[jj],
                        partner_age_group=df_current$partner_age_group,
                        p = df_current$p,
                        excess_deaths = sample)
    
    if ("date" %in% group_vars_tmp) {
      df_tmp$date = df_combinations$date[jj]
    }
    
    # Append details about the current group
    for (var in group_vars_tmp) {
      df_tmp[,var] = df_combinations[jj,var]
    }
    
    return(df_tmp)
    
  }
  
}


#' Determine the number of partners who also died
#'
#' Estimates the probability that a male partner of a deceased female also died.
#'
#' @param group_vars_tmp Character vector specifying grouping variables.
#' @param df_em_by_partner_age Dataframe with excess deaths by partner age.
#' @param df_em_sub Dataframe of excess mortality estimates.
#' @param df_popn Dataframe with population estimates.
#' @return A dataframe with estimated double deaths.
determinePartnerDeaths = function(group_vars_tmp, df_em_by_partner_age, df_em_sub, df_popn) {
  
  # Estimate the probability that a male of a given age died an excess death
  df_pdied = left_join(df_em_sub, df_popn, by=setdiff(group_vars_tmp, "date")) %>%
    mutate(p_partner_died = excess_deaths/n_pop) %>%
    rename(partner_age_group=age_group) %>%
    filter(sex=="male") %>%
    select(-sex, -n_pop, -excess_deaths)
  
  # We need to join by group_vars_tmp, but instead of age_group its partner_age_group
  join_vars_tmp = union(setdiff(group_vars_tmp, c("age_group", "sex")), c("partner_age_group"))
  
  # Join the probability of death onto the main dataframe and sample the number of cases where father also died
  df_result = left_join(df_em_by_partner_age, df_pdied, by=join_vars_tmp) %>%
    mutate(n_both_died = rbinom(nrow(df_em_by_partner_age), excess_deaths, p_partner_died)) %>%
    select_at(union(c("age_group", "p", "excess_deaths", "p_partner_died", "n_both_died"), join_vars_tmp))
  
  # Re-order (just to keep things tidy)
  first_cols = c("age_group", "partner_age_group")
  middle_cols = c(setdiff(group_vars_tmp, c(first_cols, "sex")), "p", "excess_deaths", "p_partner_died", "n_both_died")
  last_cols = setdiff(colnames(df_result), c(first_cols, middle_cols))
  df_result = df_result %>% select_at(c(first_cols, middle_cols, last_cols))
  
  return(df_result)
  
}



#' Calculate double orphanhood estimates
#'
#' Computes the number of children orphaned by the simultaneous death of both parents.
#'
#' @param group_vars_tmp Character vector specifying grouping variables.
#' @param df_doubleexcess Dataframe with estimated double deaths.
#' @param df_fertility_sub Dataframe with fertility estimates.
#' @return A dataframe with double orphanhood estimates.
calculateDoubleOrphanhood = function(group_vars_tmp, df_doubleexcess, df_fertility_sub) {
  
  # Calculate alpha for each group
  unique_vals_in_cols = lapply(group_vars_tmp, function(x) unique(df_doubleexcess[[x]])) # TODO: CHECK THAT THIS ITERATES OVER THE CORRECT COLUMNS!!!
  df_combinations = do.call(expand.grid, unique_vals_in_cols)
  colnames(df_combinations) = group_vars_tmp
  
  # Iterate over all combinations to estimate joint-fertility
  df_assigned_fertility = foreach(jj=seq(1,nrow(df_combinations)), .combine=rbind) %do% {
    
    # Subset to current group
    df_current = df_doubleexcess
    for (var in group_vars_tmp) {
      df_current = df_current %>% filter(get(var)==df_combinations[jj,var])
    }
    
    # Append male fertility rates
    male_joining_vars = union(setdiff(group_vars_tmp, c("age_group", "date")), "partner_age_group")
    df_fertility_male = df_fertility_sub %>% filter(sex=="male") %>% rename(partner_age_group=age_group) %>% select(-sex) %>% rename(father_mean_child=mean_child)
    df_current = df_current %>% left_join(df_fertility_male, by=male_joining_vars)
    
    # Append female fertility rate
    female_joining_vars = setdiff(group_vars_tmp, "date")
    df_fertility_female = df_fertility_sub %>% filter(sex=="female") %>% select(-sex) %>% rename(mother_mean_child=mean_child)
    df_current = df_current %>% left_join(df_fertility_female, by=female_joining_vars)
    
    # Calculate adjustment factor and multiply
    df_current$adjustment_factor = df_current$father_mean_child/sum(df_current$p * df_current$father_mean_child)
    df_current$mean_child = df_current$adjustment_factor * df_current$mother_mean_child
    
    return(df_current)
    
  }
  
  # And calculate orphanhood
  select_vars = c("age_group", "partner_age_group", "sex", "uf", "date", "n_both_died", "mean_child", "orphanhood")
  if (!"date" %in% group_vars_tmp) { select_vars = setdiff(select_vars, "date")}
  df_doubleorphanhood = df_assigned_fertility %>% mutate(orphanhood = n_both_died * mean_child, sex="both") %>%
    select_at(select_vars) %>%
    rename(excess_deaths = n_both_died)
  
  return(df_doubleorphanhood)
  
}



#' Calculate double orphanhood disaggregated by child age
#'
#' Computes double orphanhood estimates while accounting for child ages.
#'
#' @param df_doubleexcess Dataframe with estimated double deaths.
#' @param df_fertility_sub Dataframe with fertility estimates.
#' @return A dataframe with double orphanhood estimates by child age.
calculateDoubleOrphanhoodByChildAge = function(df_doubleexcess, df_fertility_sub) {
  
  # Calculate alpha for each group
  unique_vals_in_cols = lapply(c("uf", "age_group"), function(x) unique(df_doubleexcess[[x]]))
  df_combinations = do.call(expand.grid, unique_vals_in_cols)
  colnames(df_combinations) = c("uf", "age_group")
  
  # Iterate over all combinations to estimate joint-fertility
  df_assigned_fertility = foreach(jj=seq(1,nrow(df_combinations)), .combine=rbind) %do% {
    
    # Subset to current group
    df_current = df_doubleexcess
    df_current = df_current %>% filter(uf==df_combinations$uf[jj])
    df_current = df_current %>% filter(age_group==df_combinations$age_group[jj])
    
    # Append male fertility rates
    df_fertility_male_tmp = df_fertility_sub %>%
      filter(sex=="male", uf==df_combinations$uf[jj]) %>%
      rename(partner_age_group=age_group, father_mean_child=mean_child) %>% # Ensure we join on the father/partner's variables
      select(-sex)
    df_current = df_current %>% left_join(df_fertility_male_tmp, by=c("partner_age_group", "uf"))
    
    # Append female fertility rate
    df_fertility_female_tmp = df_fertility_sub %>%
      filter(sex=="female", uf==df_combinations$uf[jj]) %>%
      rename(mother_mean_child=mean_child) %>%
      select(-sex)
    df_current = df_current %>% left_join(df_fertility_female_tmp, by=c("age_group", "uf", "child_age"))
    
    # Calculate adjustment factor and multiply
    df_current = df_current %>%
      group_by(child_age) %>%
      mutate(mean_child = mother_mean_child * father_mean_child/sum(p * father_mean_child))
    
    
    return(df_current)
    
  }
  
  # And calculate orphanhood
  select_vars = c("age_group", "partner_age_group", "sex", "uf", "child_age", "n_both_died", "mean_child", "orphanhood")
  df_doubleorphanhood = df_assigned_fertility %>% mutate(orphanhood = n_both_died * mean_child, sex="both") %>%
    select_at(select_vars) %>%
    rename(excess_deaths = n_both_died)
  
  return(df_doubleorphanhood)
  
}




# ------------------------------------------------------------------------------
# ---------- Function for post-processing of results ---------------------------
# ------------------------------------------------------------------------------

#' Aggregate orphanhood estimates by child age
#'
#' Combines results from separate child-age-specific orphanhood estimates into a single file.
#'
#' @param mortality Character; either "excess" or "allcause".
#' @param indir Input directory containing child-age-specific results.
#' @param outdir Output directory to save the aggregated results.
#' @return A dataframe with aggregated orphanhood estimates.
aggregateChildAgeSamples = function(mortality="excess",
                                    indir="samples/orphanhoodByChildAge/",
                                    outdir = "samples/"){
  
  fnameroot = case_when(mortality=="excess" ~ "orphanhood_age_group_sex_uf_CHILDAGE",
                        mortality=="allcause" ~ "orphanhood_age_group_sex_uf_ALLCAUSE_CHILDAGE")
  
  fnameout = case_when(mortality=="excess" ~ "orphanhood_age_group_child_age_sex_uf.RDS",
                       mortality=="allcause" ~ "orphanhood_age_group_child_age_sex_uf_ALLCAUSE.RDS")
  
  df_out = foreach (child_age = seq(0,17), .combine=rbind) %do% {
    df_in = readRDS(paste0(indir, fnameroot, child_age, ".RDS")) %>% mutate(child_age=child_age)
  }
  
  saveRDS(df_out, paste0(outdir, fnameout))
  
}




# ------------------------------------------------------------------------------
# ---------- Functions for estimating partner age distributions ----------------
# ------------------------------------------------------------------------------


#' Estimate partner age distribution from PNS survey data
#'
#' Computes partner age probabilities using PNS survey responses.
#'
#' @param regional Logical; if TRUE, estimates regional distributions.
#' @param OUTDIR Directory to save results.
#' @param SAVEFILE Logical; if TRUE, saves results to file.
#' @return A dataframe with partner age distribution estimates.
getPartnerAgeDist = function(regional=FALSE, OUTDIR="samples/", SAVEFILE=TRUE) {
  
  # Load PNS data
  df_raw = readRDS("data/pns2019/pns2019_processed.rds")
  
  # Append age-groups and age-groups of female partners
  df = appendFemalePartnersAges(df_raw)
  
  # Create primary survey design object
  svydesign = pns_design(df) %>% subset(use_for_partner_dist) %>% subset(!is.na(partner_age_group))
  df_mainsub = df %>% filter(use_for_partner_dist, !is.na(partner_age_group))
  
  # Create combination table depending on whether or not we want regional estimates
  if (regional) {
    df_combinations = na.omit(expand.grid(mother_age_group=unique(df$mother_age_group),
                                          partner_age_group=unique(df$partner_age_group),
                                          uf=unique(df$uf)))
  } else {
    df_combinations = na.omit(expand.grid(mother_age_group=unique(df$mother_age_group),
                                          partner_age_group=unique(df$partner_age_group)))
  }
  
  # And estimate for each combination
  df_results = foreach(ii = 1:nrow(df_combinations), .combine=rbind) %do% {
    
    print(paste0("Running combination ", ii, " of ", nrow(df_combinations)))
    
    # Subset the survey design and data according to the current combination
    svydesign_sub = svydesign
    df_sub = df_mainsub %>% select(mother_age_group, partner_age_group, uf)
    for (var in setdiff(colnames(df_combinations), "partner_age_group")) {
      svydesign_sub = svydesign_sub %>% subset(get(var)==df_combinations[ii,var])
      df_sub = df_sub %>% subset(get(var)==df_combinations[ii,var])
    }
    
    # Estimate proportions
    est = svymean(~partner_age_group==df_combinations[ii,"partner_age_group"], svydesign_sub, na.rm=TRUE)
    conf = confint(est)
    
    # Save outputs
    df_tmp = data.frame(mother_age_group=df_combinations[ii,"mother_age_group"],
                        partner_age_group=df_combinations[ii,"partner_age_group"],
                        p=est[2],
                        p_lower = conf[2],
                        p_higher=conf[4],
                        se=SE(est)[2],
                        n=nrow(df_sub)
    )
    
    if (regional) {
      df_tmp = df_tmp %>% mutate(uf=df_combinations[ii,"uf"])
    }
    
    rownames(df_tmp) = NULL
    return(df_tmp)
    
  }
  
  
  if (SAVEFILE) {
    if (regional) {
      write.csv(df_results, paste0(OUTDIR, "partneragedist_regional.csv"), row.names=FALSE)
    } else {
      write.csv(df_results, paste0(OUTDIR, "partneragedist.csv"), row.names=FALSE)
    }
  }
  
  return(df_results)
  
}


#' Append female partner age data to survey responses
#'
#' Links mothers to their partners and extracts partner ages from PNS survey data.
#'
#' @param df_in Dataframe containing PNS survey responses.
#' @return A dataframe with added partner age information.
appendFemalePartnersAges = function(df_in) {
  
  df_left = df_in %>%
    filter(selected_for_individual==1, !is.na(spouse_order_number)) %>% # Filter to those that answered the individual section and provided spouse IDs
    filter(sex=="female") %>% # We condition on women so start here
    mutate(spouse_id = paste(household_id, spouse_order_number)) %>% # Generate the spouse ID for later joining
    select(age, resident_id, spouse_id) %>%
    appendAgeGroups() %>%
    rename(mother_age=age, mother_age_group=age_group)
  
  df_right = df_in %>%
    filter(resident_id %in% df_left$spouse_id) %>%
    select(age, sex, resident_id) %>%
    appendAgeGroups() %>%
    rename(partner_age=age, partner_age_group=age_group, partner_sex=sex, spouse_id=resident_id)
  
  df_mother = left_join(df_left, df_right, by="spouse_id") %>%
    filter(partner_sex=="male") %>%
    select(resident_id, mother_age_group, partner_age_group) %>%
    mutate(use_for_partner_dist=TRUE)
  
  rm(df_left, df_right)
  
  df = df_in %>% left_join(df_mother, by="resident_id")
  df$use_for_partner_dist[is.na(df$use_for_partner_dist)] = FALSE
  
  return(df)
  
}








