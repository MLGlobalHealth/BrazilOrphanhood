
library(tidyverse)
library(zoo)

# ----------------------- Age-group management -----------------------

#' Expands age groups into individual years
#'
#' @param df The dataframe containing an age_group column in the format "X-Y".
#' @return A dataframe where each age group is expanded into individual age values in a new column called expanded_age.
expandAgeGroups = function(df) {
  
  # Extract the lower and upper age-bounds
  df_temp = df %>% mutate(age_lower = as.numeric(sub("-.*", "", age_group)),
                          age_higher = as.numeric(sub(".*-", "", age_group)),
                          length = age_higher - age_lower + 1)
  
  # Repeat each row for the number of ages in each age-group
  df_out = df_temp[rep(seq(1,nrow(df_temp)), df_temp$length),]
  
  # Calculate and append the corresponding age
  ages = rep(0, nrow(df_out))
  max_ind = 0
  for (ii in seq(1, nrow(df_temp))) {
    ages[(max_ind+1):(max_ind+df_temp$length[ii])] = df_temp$age_lower[ii] + seq(0, df_temp$length[ii]-1)
    max_ind = max_ind + df_temp$length[ii]
  }
  df_out$expanded_age = ages
  
  # Tidy up
  df_out = df_out %>% select(-age_lower, -age_higher, -length)
  
  return(df_out)
  
}


#' Assigns age groups based on a specified age column
#'
#' @param df The dataframe containing a column with single-year ages.
#' @param agecolname The name of the column containing ages. Defaults to "age".
#' @return A dataframe with a new column age_group categorizing ages into predefined ranges.
appendAgeGroups = function(df, agecolname="age") {
  
  df = df %>% mutate(age_group = case_when(
    .data[[agecolname]] <= 9 ~ "0-9",
    .data[[agecolname]] <= 19 ~ "10-19",
    .data[[agecolname]] <= 29 ~ "20-29",
    .data[[agecolname]] <= 39 ~ "30-39",
    .data[[agecolname]] <= 49 ~ "40-49",
    .data[[agecolname]] <= 59 ~ "50-59",
    .data[[agecolname]] <= 69 ~ "60-69",
    .data[[agecolname]] >= 70 ~ "70+",
    TRUE ~ NA_character_
  ))
  
  return(df)
  
}




# ----------------------- Summarise and tidy results -----------------------


#' Summarizes orphanhood estimates with confidence intervals by specified grouping variables
#'
#' @param df A dataframe containing orphanhood estimates.
#' @param group_vars A character vector specifying the grouping variables.
#' @param cumulative Logical; if TRUE, computes cumulative orphanhood over time.
#' @return A summarized dataframe with mean and confidence intervals, optionally cumulative.
summariseByGroup = function(df,
                              group_vars=c(),
                              cumulative=FALSE) {
  
  # Check conditions
  if (cumulative & !("date" %in% group_vars)) {
    stop("If cumulative results are required then date needs to be in group_vars")
  }
  
  # Prepare and tidy up
  df_out = df
  if ("date" %in% group_vars) {
    df_out = df_out %>% mutate(date = as.yearmon(date)) %>% arrange(date)
  }
  if ("age_group" %in% group_vars) {
    df_out = df_out %>% filter(age_group!="0-9")
  }
  
  # Run initial aggregation
  df_out = df_out %>%
    group_by_at(c("iter", group_vars)) %>%
    summarise(orphanhood=sum(orphanhood, na.rm=TRUE))
  
  # Calculate mean and confidence intervals
  if (!cumulative & length(group_vars)>=1) {
    df_out = df_out %>%
      group_by_at(group_vars) %>%
      summarise(mean=mean(orphanhood), lower=quantile(orphanhood, 0.025), upper=quantile(orphanhood, 0.975))
  } else if (!cumulative) {
    df_out = df_out %>%
      ungroup() %>%
      summarise(mean=mean(orphanhood), lower=quantile(orphanhood, 0.025), upper=quantile(orphanhood, 0.975))
  } else {
    df_out = df_out %>%
      group_by(iter) %>%
      mutate(cumulative_orphanhood=cumsum(orphanhood)) %>%
      group_by_at(group_vars) %>%
      summarise(mean=mean(orphanhood), lower=quantile(orphanhood, 0.025), upper=quantile(orphanhood, 0.975),
                mean_cumul=mean(cumulative_orphanhood), lower_cumul=quantile(cumulative_orphanhood, 0.025), upper_cumul=quantile(cumulative_orphanhood, 0.975))
    
    df_out = rbind(df_out %>% select(date, mean, lower, upper) %>% mutate(type="Monthly"),
                   df_out %>% select(date, mean_cumul, lower_cumul, upper_cumul) %>% rename(mean=mean_cumul, lower=lower_cumul, upper=upper_cumul) %>% mutate(type="Cumulative"))
    df_out$type = factor(df_out$type, levels=c("Monthly", "Cumulative"))
    
    
  }
  
  return(df_out)
  
}




#' Standardizes factor levels and variable labels in a dataframe
#'
#' @param df The dataframe to standardize.
#' @return A dataframe with cleaned factor levels for age_group, sex, and uf.
tidydf = function(df) {
  
  if ("age_group" %in% colnames(df)) {
    df = df %>%
      filter(age_group != "0-9") %>%
      mutate(age_group = factor(age_group, levels=c("10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")))
  }
  
  if ("sex" %in% colnames(df)) {
    df = df %>%
      mutate(sex=case_when(sex=="male"~"Male", sex=="female"~"Female", sex=="both"~"Both")) %>%
      mutate(sex=factor(sex, levels=c("Female", "Male", "Both")))
  }
  
  if ("uf" %in% colnames(df)) {
    uf_lbls = read.csv("data/UF_labels.csv")
    uf_name_levels = sort(unique(uf_lbls$uf_name))
    df = df %>%
      left_join(uf_lbls, by="uf") %>%
      mutate(uf_name = factor(uf_name, levels=uf_name_levels))
  }
  
  return(df)
  
}




