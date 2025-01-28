
library(tidyverse)
library(foreach)

# For computational reasons, we estimate results by child-age in individual years.
# These files are saved in a subdirectory /interim/, which we exclude from the github repository for size reasons.
# These functions are used to aggregate results by child-age into single files (which are included in the gh repo).



#' Aggregate orphanhood estimates across child ages
#'
#' Merges child-age-specific orphanhood estimates into a single dataset.
#'
#' @param output Character; specifies the type of orphanhood to aggregate (default: "any").
#' @param mortality Character; either "excess" or "allcause" (default: "excess").
#' @return A dataframe with aggregated orphanhood estimates.
aggregateCoresChildAgeSamples = function(output="any",
                                         mortality="excess") {
  
  fname_in = paste0("samples/interim/coresorphanhood_", output, "_age_group_sex_uf_", mortality, "_CHILDAGE")
  fname_out = paste0("samples/coresorphanhood_", output, "_age_group_child_age_sex_uf_", mortality, ".RDS")
  
  df_out = foreach (child_age = seq(0,17), .combine=rbind) %do% {
    df_in = readRDS(paste0(fname_in, child_age, ".RDS")) %>% mutate(child_age=child_age)
  }
  
  saveRDS(df_out, fname_out)
  
}


#' Aggregate child-age-specific orphanhood estimates
#'
#' Merges orphanhood estimates for different child ages into a single dataset.
#'
#' @param group_vars Character vector specifying grouping variables (default: c("age_group", "sex", "uf")).
#' @param mortality Character; either "excess" or "allcause" (default: "excess").
#' @return A dataframe with aggregated orphanhood estimates.
aggregateChildAgeSamples = function(group_vars = c("age_group", "sex", "uf"),
                                    mortality = "excess") {
  
  fname_in = paste0("samples/interim/orphanhood_", paste0(group_vars, collapse="_"), "_", mortality, "_CHILDAGE")
  fname_out = paste0("samples/orphanhood_", paste0(sort(c(group_vars, "child_age")), collapse="_"), "_", mortality, ".RDS")
  
  df_out = foreach (child_age = seq(0,17), .combine=rbind) %do% {
    df_in = readRDS(paste0(fname_in, child_age, ".RDS")) %>% mutate(child_age=child_age)
  }
  
  saveRDS(df_out, fname_out)
  
}



#' List unique orphanhood output types stored in the interim directory
#'
#' Extracts and lists unique output prefixes from saved orphanhood estimates.
#'
#' @return A character vector of unique orphanhood output types.
listUniqueChildAgeOutputs = function() {
  
  filenames <- list.files(path = "samples/interim", full.names = TRUE)
  cleaned_filenames <- str_replace(filenames, "CHILDAGE.*", "")
  return(unique(cleaned_filenames))
  
}
