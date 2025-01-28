
# This script processes the birth records data available here:
# https://opendatasus.saude.gov.br/dataset/sistema-de-informacao-sobre-nascidos-vivos-sinasc

rm(list=ls())

library(tidyverse)

YEARS = seq(2002, 2020) # Define the years we are interested in
IN_DIR = "data/livebirths/livebirths_rawdata/" # Specify the directory of the raw data (relative to parent directory of the repository)

# Name our output dataframe
df_all = NULL

# Iterate over each year of interest
for (ii in seq(1, length(YEARS))) {
  
  current_year = YEARS[ii]
  print(paste0("Loading and appending year: ", current_year))
  
  # If the year is 2010 or later, then we can also read information bout the father's age
  if (current_year >= 2010) {
    
    # Load file
    df_in = read.csv(paste0(IN_DIR, "SINASC_", current_year, ".csv"), sep=";") %>%
      mutate(uf = as.numeric(substr(as.character(CODMUNRES), 1, 2))) %>% # Extract UF code
      rename(mother_age=IDADEMAE, father_age=IDADEPAI) %>% # Standardise column names
      select(mother_age, father_age, uf) # Extract columns of interest
    
    # Check the father_age column has been read in as numeric, otherwise convert to numeric
    if (typeof(df_in$father_age) == "character" ){
      df_in$father_age = as.numeric(df_in$father_age)
    }
    
    # Aggregate and count the number of births in each mother_age/father_age/region combination
    df_summary = df_in %>%
      group_by(mother_age, father_age, uf) %>%
      summarise(n=n()) %>%
      mutate(year=current_year)
    
    # And set father age non-response as -99
    df_summary$father_age[is.na(df_summary$father_age)] = -99
    
    
  } else {
    
    # Load file
    df_in = read_delim(paste0(IN_DIR, "SINASC_", current_year, ".csv"), ";", show_col_types=FALSE) %>%
      mutate(uf = as.numeric(substr(as.character(CODMUNRES), 1, 2))) %>% # Extract UF code
      rename(mother_age=IDADEMAE) %>% # Standardise column names 
      select(mother_age, uf) # Extract columns of interest
    
    # Aggregate and count the number of births in each mother_age/region combination
    df_summary = df_in %>%
      group_by(mother_age, uf) %>%
      summarise(n=n()) %>%
      mutate(year=current_year, father_age=NA)
    
  }
  
  # Append file and tidy
  df_all = rbind(df_all, df_summary) # And add to the main results dataframe
  rm(df_in) # This can be quite big!
  
}

# Save file
write.csv(df_all, "data/livebirths/livebirths_processed_2002_2020.csv", row.names=FALSE)