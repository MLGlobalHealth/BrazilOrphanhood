
rm(list=ls())

library(PNSIBGE)
library(tidyverse)
library(survey)

IN_DIR = "data/pns2019/pns2019_rawdata/"
OUT_DIR = "data/pns2019/"

# Read in downloaded PNS data
df_in = read_pns(paste0(IN_DIR, "PNS_2019.txt"), paste0(IN_DIR, "input_PNS_2019.txt"))

# Relabel data (where we want to keep the original names for survey weighting purposes also)
df = df_in %>% mutate(
  uf = V0001,
  UPA = UPA_PNS,
  household_order_number = V0006_PNS,
  selected_for_individual = V0025A, # 0 = No, 1 = Yes, 9 = NA
  selected_for_anthropometric = V0025B
)

# Relabel data (where we don't need to keep the original names)
df = df %>% mutate(

  # Part 3: General characteristics of residents
  household_size = C001,
  resident_order_number = C00301,
  resident_type = C004, # "Condition of person at home", fields include: responsible for domicile, spouse or partner, child of the guardian etc
  sex = C006, # 1 = male, 2 = female
  birth_year = C00703,
  age = C008,
  spouse_lives_in_domicile = C01001,
  spouse_order_number = C010010,

  # Part 3, Module S: Prenatal care (for women aged 15+)
  has_been_pregnant = S065,
  number_of_deliveries = S066,
  last_delivery_year = S06703, # can also get day and month

  # Part 3, Module Z: Paternal (for men aged 15+)
  is_father = Z001, # 1 = yes, 2 = no, 3 = unsure, 9 = ignored,
  n_male_children = Z00101,
  n_female_children = Z00102,
  age_at_birth_of_first_child = Z002,
  age_of_youngest_child = Z003,
  woman_pregnant_with = Z004,
  has_adopted = Z013,
  has_adopted_n_male = Z01401,
  has_adopted_n_female = Z01402

)

# Select only variables we need
chosen_vars = c("uf", "UPA", "household_order_number", "selected_for_individual", "selected_for_anthropometric",
                "household_size", "resident_order_number", "resident_type", "sex", "birth_year", "age", "spouse_lives_in_domicile", "spouse_order_number",
                "is_father", "n_male_children", "n_female_children", "age_at_birth_of_first_child", "age_of_youngest_child",
                "woman_pregnant_with", "has_adopted", "has_adopted_n_male", "has_adopted_n_female")
sampling_vars = c(colnames(df_in)[grepl("^V.*", colnames(df_in))], "UPA_PNS", "ID_DOMICILIO")


df = df %>% select_at(c(chosen_vars, sampling_vars))


# Clean up variables
df$sex[df$sex=="1"] = "male"
df$sex[df$sex=="2"] = "female"

df = df %>% mutate(
  uf = factor(as.numeric(uf)),
  sex = factor(sex)
)


# Create derived variables
df = df %>% mutate(
  household_id = paste(UPA, household_order_number),
  resident_id = paste(household_id, resident_order_number),
  is_adult_male = (age >= 15) & (sex=="male"),
  is_child = (age <= 18),
  total_children = n_male_children + n_female_children,
  age_of_oldest_child = age - age_at_birth_of_first_child
)


# # Do a little bit more tidying (this is convoluted but it allows you to check step-by-step that you're not doing anything obviously incorrect)
# df$total_children[!df$is_adult_male] = "-99" # If the individual isn't an adult male,
# df$total_children[!(df$selected_for_individual==1)] = "-99" # or if they're not the selected individual
# df$total_children[is.na(df$is_father)] = "-99" # or if they didn't answer the father question
# df$total_children[df$is_father=="3"] = "-99" # or if they said they're unsure, then we cannot know that they have zero children
# df$total_children[df$is_father=="2"] = "0" # otherwise the remaining individuals have 0
# df$total_children[df$total_children == "-99"] = NA # and we set the rest to NA.

# We can do the above tidying in a much nicer way now that we trust the data
df$total_children[df$is_father=="2"] = 0


# And save
saveRDS(df, paste0(OUT_DIR, "pns2019_processed.rds"))
