
rm(list=ls())

library(tidyverse)
library(cowplot)
library(foreach)

source("R/supportFunctions.R")

# Load data
df_pns = readRDS("samples/malefertility_annualagespecific.RDS")

df_births = read.csv("data/livebirths/livebirths_processed_2002_2020.csv") %>%
  filter(year>=2010) %>%
  group_by(father_age, year) %>%
  summarise(n=sum(n))



# ------------------------------------------------------------------------------
# ------------- Calculate proportion of missing data and append ----------------
# ------------------------------------------------------------------------------

df_births_p = df_births %>%
  mutate(is_missing = father_age==-99) %>%
  group_by(is_missing, year) %>%
  summarise(n=sum(n)) %>%
  ungroup()

df_births_p= left_join(df_births_p %>% filter(is_missing) %>% rename(n_missing=n) %>% select(-is_missing),
                       df_births_p %>% filter(!is_missing) %>% rename(n_present=n) %>% select(-is_missing),
                       by="year") %>%
  mutate(p = n_present/(n_present+n_missing)) %>%
  select(year, p)

df_births = df_births %>%
  left_join(df_births_p, by="year") %>%
  filter(father_age!=-99) %>%
  mutate(n_scaled=n/p)



# ------------------------------------------------------------------------------
# ----------------- Load population data into correct format -------------------
# ------------------------------------------------------------------------------

# Load raw data
df_popn = read.csv("data/population/popests_2002_2020.csv") %>%
  filter(age_group!="Total", sex=="male") %>%
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
df_popn_processed = foreach(ii = seq(1, nrow(df_combinations)), .combine=rbind) %do% {
  
  df_current = df_popn %>% filter(uf==df_combinations$uf[ii], year==df_combinations$year[ii]) %>% arrange(min_age)
  pop_interp = DemoTools::graduate_sprague(Value=df_current$n_pop, Age=as.integer(df_current$min_age), OAG=FALSE)
  return(data.frame(father_age=as.integer(names(pop_interp)), n_pop=unname(pop_interp), uf=df_combinations$uf[ii], year=df_combinations$year[ii]))
  
}

df_popn_processed = df_popn_processed %>% group_by(father_age, year) %>% summarise(n_pop = sum(n_pop))


# ------------------------------------------------------------------------------
# -------------- Join population data onto our births records ------------------
# ------------------------------------------------------------------------------

df_fertility = df_births %>%
  left_join(df_popn_processed, by=c("father_age", "year")) %>%
  rename(age=father_age) %>%
  appendAgeGroups() %>%
  group_by(age_group, year) %>%
  summarise(n_births=sum(n_scaled), n_pop=sum(n_pop)) %>%
  mutate(central = n_births/n_pop, lower=NA, upper=NA) %>%
  rename(birth_year=year)


df_plt = rbind(
  df_fertility %>% select(age_group, birth_year, central, lower, upper) %>% mutate(Source="Live births"),
  df_pns %>% select(-n) %>% mutate(Source="PNS")
)

df_plt$lower[df_plt$lower<0] = 0


plt = ggplot(df_plt %>% filter(birth_year!=2020, birth_year!=2010)) +
  geom_ribbon(aes(x=birth_year, ymin=lower, ymax=upper, fill=age_group), alpha=0.2, data=.%>%filter(Source=="PNS")) +
  geom_line(aes(x=birth_year, y=central, color=age_group, lty=Source), lwd=0.8) +
  theme_minimal() +
  xlab("Year of birth") +
  ylab("Male fertility rate") +
  labs(color = "Age-group", fill="Age-group")
plt
ggsave("outputs/supplfigure6_malefertility.png", plot=plt, width=25, height=12, units="cm", dpi=300)
