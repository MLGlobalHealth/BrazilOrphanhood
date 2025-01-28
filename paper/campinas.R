

# ------------------------------------------------------------------------------
# ---------------------------- Setup and load data -----------------------------
# ------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(DescTools)

df = read.csv("data/campinas/campinassimple.csv") %>%
  mutate(sex=case_when(sex=="Pai" ~ "Father",
                       sex=="Mãe" ~ "Mother",
                       sex=="Avô" ~ "Grandfather",
                       sex=="" ~ "Unknown"),
        age_string = case_when(age=="N/C" ~ "Unknown",
                                   age=="" ~ "Blank",
                                   TRUE ~ age),
        age = as.numeric(age_string))



# ------------------------------------------------------------------------------
# ------------ Print totals and proportions by sex of parent -------------------
# ------------------------------------------------------------------------------

print(df %>% group_by(sex) %>% summarise(n=n()) %>% mutate(p=n/sum(n)))
print(df %>% filter(sex%in%c("Father", "Mother")) %>% group_by(sex) %>% summarise(n=n()) %>% mutate(p=n/sum(n)))



# ------------------------------------------------------------------------------
# ------------- Compare age-distribution of orphaned children  -----------------
# ------------------------------------------------------------------------------

# Group by age_string to check number of blanks and unknown values
print(df %>% group_by(age_string) %>% summarise(n=n()) %>% mutate(p=n/sum(n)))

# Filter out missing/unknown ages and calculate proportion of each age
df_age = df %>% filter(!is.na(age)) %>% group_by(age) %>% summarise(n=n()) %>% mutate(p=n/sum(n))
cis = MultinomCI(df_age$n) # Calculate multinomial CIs using Sison and Glaz
df_age$lower = cis[,2] # Append these CIs to the age-dist data
df_age$upper = cis[,3]

# Load our estimates and calculate age distribution and CIs
df_est_in = readRDS("samples/orphanhood_age_group_child_age_sex_uf_excess.RDS")
df_est = df_est_in %>%
  filter(uf==35) %>%
  group_by(iter, child_age) %>%
  summarise(n=sum(orphanhood)) %>%
  group_by(iter) %>%
  mutate(p_iter=n/sum(n)) %>%
  group_by(child_age) %>%
  summarise(p=mean(p_iter), lower=quantile(p_iter, 0.025), upper=quantile(p_iter,0.975)) %>%
  rename(age=child_age) %>%
  select(age, p, lower, upper) %>%
  mutate(Source="Estimates")

# Combine data for plotting
df_plt = rbind(df_age %>% mutate(Source="Campinas data") %>% select(-n),
               df_est)

# Make the figure and save
plt = ggplot(df_plt) +
  geom_point(aes(x=age,y=p,color=Source), size=2.5) +
  geom_errorbar(aes(x=age,ymin=lower,ymax=upper, color=Source), width=0.3, alpha=0.7) +
  geom_line(aes(x=age,y=p,color=Source)) +
  theme_minimal() +
  xlab("Child age") + ylab("Proportion of total orphanhood") +
  scale_color_manual(values=c("#41b8ad", "#d34861"))
plt
ggsave("outputs/supplfigure7_childagecampinas.png", plot=plt, width=25, height=10, units="cm", dpi=300)


