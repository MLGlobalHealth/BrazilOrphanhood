
rm(list=ls())

library(tidyverse)
library(zoo)
library(cowplot)

source("R/calculateMonthlyOrphanhood.R")

df = read.csv("data/civilreg/orfaos_clean.csv") %>%
  mutate(double_orphanhood = double_orphanhood=="SIM",
         child_years = as.numeric(case_when(child_years=="-" ~ "0",
                                            TRUE ~ child_years)),
         date_of_death = as.Date(date_of_death, origin = "1899-12-30")) %>%
  filter(child_years<=6)

uf_lbls = read.csv("data/UF_labels.csv")

df_popn = read.csv("data/population/popests_simple_2010_2020.csv") %>%
  filter(age_group %in% paste0(seq(0, 6)), year==2020, !is.na(uf), sex!="both") %>% # Filter out relevant counts, ...
  group_by(uf) %>% # ...
  summarise(n_children=sum(n_pop)) %>% #..., group by federative unit
  left_join(uf_lbls, by="uf") # append the federative unit labels


# ------------------------------------------------------------------------------
# -------------- Compare time-distribution of orphanhood -----------------------
# ------------------------------------------------------------------------------

# Create year-month variable and filter out incomplete months
df_yearmon = df %>%
  filter(illness_of_death=="COVID") %>%
  mutate(date = as.yearmon(date_of_death)) %>% group_by(date) %>% summarise(n=n()) %>%
  filter(date >= as.yearmon("2020-04-01"), date <= as.yearmon("2021-08-31")) %>%
  mutate(p=n/sum(n), Source="Civil registry")

# Calculate scaling factor for monthly
total_orphanhood_full_model = readRDS("samples/orphanhood_age_group_child_age_sex_uf_excess.RDS") %>%
  group_by(age_group, uf, sex, child_age) %>%
  summarise(orphanhood=mean(orphanhood)) %>%
  ungroup() %>%
  summarise(total=sum(orphanhood)) %>%
  pull()

df_orphanhood_monthly = calculateMonthlyOrphanhood(by_child_age=TRUE)
scaling_factor = total_orphanhood_full_model / sum(df_orphanhood_monthly$orphanhood)
df_orphanhood_monthly$orphanhood = df_orphanhood_monthly$orphanhood * scaling_factor


# Load monthly orphanhood and filter to same time period
df_monthly = df_orphanhood_monthly %>%
  mutate(date = as.yearmon(paste(year, month, sep="-"))) %>%
  filter(child_age <= 6) %>%
  group_by(date) %>%
  summarise(n=sum(orphanhood)) %>%
  filter(date >= as.yearmon("2020-04-01"), date <= as.yearmon("2021-08-31")) %>%
  mutate(p=n/sum(n), Source="Our estimates")

# Combine for plotting
df_plt = rbind(df_yearmon, df_monthly)

# Make plot
plt_prop = ggplot(df_plt) +
  geom_point(aes(x=date,y=p,color=Source), size=3) +
  geom_line(aes(x=date,y=p,color=Source), alpha=0.4) +
  theme_minimal() +
  theme(legend.position="bottom", legend.title=element_blank()) +
  scale_color_manual(values=c("#41b8ad", "#d34861")) +
  xlab("Date") + ylab("Proportion of orphanhood") + ggtitle("(a)")
plt_prop

df_plt = rbind(df_yearmon, df_monthly %>% mutate(Source="Our estimates (less civil registry)"))

plt_magn = ggplot(df_plt) +
  geom_col(aes(x=date, y=n, fill=Source), position="dodge") +
  # geom_col(aes(x=date, y=n, fill=Source), position="identity", data=.%>%filter(Source=="Civil registry")) +
  theme_minimal() +
  theme(legend.position="none", legend.title=element_blank()) +
  scale_fill_manual(values=c("#41b8ad", "#d34861")) +
  xlab("Date") + ylab("Monthly orphanhood") + ggtitle("(b)")
plt_magn

# plt = plot_grid(plt_prop, plt_magn, ncol=1, align="v")
# plt
# 
# ggsave("outputs/figure4_civilregistry.png", plot=plt, width=25, height=15, units="cm", dpi=300)
# 


# For the caption: compare monthly ascertainment
df_monthlyascertainment = df_plt %>%
  mutate(Source=ifelse(Source=="Civil registry", "Civil", "Our")) %>%
  pivot_wider(id_cols=date, names_from=Source, values_from=c("n")) %>%
  mutate(ascertainment=Civil/Our)

plt_asc = ggplot(df_monthlyascertainment) +
  geom_point(aes(x=date, y=ascertainment), size=3) +
  geom_line(aes(x=date, y=ascertainment), alpha=0.4) +
  theme_minimal() +
  xlab("Date") + ylab("Monthly ascertainment ratio") + ggtitle("(c)") +
  ylim(c(0, 0.5))
plt_asc

plt = plot_grid(plt_prop, plt_magn, plt_asc, ncol=1, align="v")
plt
ggsave("outputs/figure4_civilregistry.png", plot=plt, width=25, height=19, units="cm", dpi=300)
