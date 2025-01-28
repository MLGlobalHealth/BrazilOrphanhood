
rm(list=ls())

library(tidyverse)
library(zoo)
library(geobr)
library(viridis)
library(cowplot)
library(clipr)

source("R/supportFunctions.R")
source("R/calculateMonthlyOrphanhood.R")

uf_lbls = read.csv("data/UF_labels.csv")

# Load population data (of children)
df_popn_children = read.csv("data/population/popests_simple_2010_2020.csv") %>%
  filter(age_group %in% paste0(seq(0, 17)), year==2020, !is.na(uf), sex!="both") %>%
  group_by(uf) %>%
  summarise(n_children=sum(n_pop)) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  select(Region, n_children)

# Append the national total number of children as a new row
df_popn_children = df_popn_children %>% rbind(data.frame(Region="Total", n_children=sum(df_popn_children$n_children)))

# Load population data (total)
df_popn = read.csv("data/population/popests_simple_2010_2020.csv") %>%
  filter(age_group=="TOTAL", year==2020, !is.na(uf), sex!="both") %>%
  group_by(uf) %>%
  summarise(popn=sum(n_pop)) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  select(Region, popn)

# Append the national total population as a new row
df_popn = df_popn %>% rbind(data.frame(Region="Total", popn=sum(df_popn$popn)))

# Define which states are to be plotted in color
color_mapping = c("Amapá" = "#56ae60", "São Paulo" = "#f55c47", "Amazonas"= "#807dba", "Rondônia" = "#ea6e20", "Paraná"= "#4292c6")
colored_regions = names(color_mapping)

# Fill in gray for remaining states
missing_regions = setdiff(unique(df_popn$Region), names(color_mapping))
color_mapping_gray = setNames(rep("#808080", length(missing_regions)), missing_regions)
color_mapping = c(color_mapping, color_mapping_gray)

# # Define the colours for the figure
# color_mapping <- c(
#   "Rondônia" = "#4daf4a", "Acre" = "#56ae60", "Amazonas" = "#60ad76", "Roraima" = "#6aac8c", "Pará" = "#74ab9e",
#   "Amapá" = "#7eabb0", "Tocantins" = "#88aac2",
#   "Maranhão" = "#4292c6", "Piauí" = "#4c8fb0", "Ceará" = "#56919c", "Rio Grande do Norte" = "#608f87", 
#   "Paraíba" = "#6a8f73", "Pernambuco" = "#748e5f", "Alagoas" = "#7e8d4b", "Sergipe" = "#888d37", "Bahia" = "#929c24",
#   "Minas Gerais" = "#ef3b2c", "Espírito Santo" = "#f24b39", "Rio de Janeiro" = "#f55c47", "São Paulo" = "#f86d54",
#   "Paraná" = "#807dba", "Santa Catarina" = "#8a88c1", "Rio Grande do Sul" = "#9493c9",
#   "Mato Grosso do Sul" = "#e6550d", "Mato Grosso" = "#ea6e20", "Goiás" = "#ee8733", "Distrito Federal" = "#f2a047"
# )

# Load monthly orphanhood data
df_orphanhood_in = calculateMonthlyOrphanhood()

# Load full samples to calculate adjustment factor
totalorphanhood = readRDS("samples/orphanhood_age_group_child_age_sex_uf_excess.RDS") %>%
  group_by(age_group, uf, sex, child_age) %>%
  summarise(orphanhood=mean(orphanhood)) %>%
  ungroup() %>%
  summarise(total=sum(orphanhood)) %>%
  pull()
scaling_factor = totalorphanhood/sum(df_orphanhood_in$orphanhood)

# Apply scaling
df_orphanhood_scaled = df_orphanhood_in %>%
  mutate(orphanhood = orphanhood*scaling_factor)

# Process orphanhood data for plotting
df_orphanhood = df_orphanhood_scaled %>%
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  group_by(date, uf) %>%
  summarise(orphanhood=sum(orphanhood, na.rm=TRUE)) %>%
  left_join(uf_lbls %>% select(uf, uf_name), by="uf") %>%
  rename(Region=uf_name) %>%
  left_join(df_popn_children, by="Region") %>%
  mutate(y = 1000*orphanhood/n_children) %>%
  select(-n_children, -orphanhood) %>%
  mutate(Cat = "(b) Orphanhood rate\n(number per 1,000 children)")


# Load covid deaths data
df_deaths_in = read.csv("data/coviddeaths.csv")

# Process covid deaths data for plotting
df_deaths = df_deaths_in %>%
  mutate(date = as.yearmon(paste(year, month), "%Y %m")) %>%
  rename(y=covid_deaths) %>%
  select(date, uf, y) %>%
  left_join(df_orphanhood %>% ungroup() %>% select(Region, uf) %>% unique(), by="uf") %>%
  mutate(Cat = "(a) COVID-19 deaths\n(number per 1,000 residents)") %>%
  left_join(df_popn, by="Region") %>%
  mutate(y=1000*y/popn) %>%
  select(-popn)

# And join dataframes together for plotting
df = rbind(df_deaths, df_orphanhood)


# Some reformatting for plotting
df <- df %>%
  mutate(large_region = case_when(
    Region %in% c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins") ~ "North",
    Region %in% c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia") ~ "Northeast",
    Region %in% c("Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo") ~ "Southeast",
    Region %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul") ~ "South",
    Region %in% c("Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal") ~ "Central-West",
    TRUE ~ NA_character_
  )) %>%
  mutate(large_region = factor(large_region, levels=c("North", "Northeast", "Central-West", "Southeast", "South"))) %>%
  arrange(large_region, Region) %>%
  mutate(Region = factor(Region, levels=unique(Region)))

# Make the plot
breaks <- seq(as.yearmon("2020-01"), as.yearmon("2021-12"), by = 1/3)
minor_breaks <- seq(as.yearmon("2020-01"), as.yearmon("2021-12"), by = 1/12)
minor_breaks <- minor_breaks[minor_breaks != as.yearmon("2022-01")]
labels <- format(breaks, "%b %Y")

# Define which states are to be plotted in color
color_mapping = c("Amapá" = "#56ae60", "São Paulo" = "#f55c47", "Amazonas"= "#807dba", "Rondônia" = "#ea6e20", "Minas Gerais"= "#4292c6")
colored_regions = names(color_mapping)

# Fill in gray for remaining states
missing_regions = setdiff(unique(df_popn$Region), names(color_mapping))
color_mapping_gray = setNames(rep("#808080", length(missing_regions)), missing_regions)
color_mapping = c(color_mapping, color_mapping_gray)


plt = ggplot(df) +
  geom_line(aes(x=date, y=y, group=Region), data=.%>%filter(Region %in% missing_regions), alpha=0.15) +
  geom_line(aes(x=date, y=y, color=Region), data=.%>%filter(Region %in% colored_regions)) +
  facet_wrap("Cat", scales="free_y", strip.position = "left", ncol=1) +
  
  geom_text(aes(x=as.yearmon("2021-01"), y=1, label="Amazonas"), color=color_mapping["Amazonas"], data=. %>% filter(Cat=="(a) COVID-19 deaths\n(number per 1,000 residents)"), size=3, hjust=0) +
  # geom_text(aes(x=as.yearmon("2021-03"), y=0.78, label="Rio Grande do Sul"), color=color_mapping["Rio Grande do Sul"], data=. %>% filter(Cat=="(a) COVID-19 deaths\n(number per 1,000 residents)"), size=3, hjust=0) +
  geom_text(aes(x=as.yearmon("2021-03"), y=0.66, label="     Rondônia"), color=color_mapping["Rondônia"], data=. %>% filter(Cat=="(a) COVID-19 deaths\n(number per 1,000 residents)"), size=3, hjust=0) +
  # geom_text(aes(x=as.yearmon("2021-06"), y=0.5, label="Paraná"), color=color_mapping["Paraná"], data=. %>% filter(Cat=="(a) COVID-19 deaths\n(number per 1,000 residents)"), size=3, hjust=0) +
  # geom_text(aes(x=as.yearmon("2020-06"), y=0.51, label="Roraima"), color=color_mapping["Roraima"], data=. %>% filter(Cat=="(a) COVID-19 deaths\n(number per 1,000 residents)"), size=3, hjust=0) +
  # geom_text(aes(x=as.yearmon("2020-05"), y=0.53, label="Ceará "), color=color_mapping["Ceará"], data=. %>% filter(Cat=="(a) COVID-19 deaths\n(number per 1,000 residents)"), size=3, hjust=1) +
  geom_text(aes(x=as.yearmon("2020-05"), y=0.48, label="Amapá  "), color=color_mapping["Amapá"], data=. %>% filter(Cat=="(a) COVID-19 deaths\n(number per 1,000 residents)"), size=3, hjust=1) +
  geom_text(aes(x=as.yearmon("2021-07"), y=0.32, label="Minas Gerais"), color=color_mapping["Minas Gerais"], data=. %>% filter(Cat=="(a) COVID-19 deaths\n(number per 1,000 residents)"), size=3, hjust=0) +
  geom_text(aes(x=as.yearmon("2021-06"), y=0.4, label="São Paulo"), color=color_mapping["São Paulo"], data=. %>% filter(Cat=="(a) COVID-19 deaths\n(number per 1,000 residents)"), size=3, hjust=0) +
  
  
  geom_text(aes(x=as.yearmon("2020-02"), y=0.35, label="Amazonas"), color=color_mapping["Amazonas"], data=. %>% filter(Cat=="(b) Orphanhood rate\n(number per 1,000 children)"), size=3, hjust=0) +
  geom_text(aes(x=as.yearmon("2021-03"), y=0.97, label="Rondônia"), color=color_mapping["Rondônia"], data=. %>% filter(Cat=="(b) Orphanhood rate\n(number per 1,000 children)"), size=3, hjust=0) +
  # geom_text(aes(x=as.yearmon("2021-06"), y=0.78, label="Paraná"), color=color_mapping["Paraná"], data=. %>% filter(Cat=="(b) Orphanhood rate\n(number per 1,000 children)"), size=3, hjust=0) +
  geom_text(aes(x=as.yearmon("2021-08"), y=0.32, label="Minas Gerais"), color=color_mapping["Minas Gerais"], data=. %>% filter(Cat=="(b) Orphanhood rate\n(number per 1,000 children)"), size=3, hjust=0) +
  # geom_text(aes(x=as.yearmon("2020-06"), y=0.7, label="Roraima"), color=color_mapping["Roraima"], data=. %>% filter(Cat=="(b) Orphanhood rate\n(number per 1,000 children)"), size=3, hjust=0) +
  geom_text(aes(x=as.yearmon("2021-07"), y=0.4, label="São Paulo"), color=color_mapping["São Paulo"], data=. %>% filter(Cat=="(b) Orphanhood rate\n(number per 1,000 children)"), size=3, hjust=0) +
  geom_text(aes(x=as.yearmon("2020-05"), y=0.72, label="Amapá"), color=color_mapping["Amapá"], data=. %>% filter(Cat=="(b) Orphanhood rate\n(number per 1,000 children)"), size=3, hjust=1) +
  
  theme_minimal() +
  xlab("") +
  theme(strip.background=element_rect(fill=NA)) +
  scale_color_manual(values=color_mapping, guide = guide_legend(order = 1)) + 
  ylab("") +
  theme(strip.placement = "outside",
        strip.text.y = element_text(angle = 0),
        strip.background = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8)) +
  guides(color = guide_legend(ncol=1, keyheight = unit(0.4, "cm"), keywidth = unit(0.4, "cm"))) +
  scale_x_yearmon(limits = c(as.yearmon("2020-01"), as.yearmon("2021-12")), breaks = breaks, labels = labels, minor_breaks = minor_breaks)
plt
ggsave("outputs/figure3_monthly.png", plot=plt, width=25, height=15, units="cm", dpi=300)

