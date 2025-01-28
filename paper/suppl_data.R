
rm(list=ls())

library(tidyverse)
library(geobr)
library(viridis)
library(zoo)

source("R/supportFunctions.R")

uf_lbls = read.csv("data/UF_labels.csv")


# ------------------------------------------------------------------------------
# ------------------- Visualise all-cause mortality ----------------------------
# ------------------------------------------------------------------------------

# Load data
df_em = read.csv("data/excessmortality/excess_mortality_bayesian.csv") %>%
  mutate(fitted_deaths = rowMeans(select(., starts_with("V")))) %>%
  select(-all_of(paste0("V", seq(1,1000)))) %>%
  mutate(excess_deaths = deaths - fitted_deaths)
df_popn = read.csv("data/population/popests_simple_2010_2020.csv") %>% filter(year==2020, age_group=="TOTAL", sex=="both", uf_code!="BR")
brsf_in = read_state(year=2020)

# Calculate regional all-cause mortality
df_all_regional = df_em %>%
  group_by(uf, year) %>%
  summarise(all_deaths = sum(deaths)) %>%
  left_join(df_popn %>% select(uf, n_pop), by="uf") %>%
  mutate(deaths_per_1000 = 1000*all_deaths/n_pop)

brsf2020 = brsf_in %>%
  rename(uf=code_state) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  left_join(df_all_regional %>% filter(year==2020), by="uf")

brsf2021 = brsf_in %>%
  rename(uf=code_state) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  left_join(df_all_regional %>% filter(year==2021), by="uf")

brsf = rbind(brsf2020, brsf2021)

plt_regional = ggplot(brsf) +
  geom_sf(aes(fill=deaths_per_1000)) +
  scale_fill_viridis(option="magma", direction=-1, name="All-cause deaths per 1000 residents") +
  facet_wrap("year") +
  theme_bw() +
  theme(legend.position="bottom", legend.title = element_text(vjust = 0.5), legend.title.align = 0.5, strip.background=element_blank(), strip.text=element_text(size=14)) +
  guides(fill = guide_colorbar(barwidth = 20, title.position = "top")) # Increase the barwidth value to make the bar longer
ggsave("outputs/supplfigure1_allcausemortality.png", plot=plt_regional, width=300, units="mm", dpi=300)


# ------------------------------------------------------------------------------
# -------------------- Visualise excess mortality ------------------------------
# ------------------------------------------------------------------------------

# Calculate regional mean excess mortality
df_em_regional = df_em %>%
  mutate(excess_deaths = pmax(excess_deaths, 0)) %>%
  group_by(uf, year) %>%
  summarise(excess_deaths = sum(excess_deaths)) %>%
  left_join(df_popn %>% select(uf, n_pop), by="uf") %>%
  mutate(em_per_1000 = 1000*excess_deaths/n_pop)

brsf2020 = brsf_in %>%
  rename(uf=code_state) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  left_join(df_em_regional %>% filter(year==2020), by="uf")

brsf2021 = brsf_in %>%
  rename(uf=code_state) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  left_join(df_em_regional %>% filter(year==2021), by="uf")

brsf = rbind(brsf2020, brsf2021)

plt_regional = ggplot(brsf) +
  geom_sf(aes(fill=em_per_1000)) +
  scale_fill_viridis(option="magma", direction=-1, name="Excess deaths per 1000 residents") +
  facet_wrap("year") +
  theme_bw() +
  theme(legend.position="bottom", legend.title = element_text(vjust = 0.5), legend.title.align = 0.5, strip.background=element_blank(), strip.text=element_text(size=14)) +
  guides(fill = guide_colorbar(barwidth = 20, title.position = "top")) # Increase the barwidth value to make the bar longer
ggsave("outputs/supplfigure2_excessmortality.png", plot=plt_regional, width=300, units="mm", dpi=300)




# ------------------------------------------------------------------------------
# ----------------------- Visualise live births --------------------------------
# ------------------------------------------------------------------------------

# Load data
df_births = read.csv("data/livebirths/livebirths_processed_2002_2020.csv") %>% appendAgeGroups(agecolname="mother_age")

# Calculate percentage missing age
df_noage = df_births %>%
  group_by(uf, year) %>%
  summarise(n_na=sum(is.na(mother_age) * n), ntotal=sum(n)) %>%
  mutate(p_missing = n_na/ntotal) %>%
  select(uf, year, p_missing)

# Calculate percentage missing fathers age
df_noage_father = df_births %>%
  mutate(is_missing = father_age==-99) %>%
  group_by(is_missing, year) %>%
  summarise(n=sum(n)) %>%
  ungroup()

df_births_p = left_join(df_noage_father %>% filter(is_missing) %>% rename(n_missing=n) %>% select(-is_missing),
                        df_noage_father %>% filter(!is_missing) %>% rename(n_present=n) %>% select(-is_missing),
                       by="year") %>%
  mutate(p = n_present/(n_present+n_missing)) %>%
  select(year, p)

df_births = df_births %>%
  left_join(df_births_p, by="year") %>%
  filter(father_age!=-99) %>%
  mutate(n_scaled=n/p)

# Make figure
df_mother = read.csv("data/livebirths/livebirths_processed_2002_2020.csv") %>%
  group_by(mother_age, year) %>%
  summarise(n_births=sum(n, na.rm=TRUE)) %>%
  mutate(year=factor(year), parent="Mother") %>%
  rename(age=mother_age)

df_father = read.csv("data/livebirths/livebirths_processed_2002_2020.csv") %>%
  group_by(father_age, year) %>%
  summarise(n_births=sum(n, na.rm=TRUE)) %>%
  filter(father_age!=-99, year>=2010) %>%
  mutate(year=factor(year), parent="Father") %>%
  rename(age=father_age)

df_births = rbind(df_mother, df_father) %>%
  mutate(parent=factor(parent, levels=c("Mother", "Father")),
         year=factor(year, levels=seq(2002,2020)))

my_colors <- colorRampPalette(c("#42047E", "#1DBDE6", "#5CB270"))(length(levels(df_births$year)))

plt_parentagedist = ggplot(df_births) +
  geom_line(aes(x=age, y=n_births, color=year, group=year)) +
  facet_wrap("parent", ncol=1) +
  xlim(c(0,60)) +
  xlab("Parent age") +
  ylab("Number of births") +
  scale_colour_manual(values=my_colors, name="Year") +
  theme_bw() +
  theme(strip.background = element_rect(fill="white"))
plt_parentagedist

ggsave("outputs/supplfigure3_livebirths.png", plt_parentagedist, width=20, height=14, units="cm", dpi=300)




# ------------------------------------------------------------------------------
# --------------- Visualise population data (over time) ------------------------
# ------------------------------------------------------------------------------


df_popn = read.csv("data/population/popests_2002_2020.csv") %>%
  filter(age_group!="Total", sex!="both") %>%
  group_by(age_group, sex, year) %>%
  summarise(n_pop=sum(n_pop)) %>%
  data.frame() %>%
  filter(year>=2003)

df_popn$age_group[df_popn$age_group=="90+"] = "90-94"
df_popn = df_popn %>%
  expandAgeGroups() %>%
  mutate(n_pop=n_pop/5) %>%
  appendAgeGroups(agecolname="expanded_age") %>%
  group_by(age_group, sex, year) %>%
  summarise(n_pop = sum(n_pop))

df_popn$sex[df_popn$sex=="female"] = "Female"
df_popn$sex[df_popn$sex=="male"] = "Male"

plt = ggplot(df_popn) +
  geom_point(aes(x=year,y=n_pop,color=age_group)) +
  geom_line(aes(x=year,y=n_pop,color=age_group), alpha=0.4) +
  facet_wrap("sex") +
  xlab("Year") +
  ylab("Population count") +
  theme_bw() +
  theme(legend.position="bottom", strip.background = element_rect(fill="white"), legend.title=element_text(size=10)) +
  scale_colour_viridis_d(name="Age-group", begin=1, end=0)

plt
ggsave("outputs/supplfigure4_population.png", plt, width=20, height=10, units="cm", dpi=300)



# # ------------------------------------------------------------------------------
# # ---------------- Visualise population data (in 2020) -------------------------
# # ------------------------------------------------------------------------------
# 
# # Load  data
# df_popn = read.csv("data/population/popests_simple_2010_2020.csv") %>%
#   filter(age_group != "TOTAL", year==2020, !is.na(uf), sex=="both") %>%
#   mutate(age_group = ifelse(age_group=="90+", "90", age_group), age_group = as.numeric(age_group)) %>% # TEMPORARILY replace 90+ with 90
#   group_by(age_group) %>%
#   summarise(total_pop = sum(n_pop))
# 
# plt = ggplot(df_popn) +
#   geom_bar(aes(x=age_group, y=total_pop), stat="identity", fill="skyblue") +
#   xlab("Age group") +
#   ylab("Population count") +
#   theme_bw() +
#   theme(legend.position="bottom", strip.background = element_rect(fill="white"), legend.title=element_text(size=10))
# plt
