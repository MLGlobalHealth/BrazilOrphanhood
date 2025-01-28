
rm(list=ls())

library(tidyverse)
library(zoo)
library(geobr)
library(viridis)
library(cowplot)
library(clipr)

source("R/supportFunctions.R")
# source("R/sampleParentalOrphanhood.R")

uf_lbls = read.csv("data/UF_labels.csv")

# Load population data
df_popn = read.csv("data/population/popests_simple_2010_2020.csv") %>%
  filter(age_group %in% paste0(seq(0, 17)), year==2020, !is.na(uf), sex!="both") %>%
  group_by(uf) %>%
  summarise(n_children=sum(n_pop)) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  select(Region, n_children)

# Append the national total number of children as a new row
df_popn = df_popn %>% rbind(data.frame(Region="Total", n_children=sum(df_popn$n_children)))



# ------------------------------------------------------------------------------
# ----------------------- Load orphanhood samples ------------------------------
# ------------------------------------------------------------------------------

df_allcause_elderly = readRDS("samples/coresorphanhood_any_age_group_sex_uf_allcause.RDS") %>%
  select(-child_age) %>%
  mutate(source="All-cause", type="Coresident elderly")

df_allcause_parental = readRDS("samples/orphanhood_age_group_sex_uf_allcause.RDS") %>%
  group_by(uf, iter) %>%
  summarise(orphanhood=sum(orphanhood)) %>%
  ungroup() %>%
  mutate(source="All-cause", type="Parental")

df_excess_elderly = readRDS("samples/coresorphanhood_any_age_group_sex_uf_excess.RDS") %>% 
  select(-child_age) %>%
  mutate(source="Excess", type="Coresident elderly")

df_excess_parental = readRDS("samples/orphanhood_age_group_sex_uf_excess.RDS") %>%
  group_by(uf, iter) %>%
  summarise(orphanhood=sum(orphanhood)) %>%
  ungroup() %>%
  mutate(source="Excess", type="Parental")

df = rbind(
  df_allcause_elderly,
  df_allcause_parental,
  df_excess_elderly,
  df_excess_parental
)

rm(df_allcause_elderly, df_allcause_parental, df_excess_elderly, df_excess_parental)



# ------------------------------------------------------------------------------
# ---------------- Calculate per-1000-children estimates -----------------------
# ------------------------------------------------------------------------------

df1 = df %>%
  group_by(uf, source, type) %>%
  summarise(orphanhood=mean(orphanhood)) %>%
  ungroup() %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  select(-uf, -uf_code)

df2 = df1 %>%
  left_join(df_popn, by="Region") %>%
  mutate(orphanhood=orphanhood/n_children*1000) %>%
  select(-n_children)

df = rbind(
  df1 %>% mutate(Scale="Total"),
  df2 %>% mutate(Scale="Standardised")
)  

rm(df1, df2)



# ------------------------------------------------------------------------------
# ------------- Load Brazil shapefile and append estimates ---------------------
# ------------------------------------------------------------------------------

brsf_in = read_state(year=2020)

brsf = brsf_in %>%
  rename(uf=code_state) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name) %>%
  right_join(df, by="Region") %>%
  mutate(Group=factor(paste(source, type),
                      levels=c("All-cause Parental", "All-cause Elderly", "Excess Parental", "Excess Elderly")))

rm(brsf_in)



# ------------------------------------------------------------------------------
# --------------------- Construct the figure and save --------------------------
# ------------------------------------------------------------------------------

# Plot totals
df_plt_topleft = brsf %>%
  filter(source=="Excess", Scale=="Total") %>%
  select(Region, geom, orphanhood, type) %>%
  mutate(type = factor(case_when(type=="Parental" ~ "(e) COVID-19-associated orphanhood\nTotal",
                                 type=="Coresident elderly" ~ "(g) COVID-19-associated loss of older kin\nTotal"),
                       levels = c("(e) COVID-19-associated orphanhood\nTotal", "(g) COVID-19-associated loss of older kin\nTotal")))

plt_topleft = ggplot(df_plt_topleft) +
  geom_sf(aes(fill=orphanhood)) +
  scale_fill_viridis(option="magma", direction=-1, name=NULL) +
  facet_wrap(~type, ncol=1) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  theme(strip.background = element_blank(),
        legend.position="left")  +
  guides(fill = guide_colourbar(barheight = 10))


df_plt_topright = brsf %>%
  filter(source=="Excess", Scale=="Standardised") %>%
  select(Region, geom, orphanhood, type) %>%
  mutate(type = factor(case_when(type=="Parental" ~ "(f) COVID-19-associated orphanhood\nPer 1000 children",
                                 type=="Coresident elderly" ~ "(h) COVID-19-associated loss of older kin\nPer 1000 children"),
                       levels = c("(f) COVID-19-associated orphanhood\nPer 1000 children", "(h) COVID-19-associated loss of older kin\nPer 1000 children")))

plt_topright = ggplot(df_plt_topright) +
  geom_sf(aes(fill=orphanhood)) +
  scale_fill_viridis(option="magma", direction=-1, name=NULL) +
  facet_wrap(~type, ncol=1) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  theme(strip.background = element_blank(),
        legend.position="right") +
  guides(fill = guide_colourbar(barheight = 10))


df_plt_bottomleft = brsf %>%
  filter(source=="All-cause", Scale=="Total") %>%
  select(Region, geom, orphanhood, type) %>%
  mutate(type = factor(case_when(type=="Parental" ~ "(a) All-cause orphanhood\nTotal",
                                 type=="Coresident elderly" ~ "(c) All-cause loss of older kin\nTotal"),
                       levels = c("(a) All-cause orphanhood\nTotal", "(c) All-cause loss of older kin\nTotal")))

plt_bottomleft = ggplot(df_plt_bottomleft) +
  geom_sf(aes(fill=orphanhood)) +
  scale_fill_viridis(option="mako", direction=-1, name=NULL) +
  facet_wrap(~type, ncol=1) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  theme(strip.background = element_blank(),
        legend.position="left") +
  guides(fill = guide_colourbar(barheight = 10))


df_plt_bottomright = brsf %>%
  filter(source=="All-cause", Scale=="Standardised") %>%
  select(Region, geom, orphanhood, type) %>%
  mutate(type = factor(case_when(type=="Parental" ~ "(b) All-cause orphanhood\nPer 1000 children",
                                 type=="Coresident elderly" ~ "(d) All-cause loss of older kin\nPer 1000 children"),
                       levels = c("(b) All-cause orphanhood\nPer 1000 children", "(d) All-cause loss of older kin\nPer 1000 children")))

plt_bottomright = ggplot(df_plt_bottomright) +
  geom_sf(aes(fill=orphanhood)) +
  scale_fill_viridis(option="mako", direction=-1, name=NULL) +
  facet_wrap(~type, ncol=1) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  theme(strip.background = element_blank(),
        legend.position="right") +
  guides(fill = guide_colourbar(barheight = 10))



plt = plot_grid(plt_bottomleft, plt_bottomright, plt_topleft, plt_topright, ncol=2) + theme(plot.background = element_rect(fill = "white", color = NA))
ggsave("outputs/figure1_map.png", plot=plt, width=22, height=30, units="cm", dpi=300)



# ------------------------------------------------------------------------------
# ------------------------ Also make github version ----------------------------
# ------------------------------------------------------------------------------




plot_list_gh = plot_list[c(1, 5, 7)]
plot_list_gh[[1]] = plot_list_gh[[1]] + ggtitle("All-cause parental orphanhood") + theme(legend.position="none")
plot_list_gh[[2]] = plot_list_gh[[2]] + ggtitle("COVID-19-associated parental orphanhood") + theme(legend.position="bottom") + guides(fill=guide_colourbar(barwidth=20))
plot_list_gh[[3]] = plot_list_gh[[3]] + ggtitle("COVID-19-associated loss of grandparents or older kin") + theme(legend.position="none")

for (ii in seq(1,3)) {
  plot_list_gh[[ii]] = plot_list_gh[[ii]] +
    theme(plot.title=element_text(size=10)) +
    scale_fill_viridis(option="magma", direction=-1, name=NULL, limits = c(0, 140000), labels = scales::label_number(big.mark = ","))
}

plt_choropleth_github = plot_grid(plotlist = plot_list_gh, ncol=4, align="h", axis="tblr")
ggsave("outputs/github/map.png", plot=plt_choropleth_github, units="cm", dpi=300, width=36, height=15)

