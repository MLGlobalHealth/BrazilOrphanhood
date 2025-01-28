
rm(list=ls())

library(tidyverse)

orphanhood_samples = readRDS("samples/orphanhood_age_group_child_age_sex_uf_excess.RDS")

uf_lbls = read.csv("data/UF_labels.csv")

df_popn_children = read.csv("data/population/popests_simple_2010_2020.csv") %>%
  filter(age_group %in% paste0(seq(0, 17)), year==2020, !is.na(uf), sex!="both") %>%
  mutate(age_group = as.numeric(age_group)) %>%
  group_by(uf, age_group) %>%
  summarise(n_children=sum(n_pop)) %>%
  left_join(uf_lbls, by="uf") %>%
  rename(Region=uf_name, child_age=age_group) %>%
  select(Region, n_children, child_age, uf)


df_orphanhood_excess = orphanhood_samples %>%
  group_by(child_age, uf, iter) %>%
  summarise(total_orphanhood = sum(orphanhood, na.rm=TRUE)) %>%
  group_by(child_age, uf) %>%
  summarise(mean=mean(total_orphanhood), lower=quantile(total_orphanhood, 0.025), upper=quantile(total_orphanhood, 0.975)) %>%
  left_join(uf_lbls, by="uf") %>%
  left_join(df_popn_children, by=c("uf", "child_age")) %>%
  mutate(mean=1000*mean/n_children, lower=1000*lower/n_children, upper=1000*upper/n_children)

plt_childage = ggplot(df_orphanhood_excess) + 
  geom_line(aes(x=child_age, y=mean)) +
  geom_ribbon(aes(x=child_age, ymin=lower, ymax=upper), alpha=0.2) +
  facet_wrap(~uf_name, ncol=5) +
  theme_bw() +
  theme(strip.background=element_rect(fill=NA)) +
  xlab("Age of child") +
  ylab("Orphanhood (per 1000 children)")

plt_childage
ggsave("outputs/supplfigure5_regionalchildage.png", plt_childage, dpi=300, width=25, height=20, units="cm")
