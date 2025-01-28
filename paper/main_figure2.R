
rm(list=ls())

library(tidyverse)

df_orphanhood_excess = readRDS("samples/orphanhood_age_group_child_age_sex_uf_excess.RDS") %>%
  group_by(child_age, sex, iter) %>%
  summarise(total_orphanhood = sum(orphanhood, na.rm=TRUE)) %>%
  group_by(child_age, sex) %>%
  summarise(mean=mean(total_orphanhood), lower=quantile(total_orphanhood, 0.025), upper=quantile(total_orphanhood, 0.975))

df_orphanhood_allcause = readRDS("samples/orphanhood_age_group_child_age_sex_uf_allcause.RDS") %>%
  group_by(child_age, sex, iter) %>%
  summarise(total_orphanhood = sum(orphanhood, na.rm=TRUE)) %>%
  group_by(child_age, sex) %>%
  summarise(mean=mean(total_orphanhood), lower=quantile(total_orphanhood, 0.025), upper=quantile(total_orphanhood, 0.975))

df_orphanhood_excess$sex = factor(case_when(df_orphanhood_excess$sex=="female" ~ "loss of mother",
                                            df_orphanhood_excess$sex=="male" ~ "loss of father",
                                            df_orphanhood_excess$sex=="both" ~ "loss of both parents"),
                           levels = c("loss of father", "loss of mother", "loss of both parents"))

df_orphanhood_allcause$sex = factor(case_when(df_orphanhood_allcause$sex=="female" ~ "loss of mother",
                                              df_orphanhood_allcause$sex=="male" ~ "loss of father",
                                              df_orphanhood_allcause$sex=="both" ~ "loss of both parents"),
                                  levels = c("loss of father", "loss of mother", "loss of both parents"))

df_orphanhood = rbind(df_orphanhood_excess %>% mutate(Type="COVID-19 associated"),
                      df_orphanhood_allcause %>% mutate(Type="All-cause")) %>%
  mutate(Label = factor(paste(Type, sex), levels=c("COVID-19 associated loss of father",
                                                   "COVID-19 associated loss of mother",
                                                   "COVID-19 associated loss of both parents",
                                                   "All-cause loss of father",
                                                   "All-cause loss of mother",
                                                   "All-cause loss of both parents")))

plt_childage = ggplot(df_orphanhood) + 
  geom_col(aes(x=child_age, y=mean), alpha=0.5) +
  geom_errorbar(aes(x=child_age, ymin=lower, ymax=upper)) +
  facet_wrap(~Label, ncol=3, scales="free_y") +
  theme_bw() +
  theme(strip.background=element_rect(fill=NA)) +
  xlab("Age of child") +
  ylab("Number of children")

plt_childage
ggsave("outputs/figure2_childage.png", plot=plt_childage, width=25, height=15, units="cm", dpi=300)
