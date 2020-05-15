## plots ##

library(tidyverse)

# color palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

setwd('/Users/bmanzo/Box/QR-Review/bmanzo-qr/')

mortality <- read_rds('input/pop_mort.rds')

## set some options
fig.units='in'
fig.height=3*2
fig.width=4.8*2
fig.res=300
psize=20

# exploratory plots - not included in the report
# ggplot(filter(mortality, !age_group %in% c('65_69', '70_74', '75_79', '80_04', '85_99')))+
#   geom_boxplot(aes(y=deaths, x=sex), fill=cbp1[1])+
#   theme_classic(base_size=17)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggplot(filter(mortality, age_group %in% c('65_69', '70_74', '75_79', '80_04', '85_99')))+
#   geom_boxplot(aes(y=deaths, x=age_group), fill=cbp1[1])+
#   theme_classic(base_size=17)+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
box_plot = ggplot(mortality)+
  geom_boxplot(aes(y=death_rate, x=age_group), fill=cbp1[1])+
  theme_classic(base_size=20)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave('figs/box_plot.png', 
       plot = box_plot,
       units = fig.units,
       width = fig.width,
       height = fig.height,
       dpi = fig.res)

# plots for sex/age/month
coarse_age_plot_male_young = ggplot(filter(mortality, sex=='Male', coarse_age<3))+
  geom_smooth(aes(x=month, y=death_rate*100, color=as.factor(coarse_age)))+
  scale_color_manual(values=cbp1[-1], labels=c('0-14', '15-29', '30-44'), name='age group')+
  scale_x_discrete(limits=1:12)+
  ggtitle('Males aged 0-44')

coarse_age_plot_male_young+theme_classic(base_size=20)

ggsave('figs/young_men.png', 
       plot = coarse_age_plot_male_young+theme_classic(base_size=20),
       units = fig.units,
       width = fig.width,
       height = fig.height,
       dpi = fig.res)

coarse_age_plot_female_young = ggplot(filter(mortality, sex=='Female', coarse_age<3))+
  geom_smooth(aes(x=month, y=death_rate*100, color=as.factor(coarse_age)))+
  scale_color_manual(values=cbp1[-1], labels=c('0-14', '15-29', '30-44'), name='age group')+
  scale_x_discrete(limits=1:12)+
  ggtitle('Females aged 0-44')

coarse_age_plot_female_young+theme_classic(base_size=12)

ggsave('figs/young_women.png', 
       plot = coarse_age_plot_female_young+theme_classic(base_size=20),
       units = fig.units,
       width = fig.width,
       height = fig.height,
       dpi = fig.res)

coarse_age_plot_male_old = ggplot(filter(mortality, sex=='Male', coarse_age>=3))+
  geom_smooth(aes(x=month, y=death_rate*100, color=as.factor(coarse_age)))+
  scale_color_manual(values=cbp1[-1], labels=c('45-59', '60_74', '75+'), name='age group')+
  scale_x_discrete(limits=1:12)+
  ggtitle('Males aged 45+')

coarse_age_plot_male_old+theme_classic(base_size=12)

ggsave('figs/old_men.png', 
       plot = coarse_age_plot_male_old+theme_classic(base_size=20),
       units = fig.units,
       width = fig.width,
       height = fig.height,
       dpi = fig.res)

coarse_age_plot_female_old = ggplot(filter(mortality, sex=='Female', coarse_age>=3))+
  geom_smooth(aes(x=month, y=death_rate*100, color=as.factor(coarse_age)))+
  scale_color_manual(values=cbp1[-1], labels=c('45-59', '60_74', '75+'), name='age group')+
  scale_x_discrete(limits=1:12)+
  ggtitle('Females aged 45+')

coarse_age_plot_female_old+theme_classic(base_size=12)

ggsave('figs/old_women.png', 
       plot = coarse_age_plot_female_old+theme_classic(base_size=20),
       units = fig.units,
       width = fig.width,
       height = fig.height,
       dpi = fig.res)

## also make age/year plots

coarse_age_plot_year_young_male = ggplot(filter(mortality, coarse_age<3, sex=='Male'))+
  geom_smooth(aes(x=year, y=death_rate*100, color=as.factor(coarse_age)))+
  scale_color_manual(values=cbp1[-1], labels=c('0-14', '15-29', '30-44'), name='age group')+
  ggtitle('Males aged 0-44')

coarse_age_plot_year_young_male+theme_classic(base_size=12)

ggsave('figs/young_men_year.png', 
       plot = coarse_age_plot_year_young_male+theme_classic(base_size=20),
       units = fig.units,
       width = fig.width,
       height = fig.height,
       dpi = fig.res)

coarse_age_plot_year_young_female = ggplot(filter(mortality, coarse_age<3, sex=='Female'))+
  geom_smooth(aes(x=year, y=death_rate*100, color=as.factor(coarse_age)))+
  scale_color_manual(values=cbp1[-1], labels=c('0-14', '15-29', '30-44'), name='age group')+
  ggtitle('Females aged 0-44')

coarse_age_plot_year_young_female+theme_classic(base_size=12)

ggsave('figs/young_women_year.png', 
       plot = coarse_age_plot_year_young_female+theme_classic(base_size=20),
       units = fig.units,
       width = fig.width,
       height = fig.height,
       dpi = fig.res)


coarse_age_plot_year_old_male = ggplot(filter(mortality, coarse_age>=3, sex=='Male'))+
  geom_smooth(aes(x=year, y=death_rate*100, color=as.factor(coarse_age)))+
  scale_color_manual(values=cbp1[-1], labels=c('45-59', '60_74', '75+'), name='age group')+
  ggtitle('Males aged 45+')

coarse_age_plot_year_old_male+theme_classic(base_size=12)

ggsave('figs/old_men_year.png', 
       plot = coarse_age_plot_year_old_male+theme_classic(base_size=20),
       units = fig.units,
       width = fig.width,
       height = fig.height,
       dpi = fig.res)


coarse_age_plot_year_old_female = ggplot(filter(mortality, coarse_age>=3, sex=='Female'))+
  geom_smooth(aes(x=year, y=death_rate*100, color=as.factor(coarse_age)))+
  scale_color_manual(values=cbp1[-1], labels=c('45-59', '60_74', '75+'), name='age group')+
  ggtitle('Females aged 45+')

coarse_age_plot_year_old_female+theme_classic(base_size=12)

ggsave('figs/old_women_year.png', 
       plot = coarse_age_plot_year_old_female+theme_classic(base_size=20),
       units = fig.units,
       width = fig.width,
       height = fig.height,
       dpi = fig.res)

