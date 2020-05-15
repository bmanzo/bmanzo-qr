## 2020 Applied QR RF and GAM analysis ##

library(tidyverse) # cleaning/wrangling
library(randomForest) # random forest
library(mgcv) # GAMs
# library(gbm) # gradient boosting
library(xtable) # general latex tables
library(stargazer) # regression tables
library(splines) # splines
library(mgcViz) # ggplot for GAMs

# color palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## set some options
fig.units='in'
fig.height=3*2
fig.width=4.8*2
fig.res=300
psize=20

setwd('/Users/bmanzo/Box/QR-Review/bmanzo-qr/')

mortality = read_rds('input/pop_mort.rds')

# 2:1 train/test split
set.seed(1)
mod3 <- seq(from=2, to=nrow(mortality), by=3)
test_inds <- rep(0, length(mod3))
for(i in 1:length(test_inds)){
 test_inds[i] = mod3[i] + sample(-1:1, 1) 
}

# select data for random forest

rf_data <- dplyr::select(mortality, deaths, year, month, sex, age_group, population)

rf_data$age_group <- as.factor(rf_data$age_group)
rf_data$sex <- as.factor(rf_data$sex)
rf_data$month <- as.factor(rf_data$month)

rf_data$population <- log(rf_data$population)
rf_data$deaths <- log(rf_data$deaths)

# fit random forest with different numbers of selected variables
rf <- randomForest(x=rf_data[-test_inds, -1], y=rf_data$deaths[-test_inds], importance=TRUE,
                  xtest=rf_data[test_inds, -1], ytest=rf_data$deaths[test_inds], keep.forest=TRUE,
                  ntree=500, mtry=2)
rf2 <- randomForest(x=rf_data[-test_inds, -1], y=rf_data$deaths[-test_inds], importance=TRUE,
                         xtest=rf_data[test_inds, -1], ytest=rf_data$deaths[test_inds], keep.forest=TRUE,
                         ntree=500, mtry=1)
rf3 <- randomForest(x=rf_data[-test_inds, -1], y=rf_data$deaths[-test_inds], importance=TRUE,
                    xtest=rf_data[test_inds, -1], ytest=rf_data$deaths[test_inds], keep.forest=TRUE,
                    ntree=500, mtry=3)
preds_rf <- rf$test$predicted # obtain predicted values

test_mse_rf <- mean((preds_rf - rf_data$deaths[test_inds])^2)

# gather data for plot of MSE by number of variables
rf_plot2_data = data.frame(trees=seq(1, 500), rf_1var = rf2$mse, rf_2var = rf$mse, rf_3var = rf3$mse)

# make plot
rf_err_plot = ggplot(rf_plot2_data)+ 
  geom_line(aes(x=trees, y=rf_1var, color=cbp1[3]))+
  geom_line(aes(x=trees, y=rf_2var, color=cbp1[5]))+
  geom_line(aes(x=trees, y=rf_2var, color=cbp1[8]))+
  theme_classic(base_size=20)+
  ylab('test set MSE')+
  scale_colour_manual(name = 'mtry', 
                      values =c(cbp1[c(3, 5, 8)]), labels = c('1','2', '3'))

# save plot for report
ggsave('figs/rf_err_plot.png', 
       plot=rf_err_plot,
       units = fig.units,
       width = fig.width,
       height = fig.height,
       dpi = fig.res)

# plot variable importance
rf_df = data.frame(rf$importance)
rf_df[order(rf_df$X.IncMSE, decreasing=T),]
rf_plot = ggplot(rf_df)+
  geom_bar(aes(y=X.IncMSE, x=row.names(rf_df)), stat='identity', fill=cbp1[6])+
  theme_classic(base_size=20)+
  xlab('Variable')+
  ylab('IncMSE')

ggsave('figs/rf_plot.png', 
       plot=rf_plot,
       units = fig.units,
       width = fig.width,
       height = fig.height,
       dpi = fig.res)

# fit gradient boosting <- not included in report

# gb_data <- dplyr::select(mortality, deaths, year, month, sex, age_group, population)
# 
# gb_data$age_group <- as.factor(gb_data$age_group)
# gb_data$sex <- as.factor(gb_data$sex)
# 
# gb <- gbm(deaths~.-population+offset(log(population)), data=gb_data[-test_inds,], distribution='poisson')
# 
# preds_gb <- predict(gb, gb_data[test_inds,], n.trees=80)+log(gb_data$population)[test_inds]
# 
# test_mse_gb <- mean((preds_gb - log(gb_data$deaths[test_inds]))^2) # on log scale


# fit a GAM
gam_data <- dplyr::select(mortality, deaths, year, month, sex, age_group, population)

gam_data$age_group <- as.factor(gam_data$age_group)
gam_data$sex <- as.factor(gam_data$sex)

gam_mod <- gam(deaths~s(month, by=(age_group), bs='cc')+s(year, by=age_group)+sex+age_group+offset(log(population)),
               data=gam_data[-test_inds,], family='quasipoisson')

preds_gam <- predict(gam_mod, gam_data[test_inds,])

test_mse_gam <- mean((preds_gam - log(gam_data$deaths[test_inds]))^2) # on log scale

#plot(gam_mod, seWithMean = TRUE) # test plot of GAM from mgcv package

gam_viz = getViz(gam_mod)

# create plots of nonlinear effects from GAM
gam_plot1 = plot(gam_viz, select=2)+
  l_ciLine(color=cbp1[3])+
  l_fitLine(color=cbp1[6])+
  theme_classic(base_size=18)+
  scale_x_discrete(limits=1:12)+
  ylab('s(month):05_09')+
  ylim(c(-0.05, 0.05))+
  ggtitle('Month smooth for age 5-9')

gam_plot1; ggsave('figs/gam_plot1.png')

gam_plot2 = plot(gam_viz, select=7)+
  l_ciLine(color=cbp1[3])+
  l_fitLine(color=cbp1[6])+
  theme_classic(base_size=18)+
  scale_x_discrete(limits=1:12)+
  ylab('s(month):30_34')+
  ylim(c(-0.05, 0.05))+
  ggtitle('Month smooth for age 30-34')

gam_plot2; ggsave('figs/gam_plot2.png')

gam_plot3 = plot(gam_viz, select=16)+
  l_ciLine(color=cbp1[3])+
  l_fitLine(color=cbp1[6])+
  theme_classic(base_size=18)+
  scale_x_discrete(limits=1:12)+
  ylab('s(month):75_79')+
  ggtitle('Month smooth for age 75-79')

gam_plot3; ggsave('figs/gam_plot3.png')

gam_plot4 = plot(gam_viz, select=20)+
  l_ciLine(color=cbp1[3])+
  l_fitLine(color=cbp1[6])+
  theme_classic(base_size=18)+
  ggtitle('Year smooth for age 5-9')+
  ylab('s(year):05_09')

gam_plot4; ggsave('figs/gam_plot4.png')

gam_plot5 = plot(gam_viz, select=26)+
  l_ciLine(color=cbp1[3])+
  l_fitLine(color=cbp1[6])+
  theme_classic(base_size=18)+
  ggtitle('Year smooth for age 35-39')+
  ylab('s(year):35_39')

gam_plot5; ggsave('figs/gam_plot5.png')


gam_plot6 = plot(gam_viz, select=36)+
  l_ciLine(color=cbp1[3])+
  l_fitLine(color=cbp1[6])+
  theme_classic(base_size=18)+
  ggtitle('Year smooth for age 85-99')+
  ylab('s(year):85_99')

gam_plot6; ggsave('figs/gam_plot6.png')

summary(gam_mod)
