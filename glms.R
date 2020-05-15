## 2020 Applied QR GLM analysis ##

library(tidyverse) # cleaning/wrangling
library(xtable) # general latex tables
library(stargazer) # regression tables
library(splines) # splines
library(emmeans) # estimated marginal means / contrasts

# color palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

setwd('/Users/bmanzo/Box/QR-Review/bmanzo-qr/')

mortality <- read_rds('input/pop_mort.rds')

# create train/test split
set.seed(6329)
mod3 <- seq(from=2, to=nrow(mortality), by=3)
test_inds <- rep(0, length(mod3))
for(i in 1:length(test_inds)){
  test_inds[i] = mod3[i] + sample(-1:1, 1) 
}

# calculate MSE over multiple train/test splits
calc_mse_cv = function(model, n_reps, dataset, seed){
  set.seed(seed)
  results = rep(0, n_reps)
  for(j in 1:n_reps){
    mod3 <- seq(from=2, to=nrow(mortality), by=3)
    test_inds <- rep(0, length(mod3))
    for(i in 1:length(test_inds)){
      test_inds[i] = mod3[i] + sample(-1:1, 1) 
    }
    preds = predict(model, dataset[test_inds,])
    results[j] = mean((preds - log(dataset$deaths[test_inds]))^2)
  }
  return(mean(results))
}

## set some options for saving plots
fig.units='in'
fig.height=3*2
fig.width=4.8*2
fig.res=300
psize=20

## coarse age group models
# model 3
coarse_three <- glm(deaths~as.factor(coarse_age)+as.factor(month)+year+sex+
                    as.factor(coarse_age):year+as.factor(coarse_age):as.factor(month)+
                    offset(log(population)), data=mortality[-test_inds,], family='poisson')
# model 2
coarse_two <- glm(deaths~as.factor(coarse_age)+winter+summer+year+sex+
                     as.factor(coarse_age):year+as.factor(coarse_age):winter+as.factor(coarse_age):summer+
                     offset(log(population)), 
                   family='poisson', data=mortality[-test_inds,])
# model 1
coarse_one <- glm(deaths~as.factor(coarse_age)+winter+summer+sex+year+
                    offset(log(population)), 
                  family='poisson', data=mortality[-test_inds,])


test_mse_c1 <- calc_mse_cv(coarse_one, 50, mortality, 6329)
test_mse_c2 <- calc_mse_cv(coarse_two, 50, mortality, 6329)
test_mse_cs <- calc_mse_cv(coarse_three, 50, mortality, 6329)
test_mse_c1; test_mse_c2; test_mse_cs

# try with full age range
# model 3
narrow_three <- glm(deaths~as.factor(age_group)+as.factor(month)+year+sex+
                    as.factor(age_group):year+as.factor(age_group):as.factor(month)+
                    offset(log(population)), data=mortality[-test_inds,], family='poisson')
# model 2
narrow_two <- glm(deaths~as.factor(age_group)+winter+summer+year+sex+
                    as.factor(age_group):year+as.factor(age_group):winter+as.factor(age_group):summer+
                  offset(log(population)), 
                  family='poisson', data=mortality[-test_inds,])
# model 1
narrow_one <- glm(deaths~as.factor(age_group)+winter+summer+sex +year+
                  offset(log(population)), 
                family='poisson', data=mortality[-test_inds,])

# get test set predictions
preds_n1 <- predict(narrow_one, mortality[test_inds,])
preds_n2 <- predict(narrow_two, mortality[test_inds,])
preds_ns <- predict(narrow_three, mortality[test_inds,])

# calculate test set MSE
test_mse_n1 <- calc_mse_cv(narrow_one, 50, mortality, 6329)
test_mse_n2 <- calc_mse_cv(narrow_two, 50, mortality, 6329)
test_mse_ns <- calc_mse_cv(narrow_three, 50, mortality, 6329)
test_mse_n1; test_mse_n2; test_mse_ns

# create table of model selection criteria and print to latex
mods = list(model_1_wide = coarse_one,
            model_2_wide = coarse_two,
            model_3_wide = coarse_three,
            model_1_original = narrow_one,
            model_2_original = narrow_two,
            model_3_original = narrow_three)

ICs <- imap_dfr(mods, ~tibble(Model=.y, aic = floor(AIC(.x)), bic = floor(BIC(.x))))
ICs$mse = c(test_mse_c1, test_mse_c2, test_mse_cs, test_mse_n1, test_mse_n2, test_mse_ns)
names(ICs) <- c('Model', 'AIC', 'BIC', 'Test Set MSE')
ICsX <- xtable(ICs, 
               caption='AIC, BIC, and MSE for poisson regression models', 
               label='tab:ICs',
               digits = c(0, 0, 0,0, 4))
print(ICsX,'tabs/ICs.tex', type='latex', include.rownames=FALSE)

## create stargazer table - too big to include in document
# stargazer(narrow_one, narrow_two, narrow_three, 
#           type='latex',
#           intercept.bottom=FALSE,
#           single.row=TRUE,
#           omit.table.layout = "sn", 
#           omit=':',
#           out='tabs/pois_coefs.tex',
#           label='tab:pois_coefs',
#           title='Main effect coefficients for models 1-3')

# try quasi-poisson to account for overdispersion

qp_three <- glm(deaths~as.factor(age_group)+as.factor(month)+year+sex+
                    as.factor(age_group):year+as.factor(age_group):as.factor(month)+
                    offset(log(population)), data=mortality[-test_inds,],
              family=quasipoisson)

qp_two <- glm(deaths~as.factor(age_group)+winter+summer+year+sex+
                    as.factor(age_group):year+as.factor(age_group):winter+as.factor(age_group):summer+
                  offset(log(population)), 
                  data=mortality[-test_inds,],
              family=quasipoisson)

qp_one <- glm(deaths~as.factor(age_group)+winter+summer+sex +year+
                     offset(log(population)), 
                   data=mortality[-test_inds,],
               family=quasipoisson)

# get test set predictions
preds_qp1 <- predict(qp_one, mortality[test_inds,])
preds_qp2 <- predict(qp_two, mortality[test_inds,])
preds_qp3 <- predict(qp_three, mortality[test_inds,])

# compute mse on test set
test_mse_qp1 <- calc_mse_cv(qp_one, 50, mortality, 6329)
test_mse_qp2 <- calc_mse_cv(qp_two, 50, mortality, 6329)
test_mse_qp3 <- calc_mse_cv(qp_three, 50, mortality, 6329)
test_mse_qp1; test_mse_qp2; test_mse_qp3

## only use indicators for senior citizen, middle aged

mortality$senior <- 1*(mortality$age_group %in% c('75_79', '80_84','85_99')) # use 65 because medicare age? or 75?
mortality$young_adult <- 1*(mortality$age_group) %in% c('30_34', '35_39', '40_44')

# contrasts with emmeans and emtrends

qp_yr = emtrends(qp_three, specs='age_group', var='year')
em_plot = plot(qp_yr)+theme_classic(base_size = 20)+geom_vline(aes(xintercept=0))
ggsave('figs/em_plot.png', 
       plot=em_plot,
       units = fig.units,
       width = fig.width,
       height = fig.height,
       dpi = fig.res)


fit2 = glm(deaths~as.factor(age_group)*sex+as.factor(month)+year+
                             as.factor(age_group):year+as.factor(age_group):as.factor(month)+
                             offset(log(population)), data=mortality[-test_inds,],
                       family=quasipoisson)

mf = emmeans(fit2, ~age_group*sex)
mf_table = pairs(mf)[c(18, 53, 87, 120, 152, 183, 213, 242, 270, 297, 323, 348, 372, 395, 417, 438, 458, 477),]
mf_df = data.frame(mf_table)
mf_df = dplyr::select(mf_df, -z.ratio, -df)
mf_x = xtable(mf_df, caption='Contrasts between males and females at each age group', 
              label= 'tab:mf')
print(mf_x, 'tabs/mf.tex', type='latex', include.rownames = F)

# #fit new models 
# #not used in the report 
# fit_inds <- glm(deaths~senior+young_adult+summer+winter+year+sex+
#                   sex:young_adult:summer+senior:year+offset(log(population)),
#                 data=mortality, family='poisson')
# 
# base_inds <- glm(deaths~senior+young_adult+summer+winter+year+sex+offset(log(population)),
#                  data=mortality, family='poisson')
# 
# test <- glm(deaths~as.factor(coarse_age)+as.factor(month)+sex +year+
#               offset(log(population)),data=mortality, family='poisson')
# 
# test2 <- glm(deaths~as.factor(coarse_age)+as.factor(month)+sex+year+
#                as.factor(coarse_age):year+sex:as.factor(month)+offset(log(population)),
#              data=mortality, family='poisson')