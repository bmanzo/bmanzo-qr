## 2020 Applied QR Input File ##

library(tidyverse)

setwd('/Users/bmanzo/Box/QR-Review/bmanzo-qr/input')

raw_data <- read_csv('pop_mort.csv')

head(raw_data)

# make names lowercase
names(raw_data) <- tolower(names(raw_data))

# coarsen age groups
age_groups = as.factor(unique(raw_data$age_group))
coarse_age = c(rep(0, 3), rep(1, 3), rep(2, 3), 
                   rep(3, 3), rep(4, 3), rep(5, 3))
age_table = data.frame(age_groups, coarse_age)

# calculate death rate
raw_data$death_rate = raw_data$deaths / raw_data$population

# add coarser age groups
raw_new = left_join(raw_data, age_table,by=c('age_group'='age_groups'))

# add winter/summer indicators
raw_new2 = mutate(raw_new, 
                 winter = 1*(month %in% c(1, 2, 12)),
                 summer = 1*(month %in% c(6, 7, 8))
                 )

# write to rds file
write_rds(raw_new2, 'pop_mort.rds')