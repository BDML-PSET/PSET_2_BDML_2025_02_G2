### setup
cat("\f")
rm(list = ls())
source('scripts/00_packages.R')

#--------------#
# 1. Load data #
#--------------#

train_hogares = import('stores/input/train_hogares.csv',setclass = 'tibble') %>% clean_names() %>% filter(depto != 11)
train_personas = import('stores/input/train_personas.csv',setclass = 'tibble') %>% clean_names() %>% filter(depto != 11)
test_hogares = import('stores/input/test_hogares.csv',setclass = 'tibble') %>% clean_names()
test_personas = import('stores/input/test_personas.csv',setclass = 'tibble') %>% clean_names()

#--------------------------------------#
# 2. Covariates available on test data #
#--------------------------------------#

vars_test_hogares = import('stores/input/test_hogares.csv',setclass = 'tibble') %>% clean_names() %>% colnames()
vars_test_personas = import('stores/input/test_personas.csv',setclass = 'tibble') %>% clean_names() %>% colnames()

#---------------------------------------------------------------------#
# 3. Select covariates that are available at both test and train data #
#---------------------------------------------------------------------#

train_personas = train_personas %>% 
                 select(all_of(c(vars_test_personas)))

train_hogares = train_hogares %>% 
                select(all_of(c(vars_test_hogares,"pobre","ingpcug")))

#----------------#
# 4. Export data #
#----------------#

export(train_personas,'stores/output/01_import/01_train_personas.rds')
export(train_hogares,'stores/output/01_import/01_train_hogares.rds')
export(test_personas,'stores/output/01_import/01_test_personas.rds')
export(test_hogares,'stores/output/01_import/01_test_hogares.rds')