### setup
cat("\f")
rm(list = ls())
source("scripts/00_packages.R")

#--------------#
# 1. Load data #
#--------------#

train = import('stores/output/02_wrangle/02_train.rds',setclass = 'tibble')
test = import('stores/output/02_wrangle/02_test.rds',setclass = 'tibble')

train_income = import("stores/output/03_models/00_rf_train.csv",setclass = 'tibble')
test_income = import("stores/output/03_models/00_rf_test.csv",setclass = 'tibble')

train = left_join(train,train_income,by = c('id')) %>% select(-id) %>% mutate(lp = log(lp+1))
test = left_join(test,test_income,by = c('id')) 
rm(train_income,test_income)

#---------------#
# 2. Split data #
#---------------#

set.seed(1234)
df_split = initial_validation_split(data = train,prop = c(0.6,0.2),strata = pobre)
df_train = training(df_split)
df_validation = validation(df_split)
df_test = testing(df_split)

# Constructir los ponderadores
pos_weight <- sum(df_train$pobre == 'No') / sum(df_train$pobre == 'Yes')
pos_weight

wts = ifelse(df_train$pobre == 'Yes', round(pos_weight,0), 1) 

#----------------#
# 3. Train model #
#----------------#

multiStats <- function(...) c(twoClassSummary(...), defaultSummary(...), prSummary(...))

set.seed(123)
model = caret::train(pobre ~ .,
                     data = df_train,
                     method = "xgbTree",
                     metric = 'F',
                     nthread = (parallel::detectCores() - 1), 
                     trControl = trainControl(method = 'CV',
                                              number = 5,
                                              summaryFunction = multiStats,
                                              classProbs = TRUE,
                                              verboseIter = TRUE),
                     tuneGrid = expand.grid(nrounds = c(300,500,600),
                                            max_depth = c(4,6,8),
                                            eta = c(0.001,0.01,0.03,0.05),
                                            min_child_weight = c(15,20,25),
                                            gamma = c(0,1),
                                            colsample_bytree = c(0.6,0.7),
                                            subsample = c(0.9,1)))

#------------------#
# 4. Adjust cutoff #
#------------------#

predicted_value_validation = predict(model,df_validation,type = 'prob') %>% 
                            bind_cols(df_validation %>% select(pobre))

### Optimize SENS SPEC
rfROC = roc(predicted_value_validation$pobre, predicted_value_validation$Yes, levels = rev(levels(predicted_value_validation$pobre)))
rfThresh = coords(rfROC, x = "best", best.method = "closest.topleft");rfThresh

### Optimize F1
roc_obj_rf =  roc(response = predicted_value_validation$pobre,  # Valores reales de la variable objetivo
                  predictor = predicted_value_validation$Yes, # Probabilidades predichas por el modelo
                  levels = c("No", "Yes"),  # # Establece la referencia control y caso (empleado = negativo, desempleado = positivo) 
                  direction = "<") 

prec_recall = data.frame(coords(roc_obj_rf, seq(0,1,length=100), ret=c("threshold", "precision", "recall")))
prec_recall = prec_recall %>% mutate(F1=(2*precision*recall)/(precision+recall))

# Encontrar el umbral óptimo que maximiza el F1-score
rfThreshF1 = prec_recall$threshold[which.max(prec_recall$F1)];rfThreshF1

### Hacer predicciones
predicted_value_test = tibble(predicted = predict(model,df_test)) %>% 
                       bind_cols(predict(model,df_test,type = 'prob')) %>% 
                       bind_cols(df_test %>% select(pobre)) 

## 0.5 Cut off
predicted_value_test %>% 
  conf_mat(truth = pobre,estimate = predicted) %>% 
  summary()

## cutoff optimize sens and spec
predicted_value_test %>% 
  mutate(poor = ifelse(Yes >= rfThresh$threshold,1,0),
         poor = factor(poor,levels = c('1','0'),labels = c('Yes','No'))) %>% 
  conf_mat(truth = pobre,estimate = poor) %>% 
  summary()

## optimize F1
predicted_value_test %>% 
  mutate(poor = ifelse(Yes >= rfThreshF1,1,0),
         poor = factor(poor,levels = c('1','0'),labels = c('Yes','No'))) %>% 
  conf_mat(truth = pobre,estimate = poor) %>% 
  summary()

predicted_value_test %>% 
  mutate(poor = ifelse(Yes >= rfThreshF1,1,0),
         poor = factor(poor,levels = c('1','0'),labels = c('Yes','No'))) %>% 
  count(poor) %>% 
  mutate(prop = n/sum(n))

#------------------------#
# 10.  Final predictions #
#------------------------#

predictions_bayes = test %>% 
                    mutate(pobre = predict(model,test),
                          pobre = ifelse(pobre == 'Yes',1,0)) %>% 
                    select(id,pobre) 

final_predicitions = predict(model,test,type = 'prob') %>% 
                     bind_cols(test %>% select(id))%>% 
                     mutate(pobre = ifelse(Yes >= rfThresh$threshold,1,0)) %>% 
                     select(id,pobre)

final_predicitions_f1 = predict(model,test,type = 'prob') %>% 
                        bind_cols(test %>% select(id))%>% 
                        mutate(pobre = ifelse(Yes >= rfThreshF1,1,0)) %>% 
                        select(id,pobre)

final_predicitions_f1$pobre %>% tabyl()

predictions_bayes %>% 
  count(pobre) %>% 
  mutate(prop = n/sum(n))

#------------#
# 11. Export #
#------------#
export(model,'02_model/output/00_final_model_xgboost.rds')
export(predictions_bayes,file = '02_model/output/00_final_model_xgboost_weights_optimize_bayes_rule.csv')
export(final_predicitions,file = '02_model/output/00_final_model_xgboost_weights_optimize_sens.csv')
export(final_predicitions_f1,file = '02_model/output/00_final_model_xgboost_weights_optimize_f1.csv')
