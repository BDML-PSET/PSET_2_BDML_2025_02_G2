### setup
cat("\f")
rm(list = ls())
source('scripts/00_packages.R')

#--------------#
# 1. Load data #
#--------------#

train = import('stores/output/02_wrangle/02_train.rds',setclass = 'tibble')
test = import('stores/output/02_wrangle/02_test.rds',setclass = 'tibble')

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
                     weights = wts,
                     trControl = trainControl(method = 'none',
                                              summaryFunction = multiStats,
                                              classProbs = TRUE),
                     tuneGrid = expand.grid(nrounds = c(500),
                                            max_depth = c(8),
                                            eta = c(0.03),
                                            min_child_weight = c(25),
                                            gamma = c(0),
                                            colsample_bytree = c(0.8),
                                            subsample = c(0.9)))

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
prec_recall

# Encontrar el umbral óptimo que maximiza el F1-score
rfThreshF1 = prec_recall$threshold[which.max(prec_recall$F1)]


### Hacer predicciones
predicted_value_test = tibble(predicted = predict(model,df_test)) %>% 
  bind_cols(predict(model,df_test,type = 'prob')) %>% 
  bind_cols(df_test %>% select(pobre)) 

## 0.5 Cut off
predicted_value_test %>% 
  conf_mat(truth = pobre,estimate = predicted) %>% 
  summary()

## 0.24 cutoff optimize sens and spec
predicted_value_test %>% 
  mutate(poor = ifelse(Yes >= rfThresh$threshold,1,0),
         poor = factor(poor,levels = c('1','0'),labels = c('Yes','No'))) %>% 
  conf_mat(truth = pobre,estimate = poor) %>% 
  summary()

predicted_value_test %>% 
  mutate(poor = ifelse(Yes >= rfThreshF1,1,0),
         poor = factor(poor,levels = c('1','0'),labels = c('Yes','No'))) %>% 
  conf_mat(truth = pobre,estimate = poor) %>% 
  summary()

#------------------------#
# 10.  Final predictions #
#------------------------#

final_predicitions = predict(model,test,type = 'prob') %>% 
                     bind_cols(test %>% select(id))%>% 
                     mutate(pobre = ifelse(Yes >= rfThresh$threshold,1,0)) %>% 
                     select(id,pobre)

final_predicitions_f1 = predict(model,test,type = 'prob') %>% 
                        bind_cols(test %>% select(id))%>% 
                        mutate(pobre = ifelse(Yes >= rfThreshF1,1,0)) %>% 
                        select(id,pobre)

#-------------#
# 11. Export  #
#-------------#

export(final_predicitions,file = 'stores/output/03_models/37_xgboost_case_weights_trees_500_td_8_eta_0.03_min_n_25_gamma_0_cols_0.8_ss_0.9.csv')
export(final_predicitions_f1,file = 'stores/output/03_models/37_xgboost_optimize_f1_case_weights_trees_500_td_8_eta_0.03_min_n_25_gamma_0_cols_0.8_ss_0.9.csv')
