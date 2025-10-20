### setup
cat("\f")
rm(list = ls())
source("scripts/00_packages.R")

#--------------#
# 1. Load data #
#--------------#

train = import('stores/output/02_wrangle/02_train.rds',setclass = 'tibble')
test = import('stores/output/02_wrangle/02_test.rds',setclass = 'tibble')
model = import("stores/output/03_models/41_xgb_tree_weights_nrounds_500_max_depth_7_eta_0.04_gamma_0_col_sample_0.6_min_n_25_ss_0.8.rds")

#---------------#
# 2. Split data #
#---------------#

set.seed(1234)
df_split = initial_validation_split(data = train,prop = c(0.6,0.2),strata = pobre)
df_train = training(df_split)
df_validation = validation(df_split)
df_test = testing(df_split)

#--------#
# 3. VIP #
#--------#

pfun = function(object,newdata){
  stats::predict(object,newdata = newdata)
}

set.seed(20205)
p1 = vi(object = model,
        method = 'permute',
        train = df_train,
        target = 'pobre',
        metric = yardstick::f_meas_vec,
        pred_wrapper = pfun,
        smaller_is_better = FALSE )

top_vars_10 <- p1 %>%
            arrange(desc(Importance)) %>%
            slice_head(n = 10) %>%  pull(Variable)

df_train = df_train %>% select(pobre, all_of(top_vars_10))
df_test = df_test %>% select(pobre, all_of(top_vars_10))
df_validation = df_validation %>% select(pobre, all_of(top_vars_10))


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
                     trControl = trainControl(method = "none",
                                              summaryFunction = multiStats,
                                              classProbs = TRUE,
                                              verboseIter = TRUE),
                     tuneGrid = expand.grid(nrounds = c(500),
                                            max_depth = c(7),
                                            eta = c(0.04),
                                            min_child_weight = c(25),
                                            gamma = c(0),
                                            colsample_bytree = c(0.6),
                                            subsample = c(0.8)))


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

