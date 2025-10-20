### setup
cat("\f")
rm(list = ls())
source('scripts/00_packages.R')

##==: 1. Load models
xgboost_model_weigths = import('stores/output/03_models/37_xgb_tree_weights_nrounds.rds')
xgboost_model_no_weigths = import('stores/output/03_models/41_xgb_tree_weights_nrounds_500_max_depth_7_eta_0.04_gamma_0_col_sample_0.6_min_n_25_ss_0.8.rds')

##==: 2. Split data
train = import('stores/output/02_wrangle/02_train.rds',setclass = 'tibble')
set.seed(1234)
df_split = initial_validation_split(data = train,prop = c(0.6,0.2),strata = pobre)
df_train = training(df_split)
df_validation = validation(df_split)
df_test = testing(df_split)

###==: 3. Adjusting cutoff

f_adjust_cutoff = function(model){
  
  
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
  bayes = predicted_value_test %>% 
          conf_mat(truth = pobre,estimate = predicted) %>% 
          summary() %>% 
          mutate(cutoff = 'Regla de Bayes',
                 value = 0.5)
  
  ### cutoff optimize sens and spec
  optimize_sens = predicted_value_test %>% 
                  mutate(poor = ifelse(Yes >= rfThresh$threshold,1,0),
                         poor = factor(poor,levels = c('1','0'),labels = c('Yes','No'))) %>% 
                  conf_mat(truth = pobre,estimate = poor) %>% 
                  summary() %>% 
                  mutate(cutoff = 'Sens/Sepc',
                         value = rfThresh$threshold)
  
  ### Cut off optimize f1
  optimize_f1 = predicted_value_test %>% 
                mutate(poor = ifelse(Yes >= rfThreshF1,1,0),
                       poor = factor(poor,levels = c('1','0'),labels = c('Yes','No'))) %>% 
                conf_mat(truth = pobre,estimate = poor) %>% 
                summary() %>% 
                mutate(cutoff = 'F1',
                       value = rfThreshF1)
  
  output = list(bayes,optimize_sens,optimize_f1)
  
  
}

model_weights = f_adjust_cutoff(model = xgboost_model_weigths) %>% 
                list_rbind() %>% 
                mutate(model = 'XGBoost (con pesos)')

model_no_weights = f_adjust_cutoff(model = xgboost_model_no_weigths) %>% 
                   list_rbind() %>% 
                   mutate(model = 'XGBoost (sin pesos)')

###==: 4. Make table 

single_table = bind_rows(model_weights,model_no_weights)

single_table = single_table %>% 
               filter(.metric %in% c('f_meas','precision','recall')) %>% 
               select(model,cutoff,value,metric = .metric,estimate = .estimate) %>% 
               mutate(estimate = round(estimate,3),
                      value = round(value,3)) %>% 
               pivot_wider(names_from = metric,values_from = estimate) %>% 
               arrange(cutoff,model) %>% 
               filter(cutoff != 'Sens/Sepc') %>% 
                             select('Algoritmo' = model,
                                    'Método de optimización' = cutoff,
                                    'Corte óptimo' = value,
                                    'F1' = f_meas,
                                    'Precision' = precision,
                                    'Recall' = recall) 

table = single_table %>%
        kbl(format = "latex",
            booktabs = TRUE,
            escape = FALSE,
            caption = "Resultados de la optimización de puntos de corte") %>%
        kable_styling(latex_options = "hold_position") %>% 
        footnote(general = 'Esta tabla compara la optimización del algoritmo')

### 
writeLines(table, "stores/output/04_visual/03_cut_off_adjstument.tex")
