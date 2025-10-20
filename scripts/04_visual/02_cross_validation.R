### setup
cat("\f")
rm(list = ls())
source('scripts/00_packages.R')

##==: 1. Load models
logit = import('stores/output/03_models/01_model_logit.rds')
logit_en = import('stores/output/03_models/02_model_logit_EN.rds')
cart = import('stores/output/03_models/03_model_cart.rds')
rf = import('stores/output/03_models/33_rf_tree_weights_nrounds.rds')
xgboost_model_weigths = import('stores/output/03_models/37_xgb_tree_weights_nrounds.rds')
xgboost_model_no_weigths = import('stores/output/03_models/41_xgb_tree_weights_nrounds_500_max_depth_7_eta_0.04_gamma_0_col_sample_0.6_min_n_25_ss_0.8.rds')

##==: 2. Extract values of searched hyperparameters 

hyper_logit_en = logit_en$resampledCM %>% 
                 select(-cell1,-cell2,-cell3,-cell4,-Resample) %>% 
                 summarise(across(everything(),.fns = list('min' = min,'max' = max))) %>% 
                 pivot_longer(everything()) %>% 
                 mutate(stat = str_extract_all(name,'min|max') %>% as.character(),
                       name = str_remove_all(name,'_min|_max') %>% as.character()) %>% 
                 group_by(name)  %>% 
                 mutate(range = paste0('[',min(value),'-',max(value),']')) %>% 
                 distinct(name,range)

hyper_cart = cart$resampledCM %>% 
              select(-cell1,-cell2,-cell3,-cell4,-Resample) %>% 
              summarise(across(everything(),.fns = list('min' = function(x) round(min(x),4),
                                                        'max' = function(x) round(max(x),4)))) %>% 
              pivot_longer(everything()) %>% 
              mutate(stat = str_extract_all(name,'min|max') %>% as.character(),
                     name = str_remove_all(name,'_min|_max') %>% as.character()) %>% 
              group_by(name)  %>% 
              mutate(range = paste0('[',min(value),'-',max(value),']')) %>% 
              distinct(name,range)

hyper_rf = rf$resampledCM %>% 
           select(-cell1,-cell2,-cell3,-cell4,-Resample,-splitrule) %>%
           summarise(across(everything(),.fns = list('min' = function(x) round(min(x),4),
                                                     'max' = function(x) round(max(x),4)))) %>% 
           pivot_longer(everything()) %>% 
           mutate(stat = str_extract_all(name,'min|max') %>% as.character(),
                  name = str_remove_all(name,'_min|_max') %>% as.character()) %>% 
           group_by(name)  %>% 
           mutate(range = paste0('[',min(value),'-',max(value),']')) %>% 
           distinct(name,range)

hyper_xgboost_weights = xgboost_model_weigths$resampledCM %>% 
                        select(-cell1,-cell2,-cell3,-cell4,-Resample) %>%
                        summarise(across(everything(),.fns = list('min' = function(x) round(min(x),4),
                                                                  'max' = function(x) round(max(x),4)))) %>% 
                        pivot_longer(everything()) %>% 
                        mutate(stat = str_extract_all(name,'min|max') %>% as.character(),
                               name = str_remove_all(name,'_min|_max') %>% as.character()) %>% 
                        group_by(name)  %>% 
                        mutate(range = paste0('[',min(value),'-',max(value),']')) %>% 
                        distinct(name,range)


hyper_xgboost_no_weights = xgboost_model_no_weigths$resampledCM %>% 
                           select(-cell1,-cell2,-cell3,-cell4,-Resample) %>%
                           summarise(across(everything(),.fns = list('min' = function(x) round(min(x),4),
                                                                     'max' = function(x) round(max(x),4)))) %>% 
                           pivot_longer(everything()) %>% 
                           mutate(stat = str_extract_all(name,'min|max') %>% as.character(),
                                  name = str_remove_all(name,'_min|_max') %>% as.character()) %>% 
                           group_by(name)  %>% 
                           mutate(range = paste0('[',min(value),'-',max(value),']')) %>% 
                           distinct(name,range)

##==: 3. Extract values of best hyperparameters

best_values_cv_logit = logit$results %>% 
                       select(parameter,starts_with('F'),starts_with('Recall'),starts_with('Precision')) %>% 
                       mutate(across(.cols = 'F':PrecisionSD,.fns = function(x) round(x,3)))
                        

best_values_cv_logit_en = logit_en$results %>% 
                          filter(alpha == logit_en$bestTune$alpha & lambda == logit_en$bestTune$lambda) %>% 
                          select(alpha,lambda,starts_with('F'),starts_with('Recall'),starts_with('Precision')) %>% 
                          mutate(across(.cols = 'F':PrecisionSD,.fns = function(x) round(x,3)))

best_values_cv_cart = cart$results %>% 
                      filter(cp == cart$bestTune$cp) %>% 
                      select(cp,starts_with('F'),starts_with('Recall'),starts_with('Precision')) %>% 
                      mutate(across(.cols = 'F':PrecisionSD,.fns = function(x) round(x,3)),
                             cp  = round(cp,4)) 

best_values_cv_rf = rf$results %>% 
                    filter(mtry == rf$bestTune$mtry & min.node.size == rf$bestTune$min.node.size)%>% 
                    select(mtry,min.node.size,starts_with('F'),starts_with('Recall'),starts_with('Precision')) %>% 
                    mutate(across(.cols = 'F':PrecisionSD,.fns = function(x) round(x,3)))

best_values_xgboost_weights = xgboost_model_weigths$results %>% 
                              filter(eta == xgboost_model_weigths$bestTune$eta &
                                     max_depth == xgboost_model_weigths$bestTune$max_depth & 
                                     gamma == xgboost_model_weigths$bestTune$gamma &
                                     colsample_bytree == xgboost_model_weigths$bestTune$colsample_bytree &
                                     min_child_weight == xgboost_model_weigths$bestTune$min_child_weight &
                                     subsample == xgboost_model_weigths$bestTune$subsample &  
                                     nrounds == xgboost_model_weigths$bestTune$nrounds) %>% 
                              select(eta,max_depth,gamma,colsample_bytree,min_child_weight,subsample,nrounds,
                                     starts_with('F'),starts_with('Recall'),starts_with('Precision')) %>% 
                              mutate(across(.cols = 'F':PrecisionSD,.fns = function(x) round(x,3)))


best_values_xgboost_no_weights = xgboost_model_no_weigths$results %>% 
                                 filter(eta == xgboost_model_no_weigths$bestTune$eta &
                                         max_depth == xgboost_model_no_weigths$bestTune$max_depth & 
                                         gamma == xgboost_model_no_weigths$bestTune$gamma &
                                         colsample_bytree == xgboost_model_no_weigths$bestTune$colsample_bytree &
                                         min_child_weight == xgboost_model_no_weigths$bestTune$min_child_weight &
                                         subsample == xgboost_model_no_weigths$bestTune$subsample &  
                                         nrounds == xgboost_model_no_weigths$bestTune$nrounds) %>% 
                                select(eta,max_depth,gamma,colsample_bytree,min_child_weight,subsample,nrounds,
                                       starts_with('F'),starts_with('Recall'),starts_with('Precision')) %>% 
                                mutate(across(.cols = 'F':PrecisionSD,.fns = function(x) round(x,3)))

##==: 5. Make table

### 5.1 Models
base_df = tibble(model = c('Logit','Logit \nElastic Net','CART','Random Forest','XGBoost (con pesos)','XGBoost (sin pesos)'),
                 pesos = c('No','No','No','No','Si','No'))

### 5.2 Grid Search
base_df = base_df %>%
          mutate(grid_search = c('',
                                 paste0(hyper_logit_en$name,' ',hyper_logit_en$range,collapse = ' \\\\ '),
                                 paste0(hyper_cart$name,' ',hyper_cart$range,collapse = ' \\\\ '),
                                 paste0(hyper_rf$name,' ',hyper_rf$range,collapse = ' \\\\ '),
                                 paste0(hyper_xgboost_weights$name,' ',hyper_xgboost_weights$range,collapse = ' \\\\ '),
                                 paste0(hyper_xgboost_no_weights$name,' ',hyper_xgboost_no_weights$range,collapse = ' \\\\ ')))

base_df = base_df %>%
          mutate(grid_search = paste0("\\makecell[l]{", grid_search, "}")) 


### 5.3 Best values
best_values = map(list(best_values_cv_logit,best_values_cv_logit_en,best_values_cv_cart,
         best_values_cv_rf,best_values_xgboost_weights,best_values_xgboost_no_weights),
    .f = function(x){
      
      df = x %>% 
        select(-starts_with('F'),-starts_with('Recall'),-starts_with('Precision')) %>% 
        pivot_longer(everything())
      
      string =  paste0('\\makecell[l]{',
                       paste0(df$name,' ',df$value,collapse = ' \\\\ '),
                       '}')
      
      return(string)
      
    } ) %>% unlist()

base_df = base_df %>% 
          mutate(best_values = best_values)


base_df = base_df %>%
  mutate(best_values = paste0('\\makecell{', best_values, '}')) 


### 5.4 Resultados
resultados = map(list(best_values_cv_logit,best_values_cv_logit_en,best_values_cv_cart,
                      best_values_cv_rf,best_values_xgboost_weights,best_values_xgboost_no_weights),
                 .f = function(x){
                   
                   df = x %>% 
                     select(starts_with('F'),starts_with('Recall'),starts_with('Precision')) %>% 
                     select(-contains('SD')) 
                   
                   return(df)
                   
                 } )

resultados = bind_rows(
              resultados %>% 
              list_rbind()) %>% 
            mutate(across(everything(),.fns = function(x) ifelse(x == 0,' ',x)))

### 5.5 Tabla final
tabla = base_df %>% 
        bind_cols(resultados) %>% 
        mutate(grid_search = ifelse(model == 'Logit',' ',grid_search),
               best_values = ifelse(model == 'Logit',' ',best_values)) %>% 
        rename('Algoritmo'  = model,
               'Rangos de búsqueda' = grid_search,
               'Mejor modelo' = best_values,
               'F1' = 'F') %>% 
        select(-pesos)
        

empty_data <- tibble(Algoritmo = "",
                    `Rangos de búsqueda` = "",
                    `Mejor modelo` = "",
                     F1 = 0,
                     Recall = 0,
                     Precision = 0) 

tabla = bind_rows(tabla[1,],
                  empty_data,
                  tabla[2,],
                  empty_data,
                  tabla[3,],
                  empty_data,
                  tabla[4,],
                  empty_data,
                  tabla[5,],
                  empty_data,
                  tabla[6,]) %>% 
  mutate(across(.cols = c(F1,Recall,Precision),.fns = function(x) ifelse(x == 0,' ',x)))

tabla = tabla %>%
        kbl(format = "latex",
            booktabs = TRUE,
            escape = FALSE,                  # <- prevents LaTeX escaping
            caption = "Resultados de la validación cruzada en la selección de modelos") %>%
        kable_styling(latex_options = "hold_position") %>%
        column_spec(column = 2, width = "4cm") %>%
        column_spec(column = 3, width = "4cm") 

### 
writeLines(tabla, "stores/output/04_visual/02_validacion_cruzada.tex")


