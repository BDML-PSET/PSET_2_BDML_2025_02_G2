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


# Construir los ponderadores
pos_weight = sum(train$pobre == 'No') / sum(train$pobre == 'Yes')
wts = ifelse(train$pobre == 'Yes', round(pos_weight,0), 1) 

# Crear pesos
train$weights = frequency_weights(wts)

#---------------#
# 2. Split data #
#---------------#

set.seed(1234)
df_split = initial_validation_split(data = train,prop = c(0.6,0.2),strata = pobre)
df_train = training(df_split)
df_validation = validation(df_split)
df_test = testing(df_split)
set.seed(1234)
folds = vfold_cv(df_train,v = 5,strata = pobre)

#----------------#
# 3. Train model #
#----------------#

### Set recipe
rec = recipe(pobre ~ .,
             df_train) 

### Model with fixed high learning rate
spec = boost_tree(trees = tune(),
                  learn_rate = tune(),
                  mtry = tune(),
                  min_n = tune(),
                  tree_depth = tune(),
                  loss_reduction = tune(),
                  sample_size = tune()) %>%
       set_engine("lightgbm",
                   counts = FALSE,
                   num_leaves = tune()) %>%
       set_mode("classification")

### Set workflow
wk = workflow() %>% 
     add_case_weights(weights) %>% 
     add_recipe(rec) %>% 
     add_model(spec)

#-------------------------#
# 4. Tune hyperparameters #
# ------------------------#

### 4.1 Tune Eta
tune_rounds_eta = crossing(trees = seq(100,300,by = 100),
                           learn_rate = c(0.001,0.01,0.03,0.05),
                           tree_depth = c(4,6,8)) %>% 
                  mutate(mtry = 1,
                         min_n = 25,
                         loss_reduction = 0,
                         sample_size = 1,
                         num_leaves = 2^(tree_depth)-1)

set.seed(123)
tuned_grid_rounds_eta = tune_race_anova(wk,
                                        resamples = folds,
                                        grid = tune_rounds_eta,
                                        metrics = metric_set(f_meas,yardstick::recall,yardstick::precision),
                                        control = control_race(verbose = TRUE))

best_eta = tuned_grid_rounds_eta %>% select_best()
tuned_grid_rounds_eta %>% show_best()

### 4.1 Tune Max depth, min_n and num_leaves
tune_depth_min_n_num_leaves = crossing(trees = best_eta$trees,
                                       learn_rate = best_eta$learn_rate,
                                       tree_depth = c(4,6,8),
                                       min_n = c(10,20,30)) %>% 
                              mutate(mtry = 1,
                                     loss_reduction = 0,
                                     sample_size = 1,
                                     num_leaves = 2^(tree_depth)-1)

set.seed(123)
tuned_depth_min_n_num_leaves = tune_race_anova(wk,
                                               resamples = folds,
                                               grid = tune_depth_min_n_num_leaves,
                                               metrics = metric_set(f_meas,yardstick::recall,yardstick::precision),
                                               control = control_race(verbose = TRUE))

best_depth = tuned_depth_min_n_num_leaves %>% select_best()
tuned_depth_min_n_num_leaves %>% show_best()

### 4.2 Tune column and row sampling
tune_col_and_row_sampling = crossing(trees = best_eta$trees,
                                     learn_rate = best_eta$learn_rate,
                                     tree_depth = best_depth$tree_depth,
                                     min_n =  best_depth$min_n,
                                     num_leaves = best_depth$num_leaves,
                                     mtry = c(0.8,0.9),
                                     sample_size = c(0.8,0.9)) %>% 
                              mutate(loss_reduction = 0)

set.seed(123)
tuned_col_and_row_sampling = tune_race_anova(wk,
                                             resamples = folds,
                                             grid = tune_col_and_row_sampling,
                                             metrics = metric_set(f_meas,yardstick::recall,yardstick::precision),
                                             control = control_race(verbose = TRUE))

best_col_row_sampling = tuned_col_and_row_sampling %>% select_best()
tuned_col_and_row_sampling %>% show_best()

### 4.3 Tune gamma
tune_gamma = crossing(trees = best_eta$trees,
                      learn_rate = best_eta$learn_rate,
                      tree_depth = best_depth$tree_depth,
                      min_n =  best_depth$min_n,
                      num_leaves = best_depth$num_leaves,
                      mtry = best_col_row_sampling$mtry,
                      sample_size = best_col_row_sampling$sample_size,
                      loss_reduction = c(0,0.5,1))

set.seed(123)
tuned_gama = tune_race_anova(wk,
                             resamples = folds,
                             grid = tune_gamma,
                             metrics = metric_set(f_meas,yardstick::recall,yardstick::precision),
                             control = control_race(verbose = TRUE))

best_gamma = tuned_gama %>% select_best()
tuned_gama %>% show_best()

### 4.4 Retune learn rate and number of rounds
tune_rounds_eta_final = crossing(trees = seq(100,500,by = 50),
                                 learn_rate = c(0.001,0.003,0.01,0.03,0.05),
                                 tree_depth = best_depth$tree_depth,
                                 min_n =  best_depth$min_n,
                                 num_leaves = best_depth$num_leaves,
                                 mtry = best_col_row_sampling$mtry,
                                 sample_size = best_col_row_sampling$sample_size,
                                 loss_reduction = best_gamma$loss_reduction)

set.seed(123)
tuned_rounds_eta_final = tune_race_anova(wk,
                                         resamples = folds,
                                         grid = tune_rounds_eta_final,
                                         metrics = metric_set(f_meas,yardstick::recall,yardstick::precision),
                                         control = control_race(verbose = TRUE))

best_model = tuned_rounds_eta_final %>% select_best()
tuned_rounds_eta_final %>% show_best()

##==: 5. Final model predictions
set.seed(123)
model = fit(finalize_workflow(wk,best_model),df_train)

validation = augment(x = model,new_data = df_validation) %>% 
             dplyr::select(pobre,
                           poor = .pred_class,
                           yes = .pred_Yes,
                           no = .pred_No)

testing = augment(x = model,new_data = df_test) %>% 
          dplyr::select(pobre,
                        poor = .pred_class,
                        yes = .pred_Yes,
                        no = .pred_No)

### Optimize F1
roc_obj_rf =  roc(response = validation$pobre,  # Valores reales de la variable objetivo
                  predictor = validation$yes, # Probabilidades predichas por el modelo
                  levels = c("No", "Yes"),  # # Establece la referencia control y caso (empleado = negativo, desempleado = positivo) 
                  direction = "<") 

prec_recall = data.frame(coords(roc_obj_rf, seq(0,1,length=100), ret=c("threshold", "precision", "recall")))
prec_recall = prec_recall %>% mutate(F1=(2*precision*recall)/(precision+recall));prec_recall

# Encontrar el umbral óptimo que maximiza el F1-score
rfThreshF1 = prec_recall$threshold[which.max(prec_recall$F1)];rfThreshF1

### Improve f1
testing %>% 
  mutate(poor = ifelse(yes >= rfThreshF1,1,0),
         poor = factor(poor,levels = c('1','0'),labels = c('Yes','No'))) %>% 
  conf_mat(truth = pobre,estimate = poor) %>% 
  summary()

 final_predicitions = predict(model,test,type = 'prob') %>% 
                      bind_cols(test %>% select(id))%>% 
                      mutate(pobre = ifelse(.pred_Yes >= rfThreshF1,1,0)) %>% 
                      select(id,pobre)

predict(model,test) %>% 
  count(.pred_class) %>% 
  mutate(prop = n/sum(n))

final_predicitions$pobre %>% tabyl()