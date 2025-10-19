### setup
cat("\f")
rm(list = ls())
source("scripts/00_packages.R")

#--------------#
# 1. Load data #
#--------------#
train = import('stores/output/02_wrangle/03_train_ingreso.rds',setclass = 'tibble')
test = import('stores/output/02_wrangle/03_test_ingreso.rds',setclass = 'tibble')

#---------------#
# 2. Split data #
#---------------#

set.seed(1234)
df_split = initial_validation_split(data = train,prop = c(0.8,0.2),strata = pobre)
df_train = training(df_split)
df_test = testing(df_split)
rm(df_split)

#=====================#
# 03 modeling data
#=====================#

# recepie
rec = recipe(ing_unidad_gasto_imputado_per_capita ~ ., data = df_train |> select(-c(cod_dpto, pobre, linea_pobreza))) |>
      update_role(id, new_role = "id") |>
      step_novel() |>
      step_dummy(all_nominal_predictors()) |> 
      step_zv(all_predictors()) |> 
      step_normalize(all_numeric_predictors()) 

# train model
set.seed(123)
rf <- train(
        ing_unidad_gasto_imputado_per_capita ~ .,
        data = bake(prep(rec), new_data = df_train) |> clean_names() |> select(-c(id)),
        nthread = parallel::detectCores() - 1,
        method = "ranger",
        metric = 'RMSE',
        num.threads = (parallel::detectCores() - 1), 
        trControl = trainControl(method = 'oob',
                                 verboseIter = TRUE),
        tuneGrid = expand.grid(mtry = c(15),
                               min.node.size = c(5),
                               splitrule = c("variance")),
        num.trees = 1000,
        importance = 'impurity')  

#=====================#
# metricas 
#=====================#

#------------------------------#
cat("metricas in train", "\n\n")
df_train = df_train |> 
           mutate(ing_unidad_gasto_imputado_per_capita_predicted = predict(object = rf$finalModel, bake(prep(rec), new_data = df_train) |> clean_names() |> select(-c(id, ing_unidad_gasto_imputado_per_capita)))$predictions)

df_train |> rmse(ing_unidad_gasto_imputado_per_capita_predicted, ing_unidad_gasto_imputado_per_capita)

#------------------------------#
cat("metricas in test", "\n\n")
df_test = df_test |> 
           mutate(ing_unidad_gasto_imputado_per_capita_predicted = predict(object = rf$finalModel, bake(prep(rec), new_data = df_test) |> clean_names() |> select(-c(id, ing_unidad_gasto_imputado_per_capita)))$predictions)

df_test |> rmse(ing_unidad_gasto_imputado_per_capita_predicted, ing_unidad_gasto_imputado_per_capita)

df_test  |> 
    mutate(pobre_predicho = ifelse((ing_unidad_gasto_imputado_per_capita_predicted)<=linea_pobreza,
                                   yes = "Si", 
                                   no = "No") |>factor(levels = c("Si", "No")), 
           pobre = factor(pobre, levels = c("Si", "No"))) |> 
    f_meas(truth = pobre, pobre_predicho)

#=====================#
# final predicttions
#=====================#

# train
train_export = train |> 
               mutate(ing_unidad_gasto_imputado_per_capita_predicted = predict(object = rf$finalModel, bake(prep(rec), new_data = train) |> clean_names() |> select(-c(id, ing_unidad_gasto_imputado_per_capita)))$predictions)|> 
               select(id, ing_unidad_gasto_imputado_per_capita_predicted)

# test
test_export = test |> 
              mutate(ing_unidad_gasto_imputado_per_capita_predicted = predict(object = rf$finalModel, bake(prep(rec), new_data = test) |> clean_names() |> select(-c(id)))$predictions) |> 
              select(id, ing_unidad_gasto_imputado_per_capita_predicted)

#=====================#
# export
#=====================#
export(train_export, "stores/output/03_models/00_rf_train.csv")
export(test_export, "stores/output/03_models/00_rf_test.csv")
export(rf, "stores/output/03_models/00_rf_modelo.rds")