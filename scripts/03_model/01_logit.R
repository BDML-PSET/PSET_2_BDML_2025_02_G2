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

#---------------#
# 3. receta     #
#---------------#

# Definir control de entrenamiento (validación cruzada 10-fold)

multiStats <- function(...) c(twoClassSummary(...), defaultSummary(...), prSummary(...))

ctrl <- trainControl(
          method = "cv",  
          number = 5,  
          summaryFunction = multiStats,  
          classProbs = TRUE,  
          verbose = TRUE,  
          savePredictions = TRUE)

# Entrenar modelo logístico
logit_fit <- train(pobre ~ ., 
                   data = df_train,
                   method = "glm",
                   family = binomial,
                   preProcess = c("zv", "center", "scale"),
                   trControl = ctrl,
                   metric = "F")

#-----------------------------#
# 4. Evaluación del modelo    #
#-----------------------------#
predictions_df_test= df_test %>% 
                    mutate(poor = predict(logit_fit,df_test)) %>% 
                    select(poor, pobre)

f_meas(data = predictions_df_test,truth = pobre,estimate = poor)

# Predicciones sobre test
preds <- predict(logit_fit, newdata = df_test)
probs <- predict(logit_fit, newdata = df_test, type = "prob")

# Matriz de confusión
confusionMatrix(preds, df_test$pobre)

test1 <- test %>%
          mutate(
            .pred_class = predict(logit_fit, newdata = test),
            .pred_prob  = predict(logit_fit, newdata = test, type = "prob")[, 2])

# Seleccionar columnas para exportar
csv <- test1 %>%
       select(id, pobre = .pred_class)

#-----------------------------#
# 5. Exportar resultados      #
#-----------------------------#
csv <- df_results %>%
       select(id, pobre = pred_class)
