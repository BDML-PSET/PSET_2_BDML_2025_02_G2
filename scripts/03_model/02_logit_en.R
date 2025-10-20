### setup
cat("\f")
rm(list = ls())
source('scripts/00_packages.R')

#--------------#
# 1. Load data #
#--------------#

train = import('stores/output/02_wrangle/02_train.rds',setclass = 'tibble') %>% select(-id)
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
set.seed(2025)
logit_fit <- train(pobre ~ ., 
                   data = df_train,
                   method = "glmnet",
                   preProcess = c("zv", "center", "scale"),
                   trControl = ctrl,
                   metric = "F", 
                   tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.5),          # mezcla entre Ridge (0) y Lasso (1)
                                          lambda = 10^seq(-3, 3, length = 20)))

#-----------------------------#
# 4. Evaluación del modelo    #
#-----------------------------#
predictions_df_test = df_test %>% 
                      mutate(poor = predict(logit_fit,df_test)) %>% 
                      select(poor, pobre)

f_meas(data = predictions_df_test,truth = pobre,estimate = poor)

# Predicciones sobre test - kaggle
predictions_bayes = test %>% 
                    mutate(pobre = predict(logit_fit,test),
                           pobre = ifelse(pobre == 'Yes',1,0)) %>% 
                    select(id,pobre)

#------------------------#
#      exportar datos    #
#------------------------#
export(logit_fit, "stores/output/03_models/02_model_logit_EN.rds")
export(predictions_bayes, "stores/output/03_models/02_model_logit_EN_predictions.csv")
