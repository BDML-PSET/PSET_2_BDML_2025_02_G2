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

ctrl <- trainControl(method = "cv",  
                     number = 5,  
                     summaryFunction = multiStats,  
                     classProbs = TRUE,  
                     verbose = TRUE,  
                     savePredictions = TRUE)

set.seed(4326)
model = train(pobre ~ .,
              data = df_train,
              method = "rpart",
              metric = "F",
              trControl = ctrl,
              tuneLength = 50)

#-----------------------------#
# 4. Evaluación del modelo    #
#-----------------------------#
predictions_df_test = df_test %>% 
                      mutate(poor = predict(model,df_test)) %>% 
                      select(poor, pobre)

f_meas(data = predictions_df_test,truth = pobre,estimate = poor)

# Predicciones sobre test - kaggle
predictions_bayes = test %>% 
                    mutate(pobre = predict(model,test),
                           pobre = ifelse(pobre == 'Yes',1,0)) %>% 
                    select(id,pobre)

#-------------------#
# 5. Exportar datos #
#-------------------#
export(model, "stores/output/03_models/03_model_cart.rds")
export(predictions_bayes, "stores/output/03_models/03_cart_predictions.csv")