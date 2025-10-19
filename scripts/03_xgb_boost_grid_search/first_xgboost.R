### setup
cat("\f")
rm(list = ls())
source('scripts/00_packages.R')

cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)
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

# ===============================================================#
# PREPARE MODEL DATA
# ===============================================================#

constant_vars <- names(which(sapply(df_train, function(x)
  (is.numeric(x) && var(x, na.rm = TRUE) == 0) ||
    (is.factor(x) && nlevels(x) < 2)
)))

df_train <- df_train %>% select(-all_of(constant_vars))

nzv <- nearZeroVar(df_train, saveMetrics = TRUE)

keep_vars <- c("dominio_barranquilla","dominio_bucaramanga","dominio_cali",
  "dominio_cartagena","dominio_cucuta","dominio_florencia",
  "dominio_ibague","dominio_manizales","dominio_medellin",
  "dominio_monteria","dominio_neiva","dominio_pasto",
  "dominio_pereira","dominio_popayan","dominio_quibdo",
  "dominio_resto_urbano","dominio_riohacha","dominio_rural",
  "dominio_santa_marta","dominio_sincelejo","dominio_tunja",
  "dominio_valledupar", "dominio_villavicencio"
)

nzv$nzv[rownames(nzv) %in% keep_vars] <- FALSE


df_train <- df_train[, !nzv$nzv]


vars_final <- colnames(df_train)
df_validation   <- df_validation %>% select(all_of(vars_final))
df_test  <- df_test %>% select(all_of(vars_final))

# ===============================================================#
#  CARET
# ===============================================================#

f1Summary <- function(data, lev = NULL, model = NULL) {
  f1 <- MLmetrics::F1_Score(y_pred = data$pred, y_true = data$obs, positive = "Yes")
  c(F1 = f1)
}


ctrl <- trainControl(
  method = "cv",               
  number = 5,                  
  classProbs = TRUE,           
  summaryFunction = f1Summary         
)


# ===============================================================#
#  GRID DE HIPERPARÁMETROS
# ===============================================================#
grid <- expand.grid(
  nrounds = 500,      # número de árboles
  max_depth = c(4, 6, 8),          # profundidad del árbol
  eta = c(0.05, 0.1, 0.2),         # learning rate
  gamma = 0,                       # penalización por complejidad
  colsample_bytree = c(0.7, 0.8, 1),
  min_child_weight = 10,            # mínimo peso de nodo hijo
  subsample = c(0.7, 0.8, 1)       # porcentaje de muestras por árbol
)


# ===============================================================#
#  ENTRENAR MODELO XGBOOST
# ===============================================================#
set.seed(123)
xgb_model <- train(
  pobre ~ ., 
  data = df_train,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = grid,
  metric = "F1",
  maximize = TRUE
)

stopCluster(cl)

# ===============================================================#
#  EVALUAR EN VALIDATION Y TEST
# ===============================================================#
pred_val <- predict(xgb_model, newdata = df_validation)
confusionMatrix(pred_val, df_validation$pobre, positive = "Yes")

cm_val <- confusionMatrix(pred_val, df_validation$pobre, positive = "Yes")

# Compute F1 manually
precision <- cm_val$byClass["Precision"]
recall    <- cm_val$byClass["Recall"]
F1_val    <- 2 * (precision * recall) / (precision + recall)

cat("\nF1 (Validation):", round(F1_val, 3), "\n")

pred_test <- predict(xgb_model, newdata = df_test)
confusionMatrix(pred_test, df_test$Pobre, positive = "Yes")
cm_test <- confusionMatrix(pred_test, df_test$pobre, positive = "Yes")

precision <- cm_test$byClass["Precision"]
recall    <- cm_test$byClass["Recall"]
F1_test   <- 2 * (precision * recall) / (precision + recall)

cat("\nF1 (Test):", round(F1_test, 3), "\n")
saveRDS(xgb_model, file = "../xgb_model_full.rds")

saveRDS(list(
  model = xgb_model,
  train_data = df_train,
  validation_data = df_validation,
  test_data = df_test,
  grid = grid,
  control = ctrl
), "../xgb_full_workspace.rds")


# ===============================================================#
#  CLEAN TEST DATA (same as training)
# ===============================================================#

df_1 <- df_test %>% select(-pobre)  
c <- colnames(df_1)
id_test <- test$id

test_x <- test %>% select(all_of(colnames(df_1)))


# ===============================================================#
# PREDICT AND EXPORT SUBMISSION FILE
# ===============================================================#

pred_probs <- predict(xgb_model, newdata = test_x, type = "prob")[, "Yes"]

submission_xgb <- data.frame(id = id_test,
                             Pobre = ifelse(pred_probs > 0.5, 1, 0))


write.csv(submission_xgb, "../Big_Data_ML/submission_xgbost_1.csv", row.names = FALSE)

# ===============================================================#
# COMPARISSON
# ===============================================================#
submission <- import("../submission.csv")

comparison <- merge(submission, submission_xgb, by = "id", suffixes = c("_glm", "_xgb"))
mean(comparison$pobre == comparison$Pobre)

table(comparison$pobre, comparison$Pobre)


