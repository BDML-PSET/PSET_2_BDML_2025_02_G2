library(dplyr)
library(yardstick)
library(knitr)
library(kableExtra)
library(tidyr)
library(data.table)
library(rio)
library(rsample)

#--------------#
# 1. Load data #
#--------------#
model <- import("stores/output/03_models/41_xgb_tree_weights_nrounds_500_max_depth_7_eta_0.04_gamma_0_col_sample_0.6_min_n_25_ss_0.8.rds")

train <- import("stores/output/02_wrangle/02_train.rds", setclass = "tibble")
test  <- import("stores/output/02_wrangle/02_test.rds", setclass = "tibble")

#---------------#
# 2. Split data #
#---------------#
set.seed(1234)
df_split <- initial_validation_split(data = train, prop = c(0.6, 0.2), strata = pobre)
df_train <- training(df_split)
df_validation <- validation(df_split)
df_test <- testing(df_split)

#------------------#
# 3. Predict probs #
#------------------#
predicted_value_validation <- predict(model, df_validation, type = "prob") %>%
  bind_cols(df_validation %>% select(pobre))

rm(df_split, df_train, df_test, df_validation, model, test, train)

#-------------#
# 4. F1 values #
#-------------#
f1_values <- lapply(seq(0, 1, 0.01), function(x) {
  predicted_tmp <- predicted_value_validation |>
    mutate(
      pred = ifelse(Yes >= x, "Yes", "No") |> factor(levels = c("Yes", "No")),
      pobre = factor(pobre, levels = c("Yes", "No"))
    )
  tibble(x = x, y = predicted_tmp |> yardstick::f_meas(pobre, pred) |> pull(.estimate))
}) |> data.table::rbindlist()

#-----------------------------#
# 5. Classification report (both classes)
#-----------------------------#
opt_thresh <- f1_values$x[which.max(f1_values$y)]

preds <- predicted_value_validation |>
  mutate(
    pred = ifelse(Yes >= opt_thresh, "Yes", "No") |> factor(levels = c("Yes", "No")),
    pobre = factor(pobre, levels = c("Yes", "No"))
  )

# --- métricas para clase "Yes" ---
yes_metrics <- tibble(
  Clase = "Pobre (Sí)",
  Precision = yardstick::precision_vec(preds$pobre, preds$pred, event_level = "first", na_rm = TRUE),
  Recall    = yardstick::recall_vec(preds$pobre, preds$pred, event_level = "first", na_rm = TRUE),
  F1        = yardstick::f_meas_vec(preds$pobre, preds$pred, event_level = "first", na_rm = TRUE),
  Observaciones = sum(preds$pobre == "Yes")
)

# --- métricas para clase "No" (invertir el evento positivo) ---
no_metrics <- tibble(
  Clase = "Pobre (No)",
  Precision = yardstick::precision_vec(preds$pobre, preds$pred, event_level = "second", na_rm = TRUE),
  Recall    = yardstick::recall_vec(preds$pobre, preds$pred, event_level = "second", na_rm = TRUE),
  F1        = yardstick::f_meas_vec(preds$pobre, preds$pred, event_level = "second", na_rm = TRUE),
  Observaciones = sum(preds$pobre == "No")
)

# --- accuracy global ---
accuracy_val <- yardstick::accuracy(preds, truth = pobre, estimate = pred) |> pull(.estimate)
acc_metrics <- tibble(
  Clase = "Accuracy",
  Precision = NA, Recall = NA, F1 = accuracy_val,
  Observaciones = nrow(preds)
)

# --- combinar ---
summary_table <- bind_rows(yes_metrics, no_metrics, acc_metrics)

#-----------------------------#
# 6. Export to LaTeX
#-----------------------------#
summary_table = summary_table |>
  mutate(across(where(is.numeric), ~ round(., 2))) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "lcccc",
    col.names = c("Clase", "Precisión", "Recall", "F1-score", "Observaciones")
  ) |>
  column_spec(1, bold = TRUE) |> 
  row_spec(0, bold = TRUE) |>   
  kable_styling(latex_options = "hold_position")


summary_table <- gsub("\\\\begin\\{table\\}.*?\\n", "", summary_table)
summary_table <- gsub("\\\\end\\{table\\}", "", summary_table)
#------------------#
# export
#------------------#
writeLines(text = summary_table, con = "stores/output/06_visual/04_reporte.txt")
