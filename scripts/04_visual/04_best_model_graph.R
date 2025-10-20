### setup
cat("\f")
rm(list = ls())
source("scripts/00_packages.R")

#--------------#
# 1. Load data #
#--------------#
model = import("stores/output/03_models/41_xgb_tree_weights_nrounds_500_max_depth_7_eta_0.04_gamma_0_col_sample_0.6_min_n_25_ss_0.8.rds") 

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

#------------------#
# 4. Adjust cutoff #
#------------------#
predicted_value_validation = predict(model,df_validation,type = 'prob') %>% 
                            bind_cols(df_validation %>% select(pobre))

rm(df_split, df_train, df_test, df_validation, model, test, train)

#-------------#
# roc curve

### Optimize SENS SPEC
rfROC = roc(predicted_value_validation$pobre, 
            predicted_value_validation$Yes, 
            levels = rev(levels(predicted_value_validation$pobre)))

roc_df <- data.frame(
  fpr = 1 - rfROC$specificities,
  tpr = rfROC$sensitivities
)

# Plot ROC curve with ggplot
roc_plot <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
            geom_area(aes(y = tpr), fill = "#1c61b6", alpha = 0.2) +  # shaded AUC area
            geom_line(color = "black") +
            geom_point(aes(x = (1 - coords(rfROC, x = "best", best.method = "closest.topleft")$specificity), 
                           y = coords(rfROC, x = "best", best.method = "closest.topleft")$sensitivity), 
                        color = "red", size = 3) +
            geom_abline(linetype = "dashed", color = "black") +
            annotate("text", 
                    x = 0.65, y = 0.2, 
                    label = paste("AUC =", round( auc(rfROC), 3)),
                    size = 5, color = "#1c61b6", fontface = "bold") +
            labs(title = "ROC Curve",
                 x = "False Positive Rate",
                 y = "True Positive Rate") +
            theme_classic()

#rm(rfROC, roc_df)

#-------------#
# f1 
f1_values <- lapply(seq(0, 1, 0.01), function(x) {
  predicted_tmp <- predicted_value_validation |>
                   mutate(pred = ifelse(Yes >= x, "Yes", "No") |> factor(levels = c("Yes", "No")), 
                          pobre = pobre |> factor(levels = c("Yes", "No")))
  
  return(tibble(x = x, 
         y =predicted_tmp  |> f_meas(pobre, pred)  |> pull(.estimate)))
}) |> rbindlist()

f1_plot = ggplot(f1_values, aes(x = x, y = y)) +
          geom_line(size = 1) +
          geom_vline(xintercept = f1_values$x[which.max(f1_values$y)],
                    color = "grey50",
                    linetype = "dashed") +
          geom_point(data = f1_values[which.max(f1_values$y), ],
                    aes(x = x, y = y),
                    color = "black",
                    size = 3) +
          geom_text(data = f1_values[which.max(f1_values$y), ],
                    aes(x = x, y = y, label = sprintf("F1 = %.3f", y)),
                    vjust = -1,
                    hjust = -0.3,
                    size = 3.5) +
          geom_text(data = f1_values[which.max(f1_values$y), ],
                  aes(x = x, y = 0, label = sprintf("Threshold = %.3f", x)),
                  hjust = -0.15,
                  size = 3.5)+
          labs(x = "Threshold",
              y = "",
              title = "F1 vs. Threshold") +
          theme_classic(base_size = 13)

rm(f1_values)


#-------------#
# f1 
pr_values <- lapply(seq(0, 1, 0.01), function(x) {
  predicted_tmp <- predicted_value_validation |>
                    mutate(pred = ifelse(Yes >= x, "Yes", "No") |> factor(levels = c("Yes", "No")),
                          pobre = factor(pobre, levels = c("Yes", "No")))
  
  tibble(x = x,
         Preccision = (precision_vec(predicted_tmp$pobre, predicted_tmp$pred)),
         Recall = (recall_vec(predicted_tmp$pobre, predicted_tmp$pred)), 
         f1 = predicted_tmp  |> f_meas(pobre, pred)  |> pull(.estimate))}) |> rbindlist()


pr_plot = pr_values |> 
          pivot_longer(cols = 2:3) |> 
          ggplot(aes(x = x, y = value, group = name, color = name)) +
          geom_line(size = 1) +
          geom_vline(xintercept = pr_values$x[which.max(pr_values$f1)],
                    color = "grey50",
                    linetype = "dashed" ) +
          geom_point(data = pr_values[which.max(pr_values$f1), ],
                  aes(x = x, y = f1),
                  color = "black",
                  size = 3,
                  inherit.aes = FALSE) +
          geom_text(data = pr_values[which.max(pr_values$f1), ],
                  aes(x = x, y = f1, label = sprintf("F1 = %.3f", f1)),
                  vjust = 0,
                  hjust = +1.1,
                  size = 3.5,
                  color = "black",
                  inherit.aes = FALSE) +
          geom_text(data = pr_values[which.max(pr_values$f1), ],
                  aes(x = x, y = 0.05, label = sprintf("Threshold = %.2f", x)),
                  vjust = -1,
                  hjust = -0.1,
                  size = 3.5,
                  color = "black",
                  inherit.aes = FALSE) +
          labs(x = "Threshold",
              y = "Metric",
              title = "",
              color = "") +
          theme_classic(base_size = 13); pr_plot

rm(pr_values)
#------------------#
# export
#------------------#
ggsave("stores/output/04_visual/04_pr_values.png", plot = pr_plot, width = 7, height = 5, dpi = 300)
ggsave("stores/output/04_visual/04_f1_values.png", plot = f1_plot, width = 7, height = 7, dpi = 300)
ggsave("stores/output/04_visual/04_roc_values.png", plot = roc_plot, width = 7, height = 5, dpi = 300)
