### setup
cat("\f")
rm(list = ls())
source("scripts/00_packages.R")
pacman::p_load(kableExtra, xtable)
options(knitr.kable.NA = '')

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
predicted_value_validation = predict(model,df_test,type = 'prob') %>% 
                            bind_cols(df_test %>% select(pobre))

rm(df_split, df_train, model, test, train)


#-------------#
# f1 

predicho = predicted_value_validation |> 
            mutate(pred = ifelse(Yes >= 0.33, "Yes", "No") |> factor(levels = c("Yes", "No")), 
                   pobre = pobre |> factor(levels = c("Yes", "No"))) 

conf_mat= predicho |>            
          count(pred, pobre) |>
          group_by(pobre) |> 
          mutate(n = paste0(round(n/sum(n)*100, 2), " %")) |> 
          pivot_wider(names_from = pobre, values_from = n) |>
          rename(Predicted = pred,
                  True_Yes = Yes,
                  True_No = No)  |> 
          mutate(Predicted  = c("Sí", "No"), 
                  v = c("Predicho", NA)) |>
          relocate(v, .before =Predicted )

conf_mat = conf_mat |> 
           kable(align = c("llcc"),
                 booktabs = TRUE,
                 col.names = c("", "", "Sí", "No"), 
                 format = "latex") |> 
           add_header_above(c(" " = 2, "Verdadero" = 2), bold = TRUE) |> 
           column_spec(1:2, bold = TRUE) |> 
           row_spec(0, bold = TRUE) |>   
           kable_styling(latex_options = "hold_position") |>
           footnote(general = c( "Valores estandarizados por columna",   
                                paste0("Observaciones (Sí, No): ",
                                       sum(predicho$pobre == "Yes"), ", ",
                                       sum(predicho$pobre == "No"))), 
                    general_title = "", 
                    escape = FALSE)

conf_mat <- gsub("\\\\begin\\{table\\}.*?\\n", "", conf_mat)
conf_mat <- gsub("\\\\end\\{table\\}", "", conf_mat)
#------------------#
# export
#------------------#
writeLines(text = conf_mat, con = "stores/output/04_visual/05_tabla_confuson_estandarizada.txt")
