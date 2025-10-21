
rm(list = ls())
source("scripts/00_packages.R")

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
rm(df_split, df_train, df_validation, test, train)

# predictions
df_test = df_test |> 
          mutate(pred_pobre = predict(model, df_test, type = "prob")$Yes) 
         
df_test = df_test |> 
          mutate(pred_pobre = ifelse(test = pred_pobre>= 0.33, yes = "Sí", no = "No"), 
                 pobre = ifelse(test = pobre == "Yes", yes = "Sí", no = "No"))

# selecion de variables
df_test = df_test |> 
          select(oc_household_average, 
                regimen_salud_1_household_average, 
                numero_personas_unidad_gasto, 
                hh_ocupado_horas_trabajadas_normalmente,
                lp,
                ocupado_tiempo_en_la_empresa_household_working, 
                numero_personas_hogar, 
                tipo_propiedad_vivienda_hogar_3, 
                ocupado_tiempo_en_la_empresa_household_working, 
                ocupado_horas_trabajadas_normalmente_household_working, 
                pred_pobre, 
                pobre)
        
##==: 3. outliers
df_test = df_test |> 
          filter(pobre == "Sí" | pred_pobre == "Sí") |> 
          ungroup()

df_test = df_test |> 
          mutate(outliers = fcase(pobre == "No" & pred_pobre == "Sí", "under", # -> FP
                                  pobre == "Sí" & pred_pobre == "No", "over",  # -> FN
                                  default = "No"))

df_test$outliers = factor(df_test$outliers, levels = c("No", "over", "under"))

test = df_test
##==: 4. table de diferencia de medias para continuas

## funcion para sacar tabla de diferencia de medias en valores continuos
c_test = function(data, interes_col, outlier_col){
  
        # test for over and under
        data = data |> rename(interes_col = interes_col, outlier_col = outlier_col)
        
        over = t.test(formula = interes_col ~ outlier_col, data = data |> filter(outlier_col %in% c("over","No")), alternative = "two.sided")         
        under = t.test(formula = interes_col ~ outlier_col, data = data |> filter(outlier_col %in% c("under","No")), alternative = "two.sided")         

        # statistics
        base_mean = over$estimate["mean in group No"] |> as.numeric()
        base_sd = sd(data$interes_col[data$outlier_col == "No"])
     
        over_mean = over$estimate["mean in group over"]  |> as.numeric()
        over_sd = sd(data$interes_col[data$outlier_col == "over"])

        under_mean = under$estimate["mean in group under"] |> as.numeric()
        under_sd = sd(data$interes_col[data$outlier_col == "under"])
  
        diff_1 = over_mean - base_mean
        sd_diff_1 = over$stderr
  
        diff_2 = under_mean - base_mean
        sd_diff_2 = under$stderr
  
        # estrellitas
        f_star = function(x){case_when(x<0.01 ~ "***",  x<0.05 ~ "**", x<0.1 ~ "*", .default = "")}
        fmt <- function(x) formatC(x, format = "f", digits = 2)
  
        # Tabla
        output = tibble(var = c(interes_col, NA ),
                        base_mean = c(fmt(base_mean),  paste0("(",fmt(base_sd),")")), 
                        over_mean =  c(fmt(over_mean),  paste0("(",fmt(over_sd),")")), 
                        under_mean =  c(fmt(under_mean),  paste0("(",fmt(under_sd),")")), 
                        diff_1 = c(paste0(fmt(diff_1), f_star(over$p.value)), paste0("(",fmt(sd_diff_1),")")), 
                        diff_2 = c(paste0(fmt(diff_2), f_star(under$p.value)), paste0("(",fmt(sd_diff_2),")")))
        
  return(output) 

}

continuas = bind_rows(c_test(data = test, interes_col = "oc_household_average", outlier_col = "outliers"), 
                      c_test(data = test, interes_col = "regimen_salud_1_household_average", outlier_col = "outliers"),
                      c_test(data = test, interes_col = "numero_personas_unidad_gasto", outlier_col = "outliers"), 
                      c_test(data = test, interes_col = "hh_ocupado_horas_trabajadas_normalmente", outlier_col = "outliers"), 
                      c_test(data = test, interes_col = "lp", outlier_col = "outliers"), 
                      c_test(data = test, interes_col = "ocupado_tiempo_en_la_empresa_household_working", outlier_col = "outliers"), 
                      c_test(data = test, interes_col = "numero_personas_hogar", outlier_col = "outliers"), 
                      c_test(data = test, interes_col = "ocupado_horas_trabajadas_normalmente_household_working", outlier_col = "outliers")) 

continuas$var = c("Número de ocupados promedio", NA, 
                  "Número de personas afiliadas al régimen contributivo", NA, 
                  "Número de personas por unidad de gasto", NA,
                  "Horas promedio trabajadas por ocupados", NA,
                  "Línea de pobreza", NA,
                  "Tiempo promedio de ocupación en la empresa", NA,
                  "Número de personas por hogar", NA,
                  "Horas promedio trabajadas por ocupados", NA)


##==: 4. table de diferencia de medias para discretas

## funcion para sacar tabla de diferencia de medias en valores discretos
p_test = function(interes_col, outlier_col){
  
        # Agrupaciones
        tab <- table(interes_col, outlier_col)

        # calcular columnas de si y totales
        yes <- tab[2, ]     
        n <- colSums(tab)

        # Calculando differencia de medias
        p_test_1 = prop.test(x = yes[-3], n = n[-3], alternative = "two.sided")
        p_test_2 = prop.test(x = yes[-2], n = n[-2], alternative = "two.sided")
        
        # Calculando standar deviation
        se_base = sqrt(p_test_1$estimate["prop 1"] * (1 - p_test_1$estimate["prop 1"]) / sum(tab[,1]))
        se_over = sqrt(p_test_1$estimate["prop 2"] * (1 - p_test_1$estimate["prop 2"]) / sum(tab[,2]))
        se_under = sqrt(p_test_2$estimate["prop 2"] * (1 - p_test_2$estimate["prop 2"]) / sum(tab[,3]))
        se_diff_1 = sqrt((p_test_1$estimate["prop 2"] * (1 - p_test_1$estimate["prop 2"]) / sum(tab[,2])) + (p_test_1$estimate["prop 1"] * (1 - p_test_1$estimate["prop 1"]) / sum(tab[,1])))
        se_diff_2 = sqrt((p_test_2$estimate["prop 2"] * (1 - p_test_2$estimate["prop 2"]) / sum(tab[,2])) + (p_test_2$estimate["prop 1"] * (1 - p_test_2$estimate["prop 1"]) / sum(tab[,1])))
  
        # estrellitas
        f_star = function(x){case_when(x<0.01 ~ "***",  x<0.05 ~ "**", x<0.1 ~ "*", .default = "")}
        fmt <- function(x) formatC(x, format = "f", digits = 2)
  
        # Tabla
        output = tibble(var = c(row.names(tab)[2], NA ),
                      base_prop = c(fmt(p_test_1$estimate["prop 1"]),  paste0("(",fmt(se_base),")")), 
                      over_prop =  c(fmt(p_test_1$estimate["prop 2"]),  paste0("(",fmt(se_over),")")), 
                      under_prop =  c(fmt(p_test_2$estimate["prop 2"]),  paste0("(",fmt(se_under),")")),
                      diff_1 = c(paste0(fmt(p_test_1$estimate["prop 2"] - p_test_1$estimate["prop 1"]), f_star(p_test_1$p.value)), paste0("(",fmt(se_diff_1),")")),
                      diff_2 = c(paste0(fmt(p_test_2$estimate["prop 2"] - p_test_2$estimate["prop 1"]), f_star(p_test_2$p.value)), paste0("(",fmt(se_diff_2),")")))
  
        return(output) 
  
}

## aplicar a variables discretas
discretas <- lapply(c("tipo_propiedad_vivienda_hogar_3") , function(var){

        cats <- as.character(unique(test[[var]]))
        #print(var)
        var_2 = var

        output =  bind_rows(lapply(cats, function(x){
                #cat(x, "\n")
                df = test %>%
                        mutate(temp_var = ifelse(.data[[var]] == x, x, "otro"),
                                temp_var = factor(temp_var, levels = c("otro", x)))

                df = p_test(df$temp_var, df$outliers)
                
                return(df) 
        })) 
  
        output = output |>
                mutate(category = var_2) |> 
                relocate(category, .before  = var)
        #cat("\n")
        return(output)
}) 


##==: 5. exportar resultados
options(knitr.kable.NA = '')
  
# continuas
continuas_table <- continuas |>
  kable(
    col.names = c(
      "",
      "TP",
      "FN",
      "FP",
      "FN - TP",
      "FP - TP"
    ),
    format = "latex",
    align = c("lccccc"),
    booktabs = TRUE
  ) |>
  add_header_above(c(" " = 1, "Media" = 3, "Diferencias" = 2)) |>
  kable_styling(latex_options = "hold_position") |>
  footnote(
    general = c(
      "Desviaciones estándar entre paréntesis.",
      "* p$<$0.10; ** p$<$0.05; *** p$<$0.01",
      paste0("Observaciones (TP, FN, FP): ",
             sum(test$outliers == "No"), ", ",
             sum(test$outliers == "over"), ", ",
             sum(test$outliers == "under")),
      "TP: pobres correctamente clasificados", 
      "FN: pobres clasificados como no pobres",
      "FP: no pobres clasificados como pobres."
    ),
    general_title = "", 
    escape = FALSE
  )


continuas_table <- gsub("\\\\begin\\{table\\}.*?\\n", "", continuas_table)
continuas_table <- gsub("\\\\end\\{table\\}", "", continuas_table)
writeLines(text = continuas_table, con = "stores/output/04_visual/08_diferencia_medias_continuas.tex")
