## =========== ##
## Yesenia F   ##
## =========== ##

### setup
cat("\f")
rm(list = ls())
source("scripts/00_packages.R")


library(tidyverse)
library(knitr)
library(kableExtra)

train_income = import("stores/output/03_models/00_rf_train.csv",setclass = 'tibble')

db = import("stores/output/02_wrangle/02_train.rds")

db = left_join(db,  train_income, by ="id")


#==============================#
# 1. Definir las variables de interés
#==============================#
vars_filtrar <- c('ing_unidad_gasto_imputado_per_capita_predicted',
  "cantidad_cuartos", "cantidad_cuartos_para_dormir",
  "tipo_propiedad_vivienda_hogar", "numero_personas_hogar", "arriendo",
  "urbano", "edad_household_average", "female_household_average",
  "informal_household_average", "nini_household_average",
  "regimen_salud_3_household_average", "maximo_nivel_educativo_1_household_average",
  "maximo_nivel_educativo_2_household_average", "maximo_nivel_educativo_3_household_average",
  "oc_household_average", "des_household_average",
  "edad_menor_18_household_average", "edad_menor_5_household_average",
  "edad_mayor_65_household_average", "ocupado_relab_4_household_working",
  "hh_female", "hh_regimen_salud_3", "hh_ocupado_relab_4",
  "hh_oc", "hh_des", "pobre"
)

db <- db[, vars_filtrar]

vars_continuas <- c(
  'ing_unidad_gasto_imputado_per_capita_predicted',
  "cantidad_cuartos", "cantidad_cuartos_para_dormir", "numero_personas_hogar", "arriendo",
  "edad_household_average", "female_household_average", "informal_household_average",
  "regimen_salud_3_household_average",
  "nini_household_average", "oc_household_average", "des_household_average",
  "edad_menor_18_household_average", "edad_menor_5_household_average",
  "edad_mayor_65_household_average", "maximo_nivel_educativo_1_household_average", "maximo_nivel_educativo_2_household_average",
  "maximo_nivel_educativo_3_household_average", "ocupado_relab_4_household_working"
)

vars_categoricas <- c(
  "tipo_propiedad_vivienda_hogar", "urbano"
)

dummies <- c("hh_oc", "hh_des", "hh_female",
             "hh_regimen_salud_3", "hh_ocupado_relab_4")


#==============================#
# 3. Funciones auxiliares
#==============================#

#--- Para variables continuas
desc_cont <- function(data, vars_cont, group_var = "pobre") {
  map_dfr(vars_cont, function(var) {
    # Calcular estadísticas descriptivas
    df <- data |>
      group_by(!!sym(group_var)) |>
      summarise(
        mean = mean(.data[[var]], na.rm = TRUE),
        sd = sd(.data[[var]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      pivot_longer(cols = c("mean", "sd"), names_to = "stat", values_to = "valor") |>
      pivot_wider(names_from = !!sym(group_var), values_from = valor) |>
      mutate(variable = var)
    
    # Realizar prueba t
    formula_str <- paste(var, "~", group_var)
    test_result <- t.test(as.formula(formula_str), data = data)
    
    # Añadir significancia
    df <- df |>
      mutate(
        p_valor = test_result$p.value,
        significancia = case_when(
          p_valor < 0.001 ~ "***",
          p_valor < 0.01 ~ "**",
          p_valor < 0.05 ~ "*",
          TRUE ~ ""
        )
      ) |>
      # Reordenar columnas
      select(variable, stat, Yes, No, significancia)
    
    return(df)
  })
}


#--- Para variables categoricas


analizar_categorica_str <- function(data, var_grupo_str, var_cat_str) {
  
  # 1. Calcular proporciones
  df_prop <- data |>
    group_by(across(all_of(c(var_grupo_str, var_cat_str)))) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(across(all_of(var_grupo_str))) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    pivot_wider(
      names_from = all_of(var_grupo_str),
      values_from = c(prop, n),
      names_sort = TRUE
    ) 
  
  # 2. Test de chi-cuadrado
  tabla <- table(data[[var_grupo_str]], data[[var_cat_str]])
  test_chi <- chisq.test(tabla)
  
  # 3. Extraer residuos
  residuos <- test_chi$residuals
  residuos_df <- as.data.frame.matrix(residuos)
  residuos_df$grupo <- rownames(residuos_df)
  
  # 4. Transformar residuos
  residuos_long <- residuos_df |>
    pivot_longer(
      cols = -grupo,
      names_to = "categoria",
      values_to = "residuo"
    ) |>
    mutate(
      estrellas = case_when(
        abs(residuo) >= 3.29 ~ "***",
        abs(residuo) >= 2.58 ~ "**",
        abs(residuo) >= 1.96 ~ "*",
        TRUE ~ ""
      ),
      residuo_txt = paste0(round(residuo, 2), estrellas)
    ) |>
    select(grupo, categoria, residuo_txt) |>
    pivot_wider(
      names_from = grupo,
      values_from = residuo_txt
    )
  
  # 5. Unir todo y crear la columna variable
  resultado <- df_prop |>
    rename(categoria = all_of(var_cat_str)) |>
    left_join(residuos_long, by = "categoria") |>
    select(categoria, starts_with("prop"), "Yes", "No") |>
    rename(
      Yes = prop_Yes,
      No = prop_No,
      Pobre = Yes,
      `No Pobre` = No
    ) |>
    mutate(
      variable = paste0(var_cat_str, "_", categoria),
      .before = 1
    ) |>
    select(-categoria)
  
  return(resultado)
}




#==============================#
# 4. Construcción general
#==============================#


crear_tabla_descriptiva_final <- function(data, vars_cont, vars_cat, vars_dummies, group_var = "pobre") {
  
  # 1. Variables continuas - formatear mean (sd)
  tabla_cont <- desc_cont(data, vars_cont, group_var) |>
    pivot_wider(
      names_from = stat,
      values_from = c(Yes, No, significancia)
    ) |>
    mutate(
      Variable = variable,
      Pobre = paste0(round(Yes_mean, 2), " (", round(Yes_sd, 2), ")"),
      `No Pobre` = paste0(round(No_mean, 2), " (", round(No_sd, 2), ")"),
      Diff = significancia_mean
    ) |>
    select(Variable, Pobre, `No Pobre`, Diff)
  
  # 2. Variables categóricas - proporciones y residuos CON SIGNO
  tabla_cat <- map_dfr(vars_cat, ~analizar_categorica_str(data, group_var, .x)) |>
    rename(residuos_pobre = Pobre) |>  # Guardar la columna de residuos antes de sobrescribirla
    mutate(
      Variable = variable,
      Pobre = paste0(round(Yes * 100, 1), "%"),
      `No Pobre` = paste0(round(No * 100, 1), "%"),
      Diff = residuos_pobre  # Usar la columna de residuos guardada
    ) |>
    select(Variable, Pobre, `No Pobre`, Diff)
  
  # 3. Variables dummy - proporciones y diferencias
  tabla_dummy <- cond_prob_global(data, vars_dummies, group_var) |>
    mutate(
      Variable = variable,
      Pobre = paste0(round(Yes * 100, 1), "%"),
      `No Pobre` = paste0(round(No * 100, 1), "%"),
      Diff = paste0(round(diff * 100, 1), "%")
    ) |>
    select(Variable, Pobre, `No Pobre`, Diff)
  
  # 4. Combinar todas las tablas
  tabla_final <- bind_rows(tabla_cont, tabla_cat, tabla_dummy)
  
  return(tabla_final)
}

#==============================#
# 5. Aplicar a tu base
#==============================#


tabla_completa <- crear_tabla_descriptiva_final(
  data = db,
  vars_cont = vars_continuas,
  vars_cat = vars_categoricas,
  vars_dummies = dummies,
  group_var = "pobre"
)


#==============================#
# 6. Mostrar tabla con formato
#==============================#

# Crear tabla con kable
table_latex <- tabla_completa |>
  select(-tipo) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c", "c", "c"),
    col.names = c("Variable", "Pobre", "No Pobre", "Diferencia"),
    caption = "Estadísticas Descriptivas por Condición de Pobreza",
    label = "descriptivas",
    escape = FALSE,
    linesep = ""
  ) |>
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 9,
    full_width = FALSE
  ) |>
  pack_rows("Variables Continuas", 1, idx_cont_end,
            latex_gap_space = "0.3em",
            bold = FALSE, italic = TRUE, hline_after = FALSE) |>
  pack_rows("Variables Categóricas", idx_cont_end + 1, idx_cat_end,
            latex_gap_space = "0.3em",
            bold = FALSE, italic = TRUE, hline_after = FALSE) |>
  pack_rows("Variables Dummy", idx_cat_end + 1, idx_dummy_end,
            latex_gap_space = "0.3em",
            bold = FALSE, italic = TRUE, hline_after = FALSE) |>
  footnote(
    general = "Variables continuas presentan Media (Desviación Estándar). Variables categóricas y dummy muestran porcentajes.",
    symbol = c("*** $p < 0.001$", "** $p < 0.01$", "* $p < 0.05$"),
    general_title = "Nota:",
    footnote_as_chunk = FALSE,
    escape = FALSE,
    threeparttable = TRUE
  )


# Guardar manualmente el resultado en un archivo .tex
writeLines(table_latex, "stores/output/04_visual/tabla_descriptiva.tex")
