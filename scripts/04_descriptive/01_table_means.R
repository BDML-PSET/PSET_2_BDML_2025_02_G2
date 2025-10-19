## =========== ##
## Yesenia F   ##
## =========== ##

source("scripts/00_packages.R")


library(tidyverse)
library(knitr)
library(kableExtra)

db = import("stores/output/02_wrangle/02_train.rds")

#==============================#
# 1. Definir las variables de interés
#==============================#
vars_filtrar <- c(
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



#==============================#
# 3. Funciones auxiliares
#==============================#

#--- Para variables continuas
desc_cont <- function(data, vars_cont, group_var = "pobre") {
  map_dfr(vars_cont, function(var) {
    df <- data |>
      group_by(!!sym(group_var)) |>
      summarise(
        mean = mean(.data[[var]], na.rm = TRUE),
        sd = sd(.data[[var]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      pivot_longer(cols = c("mean", "sd"), names_to = "stat", values_to = "valor") |>
      pivot_wider(names_from = !!sym(group_var), values_from = valor) |>
      mutate(variable = var, categoria = NA)
    
    return(df)
  })
}

#--- Para variables categóricas
desc_cat <- function(data, vars_cat, group_var = "pobre") {
  map_dfr(vars_cat, function(var) {
    df <- data |>
      group_by(!!sym(group_var), !!sym(var)) |>
      summarise(n = n(), .groups = "drop_last") |>
      mutate(prop = n / sum(n)) |>
      ungroup() |>
      mutate(variable = var) |>  # <- asigna el nombre como string
      rename(categoria = !!sym(var)) |>  # renombra la categoría para no interferir con `var`
      select(variable, categoria, !!sym(group_var), prop) |>
      pivot_wider(names_from = !!sym(group_var), values_from = prop)
    
    return(df)
  })
}

#==============================#
# 4. Construcción general
#==============================#

tabla_descriptiva <- function(data, vars_cont, vars_cat, group_var = "pobre") {
  tabla_cont <- if (length(vars_cont) > 0) desc_cont(data, vars_cont, group_var) else NULL
  tabla_cat  <- if (length(vars_cat) > 0)  desc_cat(data, vars_cat, group_var)  else NULL
  
  # Unir y ordenar
  final <- bind_rows(tabla_cont, tabla_cat) |>
    relocate(variable, categoria)
  
  return(final)
}

#==============================#
# 5. Aplicar a tu base
#==============================#
tabla_final <- tabla_descriptiva(
  data = db,
  vars_cont = vars_continuas,
  vars_cat = vars_categoricas,
  group_var = "pobre"
)


# 1. Calcular proporciones y N por grupo
dist_grupos <- db |>
  count(pobre) |>
  mutate(
    prop = round(n / sum(n) * 100, 1),
    etiqueta = paste0(pobre, ": ",
                      ifelse(pobre == "No", "No pobre", "Pobre"),
                      " (", prop, "%, N = ", format(n, big.mark = ","), ")")
  )

# 2. Extraer etiquetas para 0 y 1
label_0 <- dist_grupos$etiqueta[dist_grupos$pobre == "No"]
label_1 <- dist_grupos$etiqueta[dist_grupos$pobre == "Yes"]

encabezado_tabla <- setNames(c(2, 1, 1), c(" ", label_0, label_1))

#==============================#
# 6. Mostrar tabla con formato
#==============================#
tabla_final |>
  select(variable, categoria, `No`, `Yes`) |>
  mutate(across(where(is.numeric), ~round(., 3))) |>
  kable(
    caption = "Tabla descriptiva por condición de pobreza",
    col.names = c("Variable", "Categoría", "No pobre", "Pobre"),
    align = "llcc"
  ) |>
  add_header_above(encabezado_tabla) |>   # ✅ ya evaluado
  kable_styling(full_width = FALSE, position = "center") |>
  footnote(
    general = c(
      "Las variables continuas presentan la media o la desviación estándar (sd).",
      "Las variables categóricas muestran la proporción de observaciones dentro de cada grupo.",
      "Los grupos se definen según la variable `pobre` (0 = No pobre, 1 = Pobre)."
    ),
    general_title = "",
    escape = FALSE
  )
