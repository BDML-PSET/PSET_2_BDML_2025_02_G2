#----------------#
# Laura Ortiz    #
# 30 de sep 2025 # 
# ---------------#


# setup  #
rm(list = ls())
pacman::p_load(rio, rvest, tidyverse,janitor, 
               data.table, caret, tidymodels, devtools)

# import data 

list_f <- list.files("stores/processed/",
                     pattern = ".rds", 
                     full.names = TRUE, 
                     ignore.case = TRUE)

# names
file_names <- tools::file_path_sans_ext(basename(list_f))

#import
data <- setNames(lapply(list_f, readRDS), file_names)

# Export to environment
list2env(data, envir = .GlobalEnv)

# common cols
train_personas <- train_personas %>%
  select(all_of(common_cols))

prueba <- left_join(train_personas, train_hogares %>% select(id, Pobre), by = "id")

# ====================================#
# ===🕵️️ 1. EXPLORATORY ANALYSIS  ====
# ====================================#

# ───────────────────────────────────────────────────────────────────#
# 1.1 Frequency tables by poverty status ----
# ───────────────────────────────────────────────────────────────────#

vars_summary <- c(
  "relacion_jefe", "max_educ_level", "grado_aprobado",
  "posicion_empleo", "horas_extras", "subsidio_alim",
  "subsidio_transporte", "subsidio_educ", "horas_semana_trab",
  "arriendos_pensiones", "otros_ingresos", "remesas_exterior"
)

tabyl_list <- lapply(vars_summary, function(var) {
  prueba %>%
    tabyl(!!sym(var), Pobre) %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns()
})

names(tabyl_list) <- vars_summary

for (i in seq_along(tabyl_list)) {
  cat("\n\n### Variable:", vars_summary[i], "\n")
  print(tabyl_list[[i]])
}

# ────────────────────────────────────────────────────────────#
# 1.2 Missingness by poverty group ----
# ────────────────────────────────────────────────────────────#



# Function to analyze missingness by poverty group

analyze_missing_by_group <- function(df, target = "Pobre", threshold = 10) {
  
  df %>%
    summarise(
      across(
        where(is.factor),
        ~ mean(is.na(.)) * 100,
        .names = "missing_{col}"
      ),
      .by = all_of(target)
    ) %>%
    # pivot longer for easier comparison
    tidyr::pivot_longer(
      cols = starts_with("missing_"),
      names_to = "variable",
      values_to = "missing_pct"
    ) %>%
    tidyr::pivot_wider(
      names_from = all_of(target),
      values_from = missing_pct,
      names_prefix = "missing_"
    ) %>%
    mutate(
      diff = abs(missing_No - missing_Yes),
      flag = case_when(
        diff >= threshold ~ "⚠️ possible signal",
        diff < threshold ~ "ok",
        TRUE ~ NA_character_
      )
    ) %>%
    arrange(desc(diff))
}

missing_summary <- analyze_missing_by_group(prueba)

# ────────────────────────────────────────────────────────────#
# 🧹 2. MISSING VALUES CLEANING ----
# ────────────────────────────────────────────────────────────#

# we know some missing values have value for knowing if poverty
# some of these are factor an numeric, dummy if missing
# and factor NA too

clean_missing <- function(df) {
  df <- df %>%
    mutate(
      # Replace NA in factors with "Missing"
      across(where(is.factor), ~{
        f <- as.character(.x)
        f[is.na(f)] <- "Missing"
        factor(f)
      })
    )
  
  # Create dummy "_missing" for numeric vars
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      df[[paste0(col, "_missing")]] <- ifelse(is.na(df[[col]]), 1, 0)
    }
  }
  
  df
}

prueba <- clean_missing(prueba)


# ===================================================================#
# 🏠 3. HOUSEHOLD-LEVEL FEATURE CREATION  ----
# ===================================================================#

df_h <- prueba %>%
  group_by(id) %>%
  summarise(
    # Household size
    n_personas = n(),
    
    # ── Demographics ────────────────────────────────────────────────
    prop_mujeres = mean(sexo == "Mujer", na.rm = TRUE),
    n_women = sum(sexo == "Mujer", na.rm = TRUE),
    n_men   = sum(sexo == "Hombre", na.rm = TRUE),
    jefe_mujer = as.integer(any(relacion_jefe == "Jefe(a) del hogar" & sexo == "Mujer")),
    
    n_hijo           = sum(relacion_jefe == "Hijo(a), hijastro(a)", na.rm = TRUE),
    n_nieto          = sum(relacion_jefe == "Nieto(a)", na.rm = TRUE),
    n_otro_pariente  = sum(relacion_jefe == "Otro pariente", na.rm = TRUE),
    n_empleado       = sum(relacion_jefe == "Empleado(a) del servicio", na.rm = TRUE),
    n_pensionista    = sum(relacion_jefe == "Pensionista", na.rm = TRUE),
    n_trabajador     = sum(relacion_jefe == "Trabajador", na.rm = TRUE),
    n_otro_no_pariente = sum(relacion_jefe == "Otro no pariente", na.rm = TRUE),
    
    # ── Education ──────────────────────────────────────────────────
    avg_educ_hogar   = mean(as.numeric(max_educ_level), na.rm = TRUE),
    max_educ_hogar   = max(as.numeric(max_educ_level), na.rm = TRUE),
    sum_educ_hogar   = sum(as.numeric(max_educ_level), na.rm = TRUE),
    grado_aprobado_NA = sum(grado_aprobado_missing, na.rm = TRUE),
    adult_with_degree_r = sum(edad > 18 & max_educ_level == "Superior") / n(),
    educ_NA          = sum(max_educ_level == "Missing", na.rm = TRUE),
    
    # ── Occupation and employment ──────────────────────────────────
    oficio_NA        = sum(is.na(Oficio)),
    rate_person_Oficios     = sum(edad > 18 & is.na(Oficio)) / sum(edad > 18),
    rate_person_Oficios_hh  = sum(edad > 18 & is.na(Oficio)) / n_personas,
    
    n_empresa        = sum(posicion_empleo == "Obrero/empleado empresa particular", na.rm = TRUE),
    n_gob            = sum(posicion_empleo == "Obrero/empleado del gobierno", na.rm = TRUE),
    n_edomes         = sum(posicion_empleo == "Empleado doméstico", na.rm = TRUE),
    n_cuentapropia   = sum(posicion_empleo == "Cuenta propia", na.rm = TRUE),
    n_patron         = sum(posicion_empleo == "Patrón o empleador", na.rm = TRUE),
    n_sinrem         = sum(posicion_empleo == "Familiar sin remuneración", na.rm = TRUE),
    n_sin_rem_other_h = sum(posicion_empleo == "Sin remuneración en negocio de otro hogar", na.rm = TRUE),
    n_jornalero      = sum(posicion_empleo == "Jornalero o peón", na.rm = TRUE),
    n_otro           = sum(posicion_empleo == "Otro", na.rm = TRUE),
    n_nothing_emp    = sum(posicion_empleo == "Missing", na.rm = TRUE),
    
    #Oficios y empleos
    oficio_59 = sum(if_else(Oficio == 59,1,0), na.rm = TRUE),
    oficio_79 = sum(if_else(Oficio == 79,1,0), na.rm = TRUE),
    oficio_21 = sum(if_else(Oficio == 21,1,0), na.rm = TRUE),
    oficio_33 = sum(if_else(Oficio == 33,1,0), na.rm = TRUE),
    oficio_97 = sum(if_else(Oficio == 97,1,0), na.rm = TRUE),
    oficio_58 = sum(if_else(Oficio == 58,1,0), na.rm = TRUE),
    oficio_13 = sum(if_else(Oficio == 13,1,0), na.rm = TRUE),
    rate_person_Oficios = sum(edad > 18 & is.na(Oficio)) / sum(edad > 18),
    rate_person_Oficios_hh = sum(edad > 18 & is.na(Oficio)) / n_personas,
    
    # ── Labor and income ────────────────────────────────────────────
    horas_trabajas_personas = sum(horas_semana_trab, na.rm = TRUE),
    horas_trabajas_personas_r = horas_trabajas_personas / n_personas,
    up_40_wh = as.integer(horas_semana_trab > 40),
    
    depend_ratio = sum(edad < 18 | edad > 65) / n_personas,
    
    Pet_missing = any(Pet_missing),
    Oc_missing  = any(Oc_missing),
    Ina_missing = any(Ina_missing)
  )

# ==================================================================#
# 🧱 4. ONE-HOT ENCODING & AGGREGATION ----
# ==================================================================#

factor_vars <- c(
  "relacion_jefe", "seguridad_social", "ss_regimen",
  "max_educ_level", "actividad_principal", "posicion_empleo", "horas_extras"
)

df_encoded <- fastDummies::dummy_cols(
  prueba,
  select_columns = factor_vars,
  remove_selected_columns = TRUE,
  remove_first_dummy = FALSE
)

dummy_prefixes <- paste0(factor_vars, "_")

df_household <- df_encoded %>%
  group_by(id) %>%
  summarise(
    n_personas = n(),
    # counts: how many people in the HH have each category
    across(starts_with(dummy_prefixes),
           ~sum(.x == 1, na.rm = TRUE),
           .names = "count_{.col}"),
    # presence: at least one person
    across(starts_with(dummy_prefixes),
           ~as.integer(sum(.x == 1, na.rm = TRUE) > 0),
           .names = "has_{.col}")
  )


