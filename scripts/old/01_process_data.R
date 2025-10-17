#----------------#
# Laura Ortiz    #
# 30 de sep 2025 # 
# ---------------#


# setup  #
rm(list = ls())
pacman::p_load(rio, rvest, tidyverse,janitor, data.table)

#=====================#
#  1. Import 
#=====================#
list_f <- list.files("../../BIG DATA Y ML/uniandes-bdml-2025-20-ps-2",
                     pattern = ".csv", 
                     full.names = TRUE, 
                     ignore.case = TRUE)
# names
file_names <- tools::file_path_sans_ext(basename(list_f))

#import
data <- setNames(lapply(list_f, read.csv), file_names)

# Export to environment
list2env(data, envir = .GlobalEnv)

#--------------------#
# 2.-- functions
#--------------------#
clean_housing <- function(df) {
  
  # helper para recodificar solo si existe la variable
  recode_if_exists <- function(df, var, levels, labels) {
    if (var %in% names(df)) {
      df[[var]] <- factor(df[[var]], levels = levels, labels = labels)
    }
    df
  }
  
  df <- df %>%
    rename(
      rooms_at_home          = any_of("P5000"),
      rooms_to_sleep         = any_of("P5010"),
      house_ownership        = any_of("P5090"),
      month_mortgage_payment = any_of("P5100"),
      imputed_monthly_rent   = any_of("P5130"),
      monthly_rent           = any_of("P5140"),
      linea_indigencia       = any_of("Li"),
      linea_pobreza          = any_of("Lp")
    )
  
  # aplicar recodificaciones seguras
  df <- recode_if_exists(df, "Pobre", levels = c(0,1), labels = c("No","Yes"))
  df <- recode_if_exists(df, "Indigente", levels = c(0,1), labels = c("No","Yes"))
  df <- recode_if_exists(
    df, "house_ownership",
    levels = c(1, 2, 3, 4, 5, 6),
    labels = c(
      "Owned, fully paid",
      "Owned, being paid",
      "Rented or subrented",
      "Usufruct",
      "Occupied without legal title",
      "Other"
    )
  )
  
}

clean_people <- function(df) {
  df <- df %>%
    rename(
      # --- Datos básicos ---
      sexo             = any_of("P6020"),
      edad             = any_of("P6040"),
      relacion_jefe    = any_of("P6050"),
      seguridad_social = any_of("P6090"),
      ss_regimen       = any_of("P6100"),
      max_educ_level   = any_of("P6210"),
      grado_aprobado   = any_of("P6210s1"),
      
      # --- Actividad y empleo ---
      actividad_principal = any_of("P6240"),
      antiguedad_empleo   = any_of("P6426"),
      posicion_empleo     = any_of("P6430"),
      tamano_empresa      = any_of("P6870"),
      cotiza_pension      = any_of("P6920"),
      
      # --- Ingresos laborales (dinero) ---
      ingreso_mensual_bruto = any_of("P6500"),
      horas_extras          = any_of("P6510"),
      ingreso_horas_extras  = any_of("P6510s1"),
      incluyo_h_extras      = any_of("P6510s2"),
      primas                = any_of("P6545"),
      ingreso_primas        = any_of("P6545s1"),
      incluyo_primas        = any_of("P6545s2"),
      bonificaciones        = any_of("P6580"),
      ingreso_bonuses       = any_of("P6580s1"),
      incluyo_bonuses       = any_of("P6580s2"),
      
      # --- Subsidios ---
      subsidio_alim          = any_of("P6585s1"),
      dinero_subs_alim       = any_of("P6585s1a1"),
      incluyo_subs_alim      = any_of("P6585s1a2"),
      subsidio_transporte    = any_of("P6585s2"),
      dinero_subs_transp     = any_of("P6585s2a1"),
      incluyo_subs_transp    = any_of("P6585s2a2"),
      subsidio_familiar      = any_of("P6585s3"),
      dinero_subs_familiar   = any_of("P6585s3a1"),
      incluyo_subs_familiar  = any_of("P6585s3a2"),
      subsidio_educ          = any_of("P6585s4"),
      dinero_subs_educ       = any_of("P6585s4a1"),
      incluyo_subs_educ      = any_of("P6585s4a2"),
      
      # --- Ingresos en especie ---
      food_as_payment           = any_of("P6590"),
      food_as_payment_amount    = any_of("P6590s1"),
      housing_as_payment        = any_of("P6600"),
      housing_as_payment_amount = any_of("P6600s1"),
      company_transport         = any_of("P6610"),
      company_transport_amount  = any_of("P6610s1"),
      otros_especie_recibio     = any_of("P6620"),
      otros_especie_monto       = any_of("P6620s1"),
      
      # --- Primas y bonificaciones ---
      prima_servicios_recibio   = any_of("P6630s1"),
      prima_servicios_monto     = any_of("P6630s1a1"),
      prima_navidad_recibio     = any_of("P6630s2"),
      prima_navidad_monto       = any_of("P6630s2a1"),
      prima_vacaciones_recibio  = any_of("P6630s3"),
      prima_vacaciones_monto    = any_of("P6630s3a1"),
      viaticos_perm_recibio     = any_of("P6630s4"),
      viaticos_perm_monto       = any_of("P6630s4a1"),
      bonus_anual_recibio       = any_of("P6630s6"),
      bonus_anual_monto         = any_of("P6630s6a1"),
      
      # --- Independientes ---
      ganancia_neta_mes      = any_of("P6750"),
      ganancia_meses_corresp = any_of("P6760"),
      ganancia_cosecha_12m   = any_of("P550"),  # solo rural
      
      # --- Horas trabajadas ---
      horas_semana_trab = any_of("P6800"),
      
      # --- Segundo trabajo ---
      segundo_trabajo         = any_of("P7040"),
      horas_segundo_trabajo   = any_of("P7045"),
      second_job_position     = any_of("P7050"),
      ingreso_segundo_trabajo = any_of("P7070"),
      
      # --- Subempleo y cambio ---
      quiere_mas_horas     = any_of("P7090"),
      searched_more_hours  = any_of("P7110"),
      disponible_mas_horas = any_of("P7120"),
      job_change_skills    = any_of("P7140s1"),
      job_change_income    = any_of("P7140s2"),
      searched_job_change  = any_of("P7150"),
      available_new_job    = any_of("P7160"),
      
      # --- Búsqueda de empleo / desocupados ---
      job_search_type               = any_of("P7310"),
      last_job_position             = any_of("P7350"),
      ingresos_desocupado           = any_of("P7422"),
      ingresos_desocupado_monto     = any_of("P7422s1"),
      ingresos_trabajo_mes_pasado   = any_of("P7472"),
      ingresos_trabajo_pasado_monto = any_of("P7472s1"),
      
      # --- Ingresos no laborales ---
      arriendos_pensiones        = any_of("P7495"),
      ingresos_arriendos         = any_of("P7500s1"),
      ingresos_arriendos_monto   = any_of("P7500s1a1"),
      ingresos_pension           = any_of("P7500s2"),
      ingresos_pension_monto     = any_of("P7500s2a1"),
      pension_alimenticia        = any_of("P7500s3"),
      pension_alimenticia_monto  = any_of("P7500s3a1"),
      otros_ingresos             = any_of("P7505"),
      transferencias_pais        = any_of("P7510s1"),
      transferencias_pais_monto  = any_of("P7510s1a1"),
      remesas_exterior           = any_of("P7510s2"),
      remesas_exterior_monto     = any_of("P7510s2a1"),
      ayudas_instituciones       = any_of("P7510s3"),
      ayudas_instituciones_monto = any_of("P7510s3a1"),
      investment_income          = any_of("P7510s5"),
      investment_income_amount   = any_of("P7510s5a1"),
      ingresos_cesantias         = any_of("P7510s6"),
      ingresos_cesantias_monto   = any_of("P7510s6a1"),
      otros_ingresos_fuentes     = any_of("P7510s7"),
      otros_ingresos_fuentes_monto = any_of("P7510s7a1"),
      
      # --- Ingresos antes de imputación ---
      ingreso_actividad1             = any_of("Impa"),
      ingreso_actividad2             = any_of("Isa"),
      ingreso_en_especie_imp         = any_of("Ie"),
      ingreso_desocupados_inactivos  = any_of("Imdi"),
      ingreso_intereses_dividendos   = any_of("Iof1"),
      ingreso_pensiones_imp          = any_of("Iof2"),
      ingreso_ayudas_hogares_imp     = any_of("Iof3h"),
      ingreso_ayudas_instituciones_imp = any_of("Iof3i"),
      ingreso_arriendos_imp          = any_of("Iof6")
    )

  # --- Safe factor conversion ---
  recode_if_exists <- function(df, var, levels, labels) {
    if (var %in% names(df)) {
      df[[var]] <- factor(df[[var]], levels = levels, labels = labels)
    }
    df
  }
  
  df <- df %>%
    recode_if_exists("sexo", c(1, 2), c("Hombre", "Mujer")) %>%
    recode_if_exists("relacion_jefe", 1:9, c("Jefe(a) del hogar","Cónyuge o pareja","Hijo(a), hijastro(a)","Nieto(a)",
                                             "Otro pariente","Empleado(a) del servicio","Pensionista","Trabajador","Otro no pariente")) %>%
    recode_if_exists("seguridad_social", c(1, 2, 9), c("Sí","No","No sabe")) %>%
    recode_if_exists("ss_regimen", c(1, 2, 3, 9), c("Contributivo","Especial","Subsidiado","No sabe")) %>%
    recode_if_exists("max_educ_level", c(1,2,3,4,5,6,9), c("Ninguno","Preescolar","Primaria","Secundaria","Media","Superior","No sabe")) %>%
    recode_if_exists("actividad_principal", 1:6, c("Trabajando","Buscando trabajo","Estudiando","Oficios del hogar","Incapacitado permanente","Otra actividad")) %>%
    recode_if_exists("posicion_empleo", 1:9, c("Obrero/empleado empresa particular","Obrero/empleado del gobierno","Empleado doméstico","Cuenta propia","Patrón o empleador","Familiar sin remuneración","Sin remuneración en negocio de otro hogar","Jornalero o peón","Otro")) %>%
    recode_if_exists("tamano_empresa", 1:9, c("Trabaja solo","2–3 personas","4–5 personas","6–10 personas","11–19 personas","20–30 personas","31–50 personas","51–100 personas","101 o más personas")) %>%
    recode_if_exists("cotiza_pension", c(1, 2, 3), c("Sí","No","Ya es pensionado")) %>%
    recode_if_exists("horas_extras", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("incluyo_h_extras", c(1,2), c("Sí","No")) %>%
    recode_if_exists("primas", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("incluyo_primas", c(1,2), c("Sí","No")) %>%
    recode_if_exists("bonificaciones", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("incluyo_bonuses", c(1,2), c("Sí","No")) %>%
    recode_if_exists("subsidio_alim", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("incluyo_subs_alim", c(1,2), c("Sí","No")) %>%
    recode_if_exists("subsidio_transporte", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("incluyo_subs_transp", c(1,2), c("Sí","No")) %>%
    recode_if_exists("subsidio_familiar", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("incluyo_subs_familiar", c(1,2), c("Sí","No")) %>%
    recode_if_exists("subsidio_educ", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("incluyo_subs_educ", c(1,2), c("Sí","No")) %>%
    recode_if_exists("food_as_payment", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("housing_as_payment", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("company_transport", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("otros_especie_recibio", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("prima_servicios_recibio", c(1,2), c("Sí","No")) %>%
    recode_if_exists("prima_navidad_recibio", c(1,2), c("Sí","No")) %>%
    recode_if_exists("prima_vacaciones_recibio", c(1,2), c("Sí","No")) %>%
    recode_if_exists("viaticos_perm_recibio", c(1,2), c("Sí","No")) %>%
    recode_if_exists("bonus_anual_recibio", c(1,2), c("Sí","No")) %>%
    recode_if_exists("segundo_trabajo", c(1,2), c("Sí","No")) %>%
    recode_if_exists("second_job_position", 1:9, c("Obrero/empleado empresa particular","Obrero/empleado del gobierno","Empleado doméstico","Cuenta propia","Patrón o empleador","Familiar sin remuneración","Sin remuneración en negocio de otro hogar","Jornalero o peón","Otro")) %>%
    recode_if_exists("quiere_mas_horas", c(1,2), c("Sí","No")) %>%
    recode_if_exists("searched_more_hours", c(1,2), c("Sí","No")) %>%
    recode_if_exists("disponible_mas_horas", c(1,2), c("Sí","No")) %>%
    recode_if_exists("job_change_skills", c(1,2), c("Sí","No")) %>%
    recode_if_exists("job_change_income", c(1,2), c("Sí","No")) %>%
    recode_if_exists("searched_job_change", c(1,2), c("Sí","No")) %>%
    recode_if_exists("available_new_job", c(1,2), c("Sí","No")) %>%
    recode_if_exists("job_search_type", c(1,2), c("Primera vez","Trabajó antes")) %>%
    recode_if_exists("last_job_position", 1:9, c("Obrero/empleado empresa particular","Obrero/empleado del gobierno","Empleado doméstico","Cuenta propia","Patrón o empleador","Familiar sin remuneración","Sin remuneración en negocio de otro hogar","Jornalero o peón","Otro")) %>%
    recode_if_exists("ingresos_desocupado", c(1,2), c("Sí","No")) %>%
    recode_if_exists("ingresos_trabajo_mes_pasado", c(1,2), c("Sí","No")) %>%
    recode_if_exists("arriendos_pensiones", c(1,2), c("Sí","No")) %>%
    recode_if_exists("ingresos_arriendos", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("ingresos_pension", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("pension_alimenticia", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("otros_ingresos", c(1,2), c("Sí","No")) %>%
    recode_if_exists("transferencias_pais", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("remesas_exterior", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("ayudas_instituciones", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("investment_income", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("ingresos_cesantias", c(1,2,9), c("Sí","No","No sabe")) %>%
    recode_if_exists("otros_ingresos_fuentes", c(1,2,9), c("Sí","No","No sabe"))
  
}


# export 

train_hogares <- clean_housing(train_hogares)
test_hogares <- clean_housing(test_hogares)

export(train_hogares,"stores/processed/train_hogares.rds")
export(test_hogares,"stores/processed/test_hogares.rds")

train_personas <- clean_people(train_personas)
test_personas <- clean_people(test_personas)

export(train_personas,"stores/processed/train_personas.rds")
export(test_personas,"stores/processed/test_personas.rds")


common_cols <- intersect(names(train_personas), names(test_personas))

export(common_cols, "stores/processed/common_cols.rds")




