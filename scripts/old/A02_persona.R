#--------------------#
# Laura Ortiz & Alejandro Cardiles
#  
# -------------------#

# setup  #
rm(list = ls())
pacman::p_load(rio, tidyverse, janitor, data.table, jsonlite, httr)
options(scipen = 999)

#====================#
# 1. Load data ----
#====================#

# funcion para importar datos de kaggle
kaggle_import = function(user_json, file_name){

  # credentials
  kaggle <- fromJSON(user_json)
  auth <- authenticate(kaggle$username, kaggle$key, type = "basic")
  
  # download file
  url <- paste0("https://www.kaggle.com/api/v1/competitions/data/download/uniandes-bdml-2025-20-ps-2/", file_name)
  res <- GET(url, auth)
  
  # read file
  data <- read.csv(text = content(res, "text", encoding = "UTF-8"))
  return(data)
}

test_personas = kaggle_import("kaggle.json", "test_personas.csv") |> clean_names() 
train_personas = kaggle_import("kaggle.json", "train_personas.csv") |> clean_names()

rm(kaggle_import)


#===========================#
# 2. clean data personas ----
#===========================#

#---------------------------#
# 2.1 train_personas ----

#---- Cambiando nombres
train_personas = train_personas %>%
                 rename(# --- Datos básicos ---
                     sexo             = any_of("p6020"),
                     edad             = any_of("p6040"),
                     relacion_jefe    = any_of("p6050"),
                     seguridad_social = any_of("p6090"),
                     ss_regimen       = any_of("p6100"),
                     max_educ_level   = any_of("p6210"),
                     grado_aprobado   = any_of("p6210s1"),
                     
                     #-------------------------#
                     #-------------------------#
                     # Ocupado

                     # --- Actividad y empleo ---
                     actividad_principal = any_of("p6240"),
                     antiguedad_empleo   = any_of("p6426"),
                     posicion_empleo     = any_of("p6430"),
                     tamano_empresa      = any_of("p6870"),
                     cotiza_pension      = any_of("p6920"),
                     
                     # --- Ingresos laborales (dinero) ---
                     ingreso_ocupado_monto       = any_of("p6500"),
                     recibio_horas_extras        = any_of("p6510"),
                     ingreso_horas_extras_monto  = any_of("p6510s1"),
                     incluyo_horas_extras        = any_of("p6510s2"),
                     recibio_primas              = any_of("p6545"),
                     ingreso_primas_monto        = any_of("p6545s1"),
                     incluyo_primas              = any_of("p6545s2"),
                     recibio_bonificaciones      = any_of("p6580"),
                     ingreso_bonificaciones_monto= any_of("p6580s1"),
                     incluyo_bonificaciones      = any_of("p6580s2"),
                     
                     # --- Subsidios ---
                     recibio_subsidio_alim          = any_of("p6585s1"),
                     subsidio_alim_monto            = any_of("p6585s1a1"),
                     incluyo_subsidio_alim          = any_of("p6585s1a2"),
                     recibio_subsidio_transporte    = any_of("p6585s2"),
                     subsidio_transporte_monto      = any_of("p6585s2a1"),
                     incluyo_subsidio_transporte    = any_of("p6585s2a2"),
                     recibio_subsidio_familiar      = any_of("p6585s3"),
                     subsidio_familiar_monto        = any_of("p6585s3a1"),
                     incluyo_subsidio_familiar      = any_of("p6585s3a2"),
                     recibio_subsidio_educ          = any_of("p6585s4"),
                     subsidio_educ_monto            = any_of("p6585s4a1"),
                     incluyo_subsidio_educ          = any_of("p6585s4a2"),
                     
                     # --- Ingresos en especie ---
                     recibio_comida_como_pago          = any_of("p6590"),
                     comida_como_pago_monto            = any_of("p6590s1"),
                     recibio_vivenda_como_pago         = any_of("p6600"),
                     vivenda_como_pago_monto           = any_of("p6600s1"),
                     recibio_transporte_laboral_empresa= any_of("p6610"),
                     transporte_laboral_empresa_monto  = any_of("p6610s1"),
                     recibio_otros_especie             = any_of("p6620"),
                     otros_especie_monto               = any_of("p6620s1"),
                     
                     # --- Primas y bonificaciones ---
                     recibio_prima_servicios   = any_of("p6630s1"),
                     prima_servicios_monto     = any_of("p6630s1a1"),
                     recibio_prima_navidad     = any_of("p6630s2"),
                     prima_navidad_monto       = any_of("p6630s2a1"),
                     recibio_prima_vacaciones  = any_of("p6630s3"),
                     prima_vacaciones_monto    = any_of("p6630s3a1"),
                     recibio_viaticos_perm     = any_of("p6630s4"),
                     viaticos_perm_monto       = any_of("p6630s4a1"),
                     recibio_bonus_anual       = any_of("p6630s6"),
                     bonus_anual_monto         = any_of("p6630s6a1"),
                     
                     # --- Independientes ---
                     ingreso_independientes_monto      = any_of("p6750"),
                     ingresos_independientes_meses     = any_of("p6760"),
                     ganancia_cosecha_12m              = any_of("p550"),  # solo rural
                     horas_semana_trabajadas           = any_of("p6800"),
                     
                     # --- Segundo trabajo ---
                     segundo_trabajo                       = any_of("p7040"),
                     horas_segundo_trabajo                 = any_of("p7045"),
                     posicion_empleo_segundo_trabajo       = any_of("p7050"),
                     ingreso_ocupado_monto_segundo_trabajo = any_of("p7070"),
                     
                     # --- Subempleo y cambio ---
                     quiere_mas_horas           = any_of("p7090"),
                     busca_mas_horas            = any_of("p7110"),
                     disponible_mas_horas       = any_of("p7120"),
                     motivo_cambio_capacidades  = any_of("p7140s1"),
                     motivo_cambio_ingreso      = any_of("p7140s2"),
                     busca_trabajo              = any_of("p7150"),
                     disponible_trabajar        = any_of("p7160"),
                     
                     #-------------------------#
                     #-------------------------#
                     # Desocupado 

                     # --- Búsqueda de empleo / desocupados ---
                     experiencia_laboral_previa    = any_of("p7310"),
                     ocupacion_previa              = any_of("p7350"),
                     recibio_ingresos_desocupado   = any_of("p7422"),
                     ingresos_desocupado_monto     = any_of("p7422s1"),
                     recibio_ingresos_inactivo     = any_of("p7472"),
                     ingresos_inactivo_monto       = any_of("p7472s1"),
                     
                     # --- Ingresos no laborales ---
                     recibio_arriendos_o_pensiones = any_of("p7495"),
                     recibio_ingresos_arriendos    = any_of("p7500s1"),
                     ingresos_arriendos_monto      = any_of("p7500s1a1"),
                     recibio_ingresos_pension      = any_of("p7500s2"),
                     ingresos_pension_monto        = any_of("p7500s2a1"),
                     recibio_pension_alimenticia   = any_of("p7500s3"),
                     pension_alimenticia_monto     = any_of("p7500s3a1"),
                     recibio_ingresos_no_laborales = any_of("p7505"),
                     recibio_transferencias_pais   = any_of("p7510s1"),
                     transferencias_pais_monto     = any_of("p7510s1a1"),
                     recibio_remesas_exterior      = any_of("p7510s2"),
                     remesas_exterior_monto        = any_of("p7510s2a1"),
                     recibio_ayudas_instituciones  = any_of("p7510s3"),
                     ayudas_instituciones_monto    = any_of("p7510s3a1"),
                     recibio_ingreso_inversiones   = any_of("p7510s5"),
                     ingreso_inversiones           = any_of("p7510s5a1"),
                     recibio_ingresos_cesantias    = any_of("p7510s6"),
                     ingresos_cesantias_monto      = any_of("p7510s6a1"),
                     recibio_otros_ingresos        = any_of("p7510s7"),
                     otros_ingresos_monto          = any_of("p7510s7a1"),
                     
                     # --- Ingresos antes de imputación ---
                     ingreso_actividad1                         = any_of("impa"),
                     ingreso_actividad2                         = any_of("isa"),
                     ingreso_en_especie_sin_imputar             = any_of("ie"),
                     ingreso_desocupados_inactivos_sin_imputar  = any_of("imdi"),
                     ingreso_intereses_dividendos_sin_imputar   = any_of("iof1"),
                     ingreso_pensiones_sin_imputar              = any_of("iof2"),
                     ingreso_ayudas_hogares_sin_imputar         = any_of("iof3h"),
                     ingreso_ayudas_instituciones_sin_imputar   = any_of("iof3i"),
                     ingreso_arriendos_sin_imputar              = any_of("iof6"),

                     #--- classificacion de estado laboral ---
                     edad_trabajar = pet, 
                     ocupado = oc, 
                     desocupado = des,
                     inactivo = ina,

                     # ingresos
                     ingreso_total_observado = ingtotob,
                     ingreso_total_imputado = ingtotes,
                     ingresos_total = ingtot) 

#--- Labels a variables
train_personas = train_personas %>%
                  mutate(sexo = case_when(sexo == 1 ~ "Hombre",
                                          sexo == 2 ~ "Mujer",
                                          .default = as.character(sexo)),
                        relacion_jefe = case_when(relacion_jefe == 1 ~ "Jefe(a) del hogar",
                                                  relacion_jefe == 2 ~ "Cónyuge o pareja",
                                                  relacion_jefe == 3 ~ "Hijo(a), hijastro(a)",
                                                  relacion_jefe == 4 ~ "Nieto(a)",
                                                  relacion_jefe == 5 ~ "Otro pariente",
                                                  relacion_jefe == 6 ~ "Empleado(a) del servicio",
                                                  relacion_jefe == 7 ~ "Pensionista",
                                                  relacion_jefe == 8 ~ "Trabajador",
                                                  relacion_jefe == 9 ~ "Otro",
                                                  .default = as.character(relacion_jefe)),
                    seguridad_social = case_when(seguridad_social == 1 ~ "Sí",
                                                seguridad_social == 2 ~ "No",
                                                seguridad_social == 9 ~ "No sabe",
                                                .default =  as.character(seguridad_social)),
                    ss_regimen = case_when(ss_regimen == 1 ~ "Contributivo",
                                          ss_regimen == 2 ~ "Especial",
                                          ss_regimen == 3 ~ "Subsidiado",
                                          ss_regimen == 9 ~ "No sabe",
                                          .default = as.character(ss_regimen)),
                    max_educ_level = case_when(max_educ_level == 1 ~ "Ninguno",
                                              max_educ_level == 2 ~ "Preescolar",
                                              max_educ_level == 3 ~ "Primaria",
                                              max_educ_level == 4 ~ "Secundaria",
                                              max_educ_level == 5 ~ "Media",
                                              max_educ_level == 6 ~ "Superior",
                                              max_educ_level == 9 ~ "No sabe",
                                              .default = as.character(max_educ_level)),
                    actividad_principal = case_when(actividad_principal == 1 ~ "Trabajando",
                                                    actividad_principal == 2 ~ "Buscando trabajo",
                                                    actividad_principal == 3 ~ "Estudiando",
                                                    actividad_principal == 4 ~ "Oficios del hogar",
                                                    actividad_principal == 5 ~ "Incapacitado permanente",
                                                    actividad_principal == 6 ~ "Otra actividad",
                                                    .default = as.character(actividad_principal)),
                    posicion_empleo = case_when(posicion_empleo == 1 ~ "Obrero/empleado empresa particular",
                                                posicion_empleo == 2 ~ "Obrero/empleado del gobierno",
                                                posicion_empleo == 3 ~ "Empleado doméstico",
                                                posicion_empleo == 4 ~ "Cuenta propia",
                                                posicion_empleo == 5 ~ "Patrón o empleador",
                                                posicion_empleo == 6 ~ "Familiar sin remuneración",
                                                posicion_empleo == 7 ~ "Sin remuneración en negocio de otro hogar",
                                                posicion_empleo == 8 ~ "Jornalero o peón",
                                                posicion_empleo == 9 ~ "Otro",
                                                .default = as.character(posicion_empleo)), 
                    cotiza_pension = case_when(cotiza_pension == 1 ~ "Sí",
                                              cotiza_pension == 2 ~ "No",
                                              cotiza_pension == 3 ~ "Ya es pensionado", 
                                              .default = as.character(cotiza_pension)),
                    segundo_trabajo = case_when(segundo_trabajo == 1 ~ "Sí",
                                                segundo_trabajo == 2 ~ "No",
                                                .default = as.character(segundo_trabajo)),
                    busca_mas_horas = case_when(busca_mas_horas == 1 ~ "Sí",
                                                busca_mas_horas == 2 ~ "No", 
                                                .default = as.character(busca_mas_horas)),

                    # desmpleado
                    disponible_trabajar = case_when(disponible_trabajar == 1 ~ "Sí",
                                                    disponible_trabajar == 2 ~ "No", 
                                                    disponible_trabajar == 9 ~ "No sabe",
                                                    .default = as.character(disponible_trabajar)),
                    experiencia_laboral_previa = case_when(experiencia_laboral_previa == 1 ~ "Primera vez",
                                                          experiencia_laboral_previa == 2 ~ "Trabajó antes",
                                                          .default = as.character(experiencia_laboral_previa)),
                    ocupacion_previa = case_when(ocupacion_previa == 1 ~ "Obrero/empleado empresa particular",
                                                ocupacion_previa == 2 ~ "Obrero/empleado del gobierno",
                                                ocupacion_previa == 3 ~ "Empleado doméstico",
                                                ocupacion_previa == 4 ~ "Cuenta propia",
                                                ocupacion_previa == 5 ~ "Patrón o empleador",
                                                ocupacion_previa == 6 ~ "Familiar sin remuneración",
                                                ocupacion_previa == 7 ~ "Sin remuneración en negocio de otro hogar",
                                                ocupacion_previa == 8 ~ "Jornalero o peón",
                                                ocupacion_previa == 9 ~ "Otro",
                                                .default = as.character(ocupacion_previa)), 
                posicion_empleo_segundo_trabajo = case_when(posicion_empleo_segundo_trabajo == 1 ~ "Obrero/empleado empresa particular",
                                                            posicion_empleo_segundo_trabajo == 2 ~ "Obrero/empleado del gobierno",
                                                            posicion_empleo_segundo_trabajo == 3 ~ "Empleado doméstico",
                                                            posicion_empleo_segundo_trabajo == 4 ~ "Cuenta propia",
                                                            posicion_empleo_segundo_trabajo == 5 ~ "Patrón o empleador",
                                                            posicion_empleo_segundo_trabajo == 6 ~ "Familiar sin remuneración",
                                                            posicion_empleo_segundo_trabajo == 7 ~ "Sin remuneración en negocio de otro hogar",
                                                            posicion_empleo_segundo_trabajo == 8 ~ "Jornalero o peón",
                                                            posicion_empleo_segundo_trabajo == 9 ~ "Otro",
                                                            .default = as.character(posicion_empleo_segundo_trabajo)),
                quiere_mas_horas = case_when(quiere_mas_horas == 1 ~ "Sí",
                                            quiere_mas_horas == 2 ~ "No", 
                                            .default = as.character(quiere_mas_horas)),
                busca_mas_horas = case_when(busca_mas_horas == 1 ~ "Sí",
                                            busca_mas_horas == 2 ~ "No", 
                                            .default = as.character(busca_mas_horas)),
                disponible_mas_horas = case_when(disponible_mas_horas == 1 ~ "Sí",
                                                  disponible_mas_horas == 2 ~ "No", 
                                                    .default = as.character(disponible_mas_horas)),
                motivo_cambio_capacidades = case_when(motivo_cambio_capacidades == 1 ~ "Sí",
                                                    motivo_cambio_capacidades == 2 ~ "No", 
                                                    .default = as.character(motivo_cambio_capacidades)),
                motivo_cambio_ingreso = case_when(motivo_cambio_ingreso == 1 ~ "Sí",
                                            motivo_cambio_ingreso == 2 ~ "No", 
                                            .default = as.character(motivo_cambio_ingreso)),
                  #--- Variables estado laboral
                  ocupado = ifelse(is.na(ocupado), yes = 0, no = 1), 
                  desocupado = ifelse(is.na(desocupado), yes = 0, no = 1),
                  inactivo = ifelse(is.na(inactivo), yes = 0, no = 1),
                  edad_trabajar = ifelse(is.na(edad_trabajar), yes = 0, 1),

                  #--- Labels a variables de ingresos laborales
                  across(.cols = c("recibio_horas_extras", "recibio_primas", "recibio_bonificaciones",
                                  "recibio_subsidio_alim", "recibio_subsidio_transporte",
                                  "recibio_subsidio_familiar", "recibio_subsidio_educ",
                                  "recibio_comida_como_pago", "recibio_vivenda_como_pago",
                                  "recibio_transporte_laboral_empresa", "recibio_otros_especie",
                                  "recibio_prima_servicios", "recibio_prima_navidad",
                                  "recibio_prima_vacaciones", "recibio_viaticos_perm",
                                  "recibio_bonus_anual"),
                        .fns = ~ case_when(.x == 1 ~ "Sí",
                                            .x == 2 ~ "No",
                                            .x == 9 ~ "No sabe",
                                            .default = as.character(.x))),
                # desmpleado
                busca_trabajo = case_when(busca_trabajo == 1 ~ "Sí",
                                          busca_trabajo == 2 ~ "No", 
                                          .default = as.character(busca_trabajo)),
                # recibio ingresos desempleo
                recibio_ingresos_desocupado = case_when(recibio_ingresos_desocupado == 1 ~ "Sí",
                                                        recibio_ingresos_desocupado == 2 ~ "No", 
                                                        .default = as.character(recibio_ingresos_desocupado)),
                # recibio ingresos inactivo
                recibio_ingresos_inactivo = case_when(recibio_ingresos_inactivo == 1 ~ "Sí",
                                                      recibio_ingresos_inactivo == 2 ~ "No", 
                                                      .default = as.character(recibio_ingresos_inactivo)),
                # recibio arriendos o pensiones
                recibio_arriendos_o_pensiones = case_when(recibio_arriendos_o_pensiones == 1 ~ "Sí",
                                                          recibio_arriendos_o_pensiones == 2 ~ "No", 
                                                          .default = as.character(recibio_arriendos_o_pensiones)))

#--- Imputación de algunos
train_personas = train_personas |> 
                  mutate(# todos los que no tienen edad para trabajar no tienen seguridad social
                        seguridad_social = ifelse(test = is.na(seguridad_social) & edad_trabajar == 0, yes = "No aplica", no = seguridad_social), 
                        
                        # todos los que no tienen edad para trabajar no tienen regimen de seguridad social
                        ss_regimen = ifelse(test = is.na(ss_regimen) & edad_trabajar == 0, yes = "No aplica", no = ss_regimen),
                        
                        # todos los que estan desempleados o inactivos o no en edad de trabajar no tienen oficio
                        oficio = ifelse(test = (desocupado == 1 | inactivo == 1 | edad_trabajar == 0) & is.na(oficio) , yes = "No aplica", no = as.character(oficio)),
                        
                        # todos los que estan desempleados o inactivos o no en edad de trabajar no tienen posicion de empleo
                        posicion_empleo = ifelse(test = (desocupado == 1 | inactivo == 1 | edad_trabajar == 0) & is.na(posicion_empleo), yes = "No aplica", no = posicion_empleo), 
                        
                        # todos los na en educacion son niños menores o iguales a 2 años
                        max_educ_level = ifelse(test = is.na(max_educ_level) & edad <= 2, yes = "Ninguno", no = max_educ_level), 
                        grado_aprobado = ifelse(test = is.na(grado_aprobado) & edad <= 2, yes = 0, no = grado_aprobado), 
                        
                        # todos los desocupados, inactivos, que no tienen edad para trabajar
                        # asimismo, todos los:c("Cuenta propia",  "Familiar sin remuneración", "Patrón o empleador", "Sin remuneración en negocio de otro hogar", "Otro")
                        across(.cols = c("recibio_horas_extras", "recibio_primas", "recibio_bonificaciones",
                                          "recibio_subsidio_alim", "recibio_subsidio_transporte",
                                          "recibio_subsidio_familiar", "recibio_subsidio_educ",
                                          "recibio_comida_como_pago", "recibio_vivenda_como_pago",
                                          "recibio_transporte_laboral_empresa", "recibio_otros_especie",
                                          "recibio_prima_servicios", "recibio_prima_navidad",
                                          "recibio_prima_vacaciones", "recibio_viaticos_perm",
                                          "recibio_bonus_anual"),
                                .fns = ~ ifelse(test = ((desocupado == 1 | inactivo == 1 | edad_trabajar == 0) & is.na(recibio_bonificaciones)) | 
                                                      (posicion_empleo %in% c("Cuenta propia",  "Familiar sin remuneración", "Patrón o empleador", "Sin remuneración en negocio de otro hogar", "Otro") & is.na(recibio_bonificaciones)) , 
                                                yes = "No aplica", 
                                                no = .x)),
                        across(.cols = c("incluyo_horas_extras", "incluyo_primas", "incluyo_bonificaciones",
                                          "incluyo_subsidio_alim", "incluyo_subsidio_transporte",
                                          "incluyo_subsidio_familiar", "incluyo_subsidio_educ"),
                                .fns = ~ ifelse(test = get(sub("incluyo", "recibio", cur_column())) %in% c("No", "No sabe", "No aplica") & is.na(.x),
                                                yes = "No aplica",
                                                no = .x)), 
                        # todos los que no estan ocupados no cotizan pension
                        cotiza_pension = ifelse(test = (ocupado == 0 & is.na(cotiza_pension)) | (edad <= 14 & is.na(cotiza_pension)), yes = "No aplica", no = cotiza_pension),
                        # segundo trabajo
                        segundo_trabajo = ifelse(test = ocupado == 0 & is.na(segundo_trabajo), yes = "No aplica", no = segundo_trabajo), 
                        # posicion empleo segundo trabajo
                        posicion_empleo_segundo_trabajo = ifelse(test = (segundo_trabajo == "No" | segundo_trabajo == "No aplica") & is.na(posicion_empleo_segundo_trabajo), yes = "No aplica", no = posicion_empleo_segundo_trabajo), 
                        # quiere trabajar mas horas
                        quiere_mas_horas = ifelse(test = ocupado == 0 & is.na(quiere_mas_horas), yes = "No aplica", no = quiere_mas_horas),
                        # busca mas horas
                        busca_mas_horas = ifelse(test = (ocupado == 0 & is.na(busca_mas_horas)) | quiere_mas_horas == "No", yes = "No aplica", no = busca_mas_horas), 
                        # disponible mas horas
                        disponible_mas_horas = ifelse(test = (ocupado == 0 & is.na(disponible_mas_horas)) | quiere_mas_horas == "No", yes = "No aplica", no = disponible_mas_horas), 
                        # motivo cambio capacidades
                        motivo_cambio_capacidades = ifelse(test = (busca_mas_horas == "No" | busca_mas_horas == "No aplica") & is.na(motivo_cambio_capacidades), yes = "No aplica", no = motivo_cambio_capacidades),
                        # motivo cambio ingreso
                        motivo_cambio_ingreso = ifelse(test = (busca_mas_horas == "No" | busca_mas_horas == "No aplica") & is.na(motivo_cambio_ingreso), yes = "No aplica", no = motivo_cambio_ingreso),
                        # experiencia laboral previa solo se le pregunta a los desocupados 
                        experiencia_laboral_previa = ifelse(test = (desocupado == 0) & is.na(experiencia_laboral_previa), yes = "No aplica", no = experiencia_laboral_previa), 
                        # solo los desocupados recibieron ingresos de desempleo, aquellos sin experincia laboral no han recibido ingresos
                        recibio_ingresos_desocupado = ifelse(test = (desocupado == 0) & is.na(recibio_ingresos_desocupado), yes = "No aplica", no = recibio_ingresos_desocupado),
                        recibio_ingresos_desocupado = ifelse(test = (experiencia_laboral_previa == "Primera vez") & is.na(recibio_ingresos_desocupado), yes = "No", no = recibio_ingresos_desocupado),
                        # solo los inactivos recibieron ingresos de inactividad
                        recibio_ingresos_inactivo = ifelse(test = (inactivo == 0) & is.na(recibio_ingresos_inactivo), yes = "No aplica", no = recibio_ingresos_inactivo)
                    ) 

#---------------------------#
# 2.2 test_personas ----

#---- Cambiando nombres
test_personas = test_personas %>%
                 rename(# --- Datos básicos ---
                     sexo             = any_of("p6020"),
                     edad             = any_of("p6040"),
                     relacion_jefe    = any_of("p6050"),
                     seguridad_social = any_of("p6090"),
                     ss_regimen       = any_of("p6100"),
                     max_educ_level   = any_of("p6210"),
                     grado_aprobado   = any_of("p6210s1"),
                     
                     #-------------------------#
                     #-------------------------#
                     # Ocupado

                     # --- Actividad y empleo ---
                     actividad_principal = any_of("p6240"),
                     antiguedad_empleo   = any_of("p6426"),
                     posicion_empleo     = any_of("p6430"),
                     tamano_empresa      = any_of("p6870"),
                     cotiza_pension      = any_of("p6920"),
                     
                     # --- Ingresos laborales (dinero) ---
                     recibio_horas_extras        = any_of("p6510"),
                     
                     # --- Subsidios ---
                     recibio_subsidio_alim          = any_of("p6585s1"),
                     recibio_subsidio_transporte    = any_of("p6585s2"),
                     recibio_subsidio_familiar      = any_of("p6585s3"),
                     recibio_subsidio_educ          = any_of("p6585s4"),

                     # --- Ingresos en especie ---
                    recibio_primas              = any_of("p6545"),
                    recibio_bonificaciones      = any_of("p6580"),
                     recibio_comida_como_pago          = any_of("p6590"),
                     recibio_vivenda_como_pago         = any_of("p6600"),
                     recibio_transporte_laboral_empresa= any_of("p6610"),
                     recibio_otros_especie             = any_of("p6620"),
                     
                     
                     # --- Primas y bonificaciones ---
                     recibio_prima_servicios   = any_of("p6630s1"),
                     recibio_prima_navidad     = any_of("p6630s2"),
                     recibio_prima_vacaciones  = any_of("p6630s3"),
                     recibio_viaticos_perm     = any_of("p6630s4"),
                     recibio_bonus_anual       = any_of("p6630s6"),
                     
                     # --- Independientes ---
                     horas_semana_trabajadas           = any_of("p6800"),
                     
                     # --- Segundo trabajo ---
                     segundo_trabajo                       = any_of("p7040"),
                     horas_segundo_trabajo                 = any_of("p7045"),
                     posicion_empleo_segundo_trabajo       = any_of("p7050"),
                     
                     # --- Subempleo y cambio ---
                     quiere_mas_horas           = any_of("p7090"),
                     busca_mas_horas            = any_of("p7110"),
                     disponible_mas_horas       = any_of("p7120"),
                     busca_trabajo              = any_of("p7150"),
                     disponible_trabajar        = any_of("p7160"),
                     
                     #-------------------------#
                     #-------------------------#
                     # Desocupado 

                     # --- Búsqueda de empleo / desocupados ---
                     experiencia_laboral_previa    = any_of("p7310"),
                     ocupacion_previa              = any_of("p7350"),
                     recibio_ingresos_desocupado   = any_of("p7422"),
                     recibio_ingresos_inactivo     = any_of("p7472"),
                     
                     # --- Ingresos no laborales ---
                     recibio_arriendos_o_pensiones = any_of("p7495"),
                     recibio_ingresos_pension      = any_of("p7500s2"),
                     recibio_pension_alimenticia   = any_of("p7500s3"),
                     recibio_ingresos_no_laborales = any_of("p7505"),
                     recibio_transferencias_pais   = any_of("p7510s1"),
                     recibio_remesas_exterior      = any_of("p7510s2"),
                     recibio_ayudas_instituciones  = any_of("p7510s3"),
                     recibio_ingreso_inversiones   = any_of("p7510s5"),
                     recibio_ingresos_cesantias    = any_of("p7510s6"),
                     recibio_otros_ingresos        = any_of("p7510s7"),
                     
                     #--- classificacion de estado laboral ---
                     edad_trabajar = pet, 
                     ocupado = oc, 
                     desocupado = des,
                     inactivo = ina) 
                     
#--- Labels a variables
test_personas = test_personas %>%
                  mutate(sexo = case_when(sexo == 1 ~ "Hombre",
                                          sexo == 2 ~ "Mujer",
                                          .default = as.character(sexo)),
                        relacion_jefe = case_when(relacion_jefe == 1 ~ "Jefe(a) del hogar",
                                                  relacion_jefe == 2 ~ "Cónyuge o pareja",
                                                  relacion_jefe == 3 ~ "Hijo(a), hijastro(a)",
                                                  relacion_jefe == 4 ~ "Nieto(a)",
                                                  relacion_jefe == 5 ~ "Otro pariente",
                                                  relacion_jefe == 6 ~ "Empleado(a) del servicio",
                                                  relacion_jefe == 7 ~ "Pensionista",
                                                  relacion_jefe == 8 ~ "Trabajador",
                                                  relacion_jefe == 9 ~ "Otro",
                                                  .default = as.character(relacion_jefe)),
                    seguridad_social = case_when(seguridad_social == 1 ~ "Sí",
                                                seguridad_social == 2 ~ "No",
                                                seguridad_social == 9 ~ "No sabe",
                                                .default =  as.character(seguridad_social)),
                    ss_regimen = case_when(ss_regimen == 1 ~ "Contributivo",
                                          ss_regimen == 2 ~ "Especial",
                                          ss_regimen == 3 ~ "Subsidiado",
                                          ss_regimen == 9 ~ "No sabe",
                                          .default = as.character(ss_regimen)),
                    max_educ_level = case_when(max_educ_level == 1 ~ "Ninguno",
                                              max_educ_level == 2 ~ "Preescolar",
                                              max_educ_level == 3 ~ "Primaria",
                                              max_educ_level == 4 ~ "Secundaria",
                                              max_educ_level == 5 ~ "Media",
                                              max_educ_level == 6 ~ "Superior",
                                              max_educ_level == 9 ~ "No sabe",
                                              .default = as.character(max_educ_level)),
                    actividad_principal = case_when(actividad_principal == 1 ~ "Trabajando",
                                                    actividad_principal == 2 ~ "Buscando trabajo",
                                                    actividad_principal == 3 ~ "Estudiando",
                                                    actividad_principal == 4 ~ "Oficios del hogar",
                                                    actividad_principal == 5 ~ "Incapacitado permanente",
                                                    actividad_principal == 6 ~ "Otra actividad",
                                                    .default = as.character(actividad_principal)),
                    posicion_empleo = case_when(posicion_empleo == 1 ~ "Obrero/empleado empresa particular",
                                                posicion_empleo == 2 ~ "Obrero/empleado del gobierno",
                                                posicion_empleo == 3 ~ "Empleado doméstico",
                                                posicion_empleo == 4 ~ "Cuenta propia",
                                                posicion_empleo == 5 ~ "Patrón o empleador",
                                                posicion_empleo == 6 ~ "Familiar sin remuneración",
                                                posicion_empleo == 7 ~ "Sin remuneración en negocio de otro hogar",
                                                posicion_empleo == 8 ~ "Jornalero o peón",
                                                posicion_empleo == 9 ~ "Otro",
                                                .default = as.character(posicion_empleo)), 
                    cotiza_pension = case_when(cotiza_pension == 1 ~ "Sí",
                                              cotiza_pension == 2 ~ "No",
                                              cotiza_pension == 3 ~ "Ya es pensionado", 
                                              .default = as.character(cotiza_pension)),
                    segundo_trabajo = case_when(segundo_trabajo == 1 ~ "Sí",
                                                segundo_trabajo == 2 ~ "No",
                                                .default = as.character(segundo_trabajo)),
                    busca_mas_horas = case_when(busca_mas_horas == 1 ~ "Sí",
                                                busca_mas_horas == 2 ~ "No", 
                                                .default = as.character(busca_mas_horas)),

                    # desmpleado
                    disponible_trabajar = case_when(disponible_trabajar == 1 ~ "Sí",
                                                    disponible_trabajar == 2 ~ "No", 
                                                    disponible_trabajar == 9 ~ "No sabe",
                                                    .default = as.character(disponible_trabajar)),
                    experiencia_laboral_previa = case_when(experiencia_laboral_previa == 1 ~ "Primera vez",
                                                          experiencia_laboral_previa == 2 ~ "Trabajó antes",
                                                          .default = as.character(experiencia_laboral_previa)),
                    ocupacion_previa = case_when(ocupacion_previa == 1 ~ "Obrero/empleado empresa particular",
                                                ocupacion_previa == 2 ~ "Obrero/empleado del gobierno",
                                                ocupacion_previa == 3 ~ "Empleado doméstico",
                                                ocupacion_previa == 4 ~ "Cuenta propia",
                                                ocupacion_previa == 5 ~ "Patrón o empleador",
                                                ocupacion_previa == 6 ~ "Familiar sin remuneración",
                                                ocupacion_previa == 7 ~ "Sin remuneración en negocio de otro hogar",
                                                ocupacion_previa == 8 ~ "Jornalero o peón",
                                                ocupacion_previa == 9 ~ "Otro",
                                                .default = as.character(ocupacion_previa)), 
                posicion_empleo_segundo_trabajo = case_when(posicion_empleo_segundo_trabajo == 1 ~ "Obrero/empleado empresa particular",
                                                            posicion_empleo_segundo_trabajo == 2 ~ "Obrero/empleado del gobierno",
                                                            posicion_empleo_segundo_trabajo == 3 ~ "Empleado doméstico",
                                                            posicion_empleo_segundo_trabajo == 4 ~ "Cuenta propia",
                                                            posicion_empleo_segundo_trabajo == 5 ~ "Patrón o empleador",
                                                            posicion_empleo_segundo_trabajo == 6 ~ "Familiar sin remuneración",
                                                            posicion_empleo_segundo_trabajo == 7 ~ "Sin remuneración en negocio de otro hogar",
                                                            posicion_empleo_segundo_trabajo == 8 ~ "Jornalero o peón",
                                                            posicion_empleo_segundo_trabajo == 9 ~ "Otro",
                                                            .default = as.character(posicion_empleo_segundo_trabajo)),
                quiere_mas_horas = case_when(quiere_mas_horas == 1 ~ "Sí",
                                            quiere_mas_horas == 2 ~ "No", 
                                            .default = as.character(quiere_mas_horas)),
                busca_mas_horas = case_when(busca_mas_horas == 1 ~ "Sí",
                                            busca_mas_horas == 2 ~ "No", 
                                            .default = as.character(busca_mas_horas)),
                disponible_mas_horas = case_when(disponible_mas_horas == 1 ~ "Sí",
                                                  disponible_mas_horas == 2 ~ "No", 
                                                    .default = as.character(disponible_mas_horas)),
                  #--- Variables estado laboral
                  ocupado = ifelse(is.na(ocupado), yes = 0, no = 1), 
                  desocupado = ifelse(is.na(desocupado), yes = 0, no = 1),
                  inactivo = ifelse(is.na(inactivo), yes = 0, no = 1),
                  edad_trabajar = ifelse(is.na(edad_trabajar), yes = 0, 1),
                  
                # desmpleado
                busca_trabajo = case_when(busca_trabajo == 1 ~ "Sí",
                                          busca_trabajo == 2 ~ "No", 
                                          .default = as.character(busca_trabajo)),
                # recibio ingresos desempleo
                recibio_ingresos_desocupado = case_when(recibio_ingresos_desocupado == 1 ~ "Sí",
                                                        recibio_ingresos_desocupado == 2 ~ "No", 
                                                        .default = as.character(recibio_ingresos_desocupado)),
                # recibio ingresos inactivo
                recibio_ingresos_inactivo = case_when(recibio_ingresos_inactivo == 1 ~ "Sí",
                                                      recibio_ingresos_inactivo == 2 ~ "No", 
                                                      .default = as.character(recibio_ingresos_inactivo)),
                # recibio arriendos o pensiones
                recibio_arriendos_o_pensiones = case_when(recibio_arriendos_o_pensiones == 1 ~ "Sí",
                                                          recibio_arriendos_o_pensiones == 2 ~ "No", 
                                                          .default = as.character(recibio_arriendos_o_pensiones)))

#--- Imputación de algunos
test_personas = test_personas |> 
                  mutate(# todos los que no tienen edad para trabajar no tienen seguridad social
                        seguridad_social = ifelse(test = is.na(seguridad_social) & edad_trabajar == 0, yes = "No aplica", no = seguridad_social), 
                        
                        # todos los que no tienen edad para trabajar no tienen regimen de seguridad social
                        ss_regimen = ifelse(test = is.na(ss_regimen) & edad_trabajar == 0, yes = "No aplica", no = ss_regimen),
                        
                        # todos los que estan desempleados o inactivos o no en edad de trabajar no tienen oficio
                        oficio = ifelse(test = (desocupado == 1 | inactivo == 1 | edad_trabajar == 0) & is.na(oficio) , yes = "No aplica", no = as.character(oficio)),
                        
                        # todos los que estan desempleados o inactivos o no en edad de trabajar no tienen posicion de empleo
                        posicion_empleo = ifelse(test = (desocupado == 1 | inactivo == 1 | edad_trabajar == 0) & is.na(posicion_empleo), yes = "No aplica", no = posicion_empleo), 
                        
                        # todos los na en educacion son niños menores o iguales a 2 años
                        max_educ_level = ifelse(test = is.na(max_educ_level) & edad <= 2, yes = "Ninguno", no = max_educ_level), 
                        grado_aprobado = ifelse(test = is.na(grado_aprobado) & edad <= 2, yes = 0, no = grado_aprobado), 
                        
                        # todos los desocupados, inactivos, que no tienen edad para trabajar
                        # asimismo, todos los:c("Cuenta propia",  "Familiar sin remuneración", "Patrón o empleador", "Sin remuneración en negocio de otro hogar", "Otro")
                        across(.cols = c("recibio_horas_extras", "recibio_primas", "recibio_bonificaciones",
                                          "recibio_subsidio_alim", "recibio_subsidio_transporte",
                                          "recibio_subsidio_familiar", "recibio_subsidio_educ",
                                          "recibio_comida_como_pago", "recibio_vivenda_como_pago",
                                          "recibio_transporte_laboral_empresa", "recibio_otros_especie",
                                          "recibio_prima_servicios", "recibio_prima_navidad",
                                          "recibio_prima_vacaciones", "recibio_viaticos_perm",
                                          "recibio_bonus_anual"),
                                .fns = ~ ifelse(test = ((desocupado == 1 | inactivo == 1 | edad_trabajar == 0) & is.na(recibio_bonificaciones)) | 
                                                      (posicion_empleo %in% c("Cuenta propia",  "Familiar sin remuneración", "Patrón o empleador", "Sin remuneración en negocio de otro hogar", "Otro") & is.na(recibio_bonificaciones)) , 
                                                yes = "No aplica", 
                                                no = .x)),
                        # todos los que no estan ocupados no cotizan pension
                        cotiza_pension = ifelse(test = (ocupado == 0 & is.na(cotiza_pension)) | (edad <= 14 & is.na(cotiza_pension)), yes = "No aplica", no = cotiza_pension),
                        # segundo trabajo
                        segundo_trabajo = ifelse(test = ocupado == 0 & is.na(segundo_trabajo), yes = "No aplica", no = segundo_trabajo), 
                        # posicion empleo segundo trabajo
                        posicion_empleo_segundo_trabajo = ifelse(test = (segundo_trabajo == "No" | segundo_trabajo == "No aplica") & is.na(posicion_empleo_segundo_trabajo), yes = "No aplica", no = posicion_empleo_segundo_trabajo), 
                        # quiere trabajar mas horas
                        quiere_mas_horas = ifelse(test = ocupado == 0 & is.na(quiere_mas_horas), yes = "No aplica", no = quiere_mas_horas),
                        # busca mas horas
                        busca_mas_horas = ifelse(test = (ocupado == 0 & is.na(busca_mas_horas)) | quiere_mas_horas == "No", yes = "No aplica", no = busca_mas_horas), 
                        # disponible mas horas
                        disponible_mas_horas = ifelse(test = (ocupado == 0 & is.na(disponible_mas_horas)) | quiere_mas_horas == "No", yes = "No aplica", no = disponible_mas_horas), 
                        # experiencia laboral previa solo se le pregunta a los desocupados 
                        experiencia_laboral_previa = ifelse(test = (desocupado == 0) & is.na(experiencia_laboral_previa), yes = "No aplica", no = experiencia_laboral_previa), 
                        # solo los desocupados recibieron ingresos de desempleo, aquellos sin experincia laboral no han recibido ingresos
                        recibio_ingresos_desocupado = ifelse(test = (desocupado == 0) & is.na(recibio_ingresos_desocupado), yes = "No aplica", no = recibio_ingresos_desocupado),
                        recibio_ingresos_desocupado = ifelse(test = (experiencia_laboral_previa == "Primera vez") & is.na(recibio_ingresos_desocupado), yes = "No", no = recibio_ingresos_desocupado),
                        # solo los inactivos recibieron ingresos de inactividad
                        recibio_ingresos_inactivo = ifelse(test = (inactivo == 0) & is.na(recibio_ingresos_inactivo), yes = "No aplica", no = recibio_ingresos_inactivo)
                    ) 


#=====================#
# 3. Export data ----
#=====================#
export(train_personas, "stores/A_procesamiento/02_train_personas.rds")
export(test_personas, "stores/A_procesamiento/02_test_personas.rds")