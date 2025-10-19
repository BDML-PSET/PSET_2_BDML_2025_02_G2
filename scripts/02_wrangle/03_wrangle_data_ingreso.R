### setup
cat("\f")
rm(list = ls())
source('scripts/00_packages.R')


#===========================#
# 1. importar datos ----
#===========================#
train_personas = import('stores/output/01_import/01_train_personas.rds')
train_hogar = import('stores/output/01_import/01_train_hogares.rds')
test_personas = import('stores/output/01_import/01_test_personas.rds')
test_hogar = import('stores/output/01_import/01_test_hogares.rds')

# select columns
vars_test_hogares = import('stores/input/test_hogares.csv',setclass = 'tibble') %>% clean_names() %>% colnames()
vars_test_personas = import('stores/input/test_personas.csv',setclass = 'tibble') %>% clean_names() %>% colnames()

train_personas = train_personas %>% 
                 select(all_of(c(vars_test_personas)))

train_hogar = train_hogar %>% 
                select(all_of(c(vars_test_hogares,"pobre","ingpcug")))

# rm 
rm(vars_test_hogares, vars_test_personas)


#===========================#
# 2. clean data hogar ----
#===========================#

#----------------------#
# 2.1. train_hogar ----
train_hogar = train_hogar %>%
               rename(# personas, 
                      n_per              = nper , 
                      n_per_unidad_gasto = npersug, 

                      # ingresos,
                      ing_unidad_gasto_imputado_per_capita = ingpcug,

                      #
                      n_habitaciones            = any_of("p5000"),
                      n_cuartos_dormir          = any_of("p5010"),
                      tenencia_vivienda         = any_of("p5090"),
                      pago_mensual_hipoteca     = any_of("p5100"),
                      arriendo_imputado_mensual = any_of("p5130"),
                      arriendo_mensual          = any_of("p5140"),
                      linea_indigencia          = any_of("li"),
                      linea_pobreza             = any_of("lp"), 
                      cod_dpto = depto) |> 
               select(id, cod_dpto, clase, dominio,
                      tenencia_vivienda, n_habitaciones, n_cuartos_dormir,
                      n_per, n_per_unidad_gasto, 
                      pago_mensual_hipoteca, arriendo_mensual, arriendo_imputado_mensual,
                      ing_unidad_gasto_imputado_per_capita,
                      linea_pobreza, pobre, 
                      linea_indigencia, everything()) 

train_hogar = train_hogar |>  
              mutate(pobre = ifelse(pobre == 1, yes = "Si", "No"), 
                     tenencia_vivienda = case_when(tenencia_vivienda == 1 ~ "Propia, pagada",
                                                   tenencia_vivienda == 2 ~ "Propia, pagando",
                                                   tenencia_vivienda == 3 ~ "Arrendada",
                                                   tenencia_vivienda == 4 ~ "En usufructo",
                                                   tenencia_vivienda == 5 ~ "Posesión sin titulo",
                                                   tenencia_vivienda == 6 ~ "Otro",
                                                   .default =  as.character(tenencia_vivienda)),
                     clase = case_when(clase == 1 ~ "Cabecera",
                                       clase == 2 ~ "Resto",
                                       .default = as.character(clase)))

#----------------------#
# 2.2. test_hogar   ----
test_hogar = test_hogar %>%
               rename(# personas, 
                      n_per              = nper , 
                      n_per_unidad_gasto = npersug, 

                      #
                      n_habitaciones            = any_of("p5000"),
                      n_cuartos_dormir          = any_of("p5010"),
                      tenencia_vivienda         = any_of("p5090"),
                      pago_mensual_hipoteca     = any_of("p5100"),
                      arriendo_imputado_mensual = any_of("p5130"),
                      arriendo_mensual          = any_of("p5140"),
                      linea_indigencia          = any_of("li"),
                      linea_pobreza             = any_of("lp"), 
                      cod_dpto = depto) |> 
               select(id, cod_dpto, clase, dominio,
                      tenencia_vivienda, n_habitaciones, n_cuartos_dormir,
                      n_per, n_per_unidad_gasto, 
                      pago_mensual_hipoteca, arriendo_mensual, arriendo_imputado_mensual, everything()) 

test_hogar = test_hogar |>  
              mutate(tenencia_vivienda = case_when(tenencia_vivienda == 1 ~ "Propia, pagada",
                                                   tenencia_vivienda == 2 ~ "Propia, pagando",
                                                   tenencia_vivienda == 3 ~ "Arrendada",
                                                   tenencia_vivienda == 4 ~ "En usufructo",
                                                   tenencia_vivienda == 5 ~ "Posesión sin titulo",
                                                   tenencia_vivienda == 6 ~ "Otro",
                                                   .default =  as.character(tenencia_vivienda)),
                     clase = case_when(clase == 1 ~ "Cabecera",
                                       clase == 2 ~ "Resto",
                                       .default = as.character(clase)))

#===========================#
# 3. clean data personas ----
#===========================#

clean_persona = function(x){
      x = x %>%
          rename(# --- Datos básicos ---
              sexo             = any_of("p6020"),
              edad             = any_of("p6040"),
              relacion_jefe    = any_of("p6050"),
              seguridad_social = any_of("p6090"),
              ss_regimen       = any_of("p6100"),
              max_educ_level   = any_of("p6210"),
              grado_aprobado   = any_of("p6210s1"),
              cod_dpto = depto,
              
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
              inactivo = ina) |> 
              relocate(cod_dpto, .before = "dominio")

      #--- Labels a variables
      x = x %>%
          mutate(clase = ifelse(clase == 1, yes = "Cabecera", no = "Resto"),
                sexo = case_when(sexo == 1 ~ "Hombre",
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
                                        .default = "No Aplica"), 
          #--- Variables estado laboral
          ocupado = ifelse(is.na(ocupado), yes = 0, no = 1), 
          desocupado = ifelse(is.na(desocupado), yes = 0, no = 1),
          inactivo = ifelse(is.na(inactivo), yes = 0, no = 1),
          edad_trabajar = ifelse(is.na(edad_trabajar), yes = 0, 1),

          #--- Labels a variables de ingresos laborales
          across(.cols = c("recibio_horas_extras", "recibio_primas", "recibio_bonificaciones",
                          "recibio_bonus_anual"),
                .fns = ~ case_when(.x == 1 ~ "Sí",
                                    .x == 2 ~ "No",
                                    .x == 9 ~ "No sabe",
                                    .default = as.character(.x))),
        # desmpleado
        busca_trabajo = case_when(busca_trabajo == 1 ~ "Sí",
                                  busca_trabajo == 2 ~ "No", 
                                  .default = as.character(busca_trabajo)),
        across(.cols = c("recibio_ingresos_desocupado", "recibio_ingresos_inactivo", "recibio_arriendos_o_pensiones", "recibio_ingresos_pension", 
                         "recibio_transferencias_pais", "recibio_remesas_exterior", "recibio_ayudas_instituciones", "recibio_ingreso_inversiones", "recibio_ingresos_cesantias", 
                         "recibio_otros_ingresos", ),
                .fns = ~ case_when(.x == 1 ~ "Sí",
                                    .x == 2 ~ "No",
                                    .x == 9 ~ "No sabe",
                                    .default = as.character(.x))))

      #--- Imputación de algunos
      x = x |> 
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
                                  "recibio_prima_vacaciones", "recibio_viaticos_perm"),
                        .fns = ~ ifelse(test = ((desocupado == 1 | inactivo == 1 | edad_trabajar == 0) & is.na(recibio_bonificaciones)) | 
                                              (posicion_empleo %in% c("Cuenta propia",  "Familiar sin remuneración", "Patrón o empleador", "Sin remuneración en negocio de otro hogar", "Otro") & is.na(recibio_bonificaciones)) , 
                                        yes = "No aplica", 
                                        no = .x)),
                # todos los que no estan ocupados no cotizan pension
                cotiza_pension = ifelse(test = (ocupado == 0 & is.na(cotiza_pension)) | (edad <= 14 & is.na(cotiza_pension)), yes = "No aplica", no = cotiza_pension),
                # experiencia laboral previa solo se le pregunta a los desocupados 
                experiencia_laboral_previa = ifelse(test = (desocupado == 0) & is.na(experiencia_laboral_previa), yes = "No aplica", no = experiencia_laboral_previa), 
                # solo los desocupados recibieron ingresos de desempleo, aquellos sin experincia laboral no han recibido ingresos
                recibio_ingresos_desocupado = ifelse(test = (desocupado == 0) & is.na(recibio_ingresos_desocupado), yes = "No aplica", no = recibio_ingresos_desocupado),
                recibio_ingresos_desocupado = ifelse(test = (experiencia_laboral_previa == "Primera vez") & is.na(recibio_ingresos_desocupado), yes = "No", no = recibio_ingresos_desocupado),
                # solo los inactivos recibieron ingresos de inactividad
                recibio_ingresos_inactivo = ifelse(test = (inactivo == 0) & is.na(recibio_ingresos_inactivo), yes = "No aplica", no = recibio_ingresos_inactivo), 
                # solo los que ponene no o no aplica en seguridad social no tienen ss_regimen
                ss_regimen = ifelse(test = seguridad_social %in% c("No", "No sabe", "No aplica") & is.na(ss_regimen), yes = "No aplica", no = ss_regimen),
            ) 
  
        db_personas = x |> 
                select(id, 
                      # variables de geografia
                      clase, dominio, 
                      # variables de personas
                      sexo, edad, relacion_jefe, seguridad_social, ss_regimen, max_educ_level, actividad_principal, 

                      # variables de ingresos
                      oficio,  antiguedad_empleo, posicion_empleo, recibio_horas_extras, recibio_primas, recibio_bonificaciones, recibio_bonus_anual, horas_semana_trabajadas, 
                      cotiza_pension, 

                      ## variables ingreso de desempleados
                      busca_trabajo, disponible_trabajar, experiencia_laboral_previa, ocupacion_previa , 
                      
                      # recibio remesas
                      recibio_ingresos_desocupado, recibio_ingresos_inactivo, recibio_arriendos_o_pensiones, recibio_ingresos_pension, 
                      recibio_transferencias_pais, recibio_remesas_exterior, recibio_ayudas_instituciones, recibio_ingreso_inversiones, recibio_ingresos_cesantias, 
                      recibio_otros_ingresos, 

                      # estado de trabajo
                      edad_trabajar, ocupado, desocupado, inactivo
                      ) |> 
              replace_na(list(actividad_principal = "Ninguna", 
                              antiguedad_empleo = 0, 
                              horas_semana_trabajadas = 0, 
                              ingresos_total = 0, 
                              busca_trabajo = "No Aplica", 
                              disponible_trabajar = "No Apica"
                            )) |> 
              mutate(across(.cols = starts_with("recibio"), ~ ifelse(is.na(.x), yes = "No Aplica", no = .x)), 
                     sexo = ifelse(sexo == "Hombre", yes = 1, no = 0),
                     grupo_etario = case_when(edad < 12 ~ "Niñez",
                                              edad < 18 ~ "Adolescencia",
                                              edad < 30 ~ "Juventud",
                                              edad < 60 ~ "Adultez",
                                              .default = "Vejez")) |> 
                    as.data.table()
  
    # pivot and group by id 
    num_vars = names(db_personas)[sapply(db_personas, is.numeric)]
    cat_vars = names(db_personas)[sapply(db_personas, \(x) is.character(x) || is.factor(x))] %>% .[!. %in% c("id", "clase", "dominio", "region")]

    categorical = lapply(cat_vars, FUN = function(x){
      print(x)
      a = db_personas[, by = .(id, get(x)), .(total = .N)]

      a[, total := total/sum(total)]
      a[, get := paste0(x,"_", get)]
      a= dcast(a, id ~ get,value.var = "total", fill = 0, fun.aggregate = sum)
      return(a)
    })

    categorical = Reduce(function(x, y) merge(x, y, by = "id", all = TRUE), categorical)
    numeric = db_personas[, by = id,lapply(.SD, mean, na.rm = TRUE), .SDcols = num_vars]
    db_personas = full_join(categorical, numeric)  
    rm(categorical, numeric, num_vars, cat_vars)
  
  return(db_personas)
}

#---------------------------#
# 2.1 train_personas ----
train_personas = clean_persona(train_personas) |> select(-oficio_0)
test_personas = clean_persona(test_personas)


#===========================#
# 5. union con datos     ----
#===========================#
train = train_personas %>% 
        left_join(.,
                  train_hogar |> 
                  select(id, cod_dpto, clase, tenencia_vivienda, n_habitaciones, n_cuartos_dormir, n_per, n_per_unidad_gasto, linea_pobreza, ing_unidad_gasto_imputado_per_capita, pobre), by = "id") |> 
        relocate(cod_dpto, clase,  .before = id) |> 
        mutate(linea_pobreza = log(linea_pobreza+1), 
               ing_unidad_gasto_imputado_per_capita = log(ing_unidad_gasto_imputado_per_capita+1))

test = test_personas %>% 
        left_join(.,
                  test_hogar |> 
                  select(id, cod_dpto, clase, tenencia_vivienda, n_habitaciones, n_cuartos_dormir, n_per, n_per_unidad_gasto, linea_pobreza), by = "id") |> 
        relocate(cod_dpto, clase,  .before = id) |> 
        mutate(linea_pobreza = log(linea_pobreza+1))

#===========================#
# 6. export     ----
#===========================#
export(train, "stores/output/02_wrangle/03_train_ingreso.rds")
export(test, "stores/output/02_wrangle/03_test_ingreso.rds")