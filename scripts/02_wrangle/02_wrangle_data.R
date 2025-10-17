### setup
cat("\f")
rm(list = ls())
source('scripts/00_packages.R')

#--------------#
# 1. Load data #
#--------------#

train_personas = import('stores/output/01_import/01_train_personas.rds',setclass = 'tibble')
train_hogares = import('stores/output/01_import/01_train_hogares.rds',setclass = 'tibble')
test_personas = import('stores/output/01_import/01_test_personas.rds',setclass = 'tibble')
test_hogares = import('stores/output/01_import/01_test_hogares.rds',setclass = 'tibble')

#---------------------#
# 2. Feature Cleaning #
#---------------------#

### Household features
f_process_hogares = function(data){
  
  ### Choose covariates
  df = data %>% 
    select(id,clase,dominio,depto,
           cantidad_cuartos = p5000,
           cantidad_cuartos_para_dormir = p5010,
           tipo_propiedad_vivienda_hogar = p5090,
           cuanto_paga_por_arriendo = p5140,
           cuanto_deberia_pagar_por_arriendo = p5130,
           numero_personas_hogar = nper,
           numero_personas_unidad_gasto = npersug,
           any_of(c('pobre','ingpcug','lp')))
  
  ### Create unified rent variable, plus add a marker if  the value is imputed or observed
  df = df %>% 
    mutate(arriendo = ifelse(is.na(cuanto_paga_por_arriendo),cuanto_deberia_pagar_por_arriendo,cuanto_paga_por_arriendo)) %>% 
    select(-cuanto_deberia_pagar_por_arriendo,-cuanto_paga_por_arriendo)
  
  ### Handle categorical predictors
  df = df %>% 
    mutate(urbano = ifelse(clase == 1,1,0)) %>% ### Urban covariate
    select(-clase)
  
  ### Handle categorical predictors
  df = df %>% 
    dummy_cols(c('dominio','depto','tipo_propiedad_vivienda_hogar'),
               remove_first_dummy = TRUE,
               remove_selected_columns = FALSE)
  
  ### Clean variable names
  df = df %>% 
    clean_names()
  
  return(df)
  
}

### Person in household features
f_process_personas = function(data){
  
  ### Clean covariates
  df_personas_train = data %>% 
    select(id,orden,clase,
           sexo = p6020,edad = p6040,
           parentesco_jefe_hogar = p6050,
           cotiza_salud = p6090,cotiza_pension = p6920,regimen_salud = p6100,
           principal_actividad = p6240,
           maximo_nivel_educativo = p6210,
           ocupado_tiempo_en_la_empresa = p6426,
           ocupado_relab = p6430,
           oficio,
           ocupado_recibio_pago_horas_extra = p6510,
           ocupado_recibio_primas = p6545,
           ocupado_subsidio_alimentacion = p6585s1,
           ocupado_subsidio_transporte = p6585s2,
           ocupado_subsidio_familiar = p6585s3,
           ocupado_subsidio_educativo = p6585s4,
           ocupado_pago_en_alimentos = p6590,
           ocupado_pago_en_vivienda = p6600,
           ocupado_transporte_de_la_empresa_para_trabajar = p6610,
           ocupado_pago_en_especie = p6620,
           ocupado_recibio_prima_servicio = p6630s1,
           ocupado_recibio_prima_navidad = p6630s2,
           ocupado_recibio_prima_vacaciones = p6630s3,
           ocupado_recibio_viaticos_permanentes = p6630s4,
           ocupado_recibio_bonificaciones_anuales = p6630s6,
           ocupado_tamano_de_la_empresa = p6870,
           ocupado_horas_trabajadas_normalmente = p6800,
           ocupado_segundo_trabajo = p7040,
           ocupado_segundo_trabajo_horas_trabajadas = p7045,
           ocupado_segundo_trabajo_relab = p7050,
           ocupado_quiere_trabajar_mas_horas = p7090,
           ocupado_hizo_diligencias_trabajar_mas_horas = p7110,
           ocupado_estaba_disponible_trabajar_mas_horas = p7120,
           ocupado_cambiar_trabajo_diligencias = p7150,
           ocupado_cambiar_trabajo_en_un_mes = p7160,
           desocupados_relab = p7350,
           desocupados_recibio_ingresos = p7422,
           desocupados_pago_por_arriendo_y_o_pension = p7495,
           desocupados_ingresos_otros_hogares = p7505,
           pet,oc,des,ina) 
  
  ### Add covariates
  df_personas_train = df_personas_train %>% 
    mutate(edad_sq = edad*edad,
           edad_menor_18 = ifelse(edad < 18,1,0),
           edad_menor_5 = ifelse(edad <= 5,1,0),
           edad_mayor_65 = ifelse(edad >= 65,1,0))
  
  ### Clean Indicator variables
  df_personas_train = df_personas_train %>% 
    mutate(across(.cols = c(pet,oc,des,ina),
                  .fns =  function(x) ifelse(is.na(x),0,x)))
  
  ### Indicator Variables NINI and Informal labor
  df_personas_train = df_personas_train %>% 
    mutate(female = ifelse(sexo == 2,1,0),
           informal = ifelse(oc == 1 & cotiza_salud == 2 & cotiza_pension == 2,1,0),
           nini = case_when(clase == 1 & edad >= 12 & edad <= 28 & !principal_actividad %in% c(1,2,3) ~ 1,
                            clase == 2 & edad >= 10 & edad <= 28 & !principal_actividad %in% c(1,2,3) ~ 1,
                            .default = 0)) %>% 
    select(-sexo,-cotiza_salud,-cotiza_pension,-clase,-principal_actividad) 
  
  ### Indicator variables position within household
  df_personas_train = df_personas_train %>% 
    mutate(posicion_jefe_del_hogar = ifelse(parentesco_jefe_hogar == 1,1,0),
           posicion_conyuge_jefe_del_hogar = ifelse(parentesco_jefe_hogar == 2,1,0),
           posicion_hijo = ifelse(parentesco_jefe_hogar == 3,1,0),
           posicion_otro_pariente = ifelse(parentesco_jefe_hogar == 5,1,0),
           posicion_nieto = ifelse(parentesco_jefe_hogar == 4,1,0),
           posicion_empleado_servicio_domestico = ifelse(parentesco_jefe_hogar == 6,1,0)) %>% 
    select(-parentesco_jefe_hogar)
  
  ### Clean education covariate
  df_personas_train = df_personas_train %>% 
    mutate(maximo_nivel_educativo = ifelse(maximo_nivel_educativo == 9,1,maximo_nivel_educativo)) 
  
  ### One hot encode categorical variables
  df_personas_train = df_personas_train %>% 
    dummy_cols(c("regimen_salud","maximo_nivel_educativo"),
               remove_selected_columns = TRUE,
               ignore_na = TRUE)
  
  ### Handle categorical variables that require dummy encoding
  df_personas_train = df_personas_train %>% 
    mutate(across(.cols = c(ocupado_recibio_pago_horas_extra,ocupado_recibio_primas, ### Primas y horas extra
                            ocupado_subsidio_alimentacion,ocupado_subsidio_transporte,ocupado_subsidio_familiar,ocupado_subsidio_educativo, ### Subsidios
                            ocupado_pago_en_alimentos,ocupado_pago_en_vivienda,ocupado_pago_en_especie, ### Pagos no monetarios
                            ocupado_transporte_de_la_empresa_para_trabajar, ### Transporte para ir a trabajar
                            ocupado_recibio_prima_servicio,ocupado_recibio_prima_navidad,ocupado_recibio_prima_vacaciones, ### Tipos de primas
                            ocupado_recibio_viaticos_permanentes,ocupado_recibio_bonificaciones_anuales, ### Otros beneficios
                            ocupado_segundo_trabajo, ### Tiene un segundo trabajo
                            ocupado_quiere_trabajar_mas_horas,ocupado_hizo_diligencias_trabajar_mas_horas,ocupado_cambiar_trabajo_en_un_mes, ### Trabajar  mas horas y cambiar de trabajo
                            desocupados_recibio_ingresos,desocupados_pago_por_arriendo_y_o_pension,desocupados_ingresos_otros_hogares), ### Desocupados 
                  .fns = function(x){x = fifelse(x == 1,1,0)}))
  
  ### Handle categorical variables that do not require NA level
  df_personas_train = df_personas_train %>% 
    dummy_cols(c('ocupado_relab','ocupado_tamano_de_la_empresa','oficio',
                 'ocupado_segundo_trabajo_relab','desocupados_relab'),
               remove_selected_columns = TRUE,
               ignore_na = TRUE) 
  
  ### After cleaning the data, NA are marked as 0 
  df_personas_train = df_personas_train %>% 
    mutate(across(.cols = everything(),
                  .fns = function(x) replace_na(x,0))) 
  
}

### Apply processing
db_train_personas = f_process_personas(data = train_personas)
db_train_hogares = f_process_hogares(data = train_hogares)
db_test_personas = f_process_personas(data = test_personas)
db_test_hogares = f_process_hogares(data = test_hogares)
rm(train_personas,train_hogares,test_personas,test_hogares)
gc();gc();gc()

#------------------#
# 3. Collapse data #
#------------------#

f_collapse = function(personas,hogares){
  
  
  generalidades_hogar = personas %>% 
    group_by(id) %>% 
    summarise(across(.cols = c(edad,edad_sq,female,informal,nini,
                               regimen_salud_1,regimen_salud_2,regimen_salud_3,
                               maximo_nivel_educativo_1:maximo_nivel_educativo_6,
                               pet,oc,des,ina,
                               edad_menor_18,edad_menor_5,edad_mayor_65),.fns = list('household_average' = mean)))
  
  ocupados_hogar = personas %>% 
    filter(oc == 1) %>% 
    select(id,ocupado_tiempo_en_la_empresa,
           ocupado_recibio_pago_horas_extra,
           ocupado_recibio_primas,
           ocupado_pago_en_especie,
           ocupado_horas_trabajadas_normalmente,
           ocupado_segundo_trabajo,
           ocupado_segundo_trabajo_horas_trabajadas,
           ocupado_quiere_trabajar_mas_horas,
           ocupado_relab_1:ocupado_tamano_de_la_empresa_9) %>% 
    group_by(id) %>% 
    summarise(across(where(is.numeric),.fns = list('household_working' = mean)))
  
  jefe_de_hogar = personas %>% 
    filter(posicion_jefe_del_hogar == 1) %>% 
    select(id,edad,edad_sq,female,informal,nini,
           regimen_salud_1,regimen_salud_2,regimen_salud_3,
           maximo_nivel_educativo_1:maximo_nivel_educativo_6,
           ocupado_tiempo_en_la_empresa,
           ocupado_recibio_pago_horas_extra,
           ocupado_recibio_primas,
           ocupado_pago_en_especie,
           ocupado_horas_trabajadas_normalmente,
           ocupado_segundo_trabajo,
           ocupado_segundo_trabajo_horas_trabajadas,
           ocupado_quiere_trabajar_mas_horas,
           ocupado_relab_1:ocupado_tamano_de_la_empresa_9,
           pet,oc,des,ina,edad_mayor_65)  %>%
    rename_with(~ paste0("hh_", .x), .cols = -c(id))
  
  data = left_join(x = hogares,y = generalidades_hogar,by = c('id')) %>% 
    left_join(x = .,y = ocupados_hogar,by = c('id')) %>% 
    left_join(x = .,y = jefe_de_hogar,by = c('id'))  
  
  return(data)
  
  
  
}

db_train = f_collapse(personas = db_train_personas,
                      hogares = db_train_hogares) %>% 
           mutate(pobre = factor(pobre,levels = c('1','0')))

db_test = f_collapse(personas = db_test_personas,
                     hogares = db_test_hogares)

#----------------------------------------------------------------#
# 4. Remove covariates that are not available for submission set #
#----------------------------------------------------------------#

covariates_to_remove = setdiff(colnames(db_train %>% select(-pobre,-ingpcug)),
                               colnames(db_test))

db_train = db_train %>% 
           select(-any_of(covariates_to_remove))

db_train = db_train %>% 
           relocate(.after = id, any_of(c('lp','ingpcug')))

db_test = db_test %>% 
          relocate(.after = id, any_of(c('lp','ingpcug')))


#------------------------------#
# 5. Final cleaning covariates #
#------------------------------#

db_train = db_train %>% 
           select(-depto,-dominio,-id,-ingpcug) %>% 
           mutate(across(ends_with('_household_working'),.fns = function(x) replace_na(x,0)),
                  pobre = factor(pobre,levels = c('1','0'),labels = c('Yes','No')))

db_test = db_test %>%
          select(-dominio,-depto) %>% 
          mutate(across(ends_with('_household_working'),.fns = function(x) replace_na(x,0))) 

#----------------#
# 6. Export data #
#----------------#

export(db_train,'stores/output/02_wrangle/02_train.rds')
export(db_test,'stores/output/02_wrangle/02_test.rds')