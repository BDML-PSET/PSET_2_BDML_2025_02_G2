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

test_hogar = kaggle_import("kaggle.json", "test_hogares.csv") |> clean_names()
train_hogar = kaggle_import("kaggle.json", "train_hogares.csv") |> clean_names()

rm(kaggle_import)

#===========================#
# 2. clean data hogar ----
#===========================#

#----------------------#
# 2.1. train_hogar ----
train_hogar = train_hogar %>%
               rename(# personas, 
                      n_per              = nper , 
                      n_per_unidad_gasto = npersug, 
                      n_per_pobre        = npobres,
                      n_per_indigentes   = nindigentes, 

                      # ingresos,
                      ing_unidad_gasto                     = ingtotug , 
                      ing_unidad_gasto_imputado            = ingtotugarr,
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
                      ing_unidad_gasto, ing_unidad_gasto_imputado, ing_unidad_gasto_imputado_per_capita,
                      linea_pobreza, pobre, n_per_pobre, 
                      linea_indigencia, indigente, n_per_indigentes, everything()) 

train_hogar = train_hogar |>  
              mutate(pobre = ifelse(pobre == 1, yes = "Si", "No"), 
                     indigente = ifelse(indigente == 1, yes = "Si", "No"), 
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

#=====================#
# 3. Export data ----
#=====================#
export(train_hogar, "stores/A_procesamiento/01_train_hogar.rds")
export(test_hogar, "stores/A_procesamiento/01_test_hogar.rds")