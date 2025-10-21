### setup
cat("\f")
rm(list = ls())
source('scripts/00_packages.R')

#--------------#
# 1. Load data #
#--------------#
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

train_personas =  kaggle_import("kaggle.json", "train_personas.csv") |> clean_names()
train_hogares = kaggle_import("kaggle.json", "train_hogares.csv") |> clean_names() 

test_personas = kaggle_import("kaggle.json", "test_personas.csv") |> clean_names()
test_hogares = kaggle_import("kaggle.json", "test_hogares.csv") |> clean_names()

#----------------#
# 4. Export data #
#----------------#

export(train_personas,'stores/output/01_import/01_train_personas.rds')
export(train_hogares,'stores/output/01_import/01_train_hogares.rds')
export(test_personas,'stores/output/01_import/01_test_personas.rds')
export(test_hogares,'stores/output/01_import/01_test_hogares.rds')