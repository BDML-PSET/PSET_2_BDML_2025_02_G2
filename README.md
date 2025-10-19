# Predicción de Pobreza en Colombia (2018)

Este proyecto tiene como objetivo predecir la condición de pobreza de los hogares en Colombia para el año 2018, utilizando información proveniente de hogares y personas. El flujo de trabajo se estructura en tres etapas principales: limpieza, modelamiento y predicción, así como generación de visualizaciones. Cada etapa se encuentra organizada en carpetas diferenciadas y sigue convenciones de nombres específicas.

<!---------------------------------------------------------------------->
<!---------------------------------------------------------------------->
<!---------------------------------------------------------------------->
## scripts
Esta carpeta contiene cuatro (4) subcarpetas que almacenan los códigos necesarios en cada tarea, a continuación se ennuncia cada una: 

📁01_import: el código contenido en esta carpeta importa los datos crudos del train y test para hogares y personas y hace la selección común de variables. 


📁02_wrangle: el código contenido en esta carpeta realiza la selección de variables, la recodificación y limpieza de datos.


📁03_model : los códigos contenidos en esta carpeta realizan los diferentes modelos de predicción utilizados en la competencia: Logit, Logit EN, XGboost y Random Forest. 


📁04_visual: los códigos contenidos en esta carpeta realizan las tablas, gráficos y en general objetos visuales necesarios para el documento. 
<!---------------------------------------------------------------------->
<!---------------------------------------------------------------------->
<!---------------------------------------------------------------------->
## stores
Esta carpeta contiene dos (2) carpetas principales: 


📁input: en esta carpeta se almacenan los datos inciales halados del reposorio de Igancio Sarmiento en formato csv y la documentaión de variables. 


📁output: esta carpeta contiene cuatro (4) subcarpetas que almacenan los resultados de los códigos de cada una de las tareas ejecutadas en la carpte scripts. 


      ----📁01_import: contiene los datos listos para el procesamiento.

      
      ----📁02_wrangle: el código contenido en esta carpeta realiza la selección de variables, la recodificación y limpieza de datos.

      
      ----📁03_model : contiene los modelos en formarto .rds y las predicciones de cada modelo en formato csv.

      
      ----📁04_visual: contiene las tablas en formato overleaf y gráficas.



<!---------------------------------------------------------------------->
<!---------------------------------------------------------------------->
