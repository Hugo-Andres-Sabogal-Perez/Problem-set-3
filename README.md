# Problem-set-3. Big Data & Machine Learning

Maria Paula Osuna (202021732), Hugo Sabogal (202013538), Juan Andres Silva (201923201) y Gabriela Pérez (202013115).

Este repositorio está diseñado para resolver el enunciado del problem set 3 del curso "Big Data and Machine Learning". El propósito principal es desarrollar algoritmos predictivos con el objetivo de predecir los precios de las propiedades en Bogotá. A continuación, se detalla la estructura y el contenido del repositorio. 



## Carpetas 
Nuestro Repositorio contiene la siguientes carpetas: 
- *Scripts*: incluye scripts de manipulación de datos, división bases de entreno y testeo, text mining, imputación de datos, estadísticas descriptivas y modelos predictivos. 
- *Stores*: contiene los archivos con los que trabajamos. Está dividido en carpeta de Inputs, Outputs y Submits.
- *Views*: contiene todas los archivos de tablas y gráficos que fueron utilizados dentro del documento final.
- *Documents*: contiene el documento final del taller, luego de haberlo realizado en LaTeX.


## Replicación del trabajo.
Este codigo consis de varios scripts, debido a que se dividio el trabajo en subproblemas que se trataron en diferentes documentos. Para replicar el trabajo se recomienda ejecuar el script `Main.R` el cual ejecuta todos los scripts en un orden determinado. A continuación se describira brevemente el contenido de los scripts.

* `0_preprocess espacial.R`: Este script se encarga de añadir variables geograficas a los conjuntos de datos de entranamiento y prueba. Es posible que la ejecución de este script dependa de la disponibilidad de la pagina web: https://datosabiertos.bogota.gov.co/ debido a que algunas variables son descargadas directamente a traves de enlaces de la web.
* `1_imp_area_test` y `2_imp_area_train`: Estos scripts tratan de imputar valores faltantes para las variables de Arae y numero de baños.
* `3_Text mining test` y `4_Text mining train`: Obtienen variables a partir de las descripción de los inmuebles a traves de expresiones regulares y herramientas de texto.
* `5_limpieza e impkNN` y `6_data process`: Realizan diferentes tipos de limpieza de datos para preparar el conjunto de datos para los modelos.
* `7_modelos predictivos HS`, `9_LightGBM`, `9_LightGBM` y `12_XGB`: Ejecutan el codigo de diferentes modelos predictivos y realizan predicciones.
* `8_estadisticas_descriptivas` y `11_Tabla folds`: Realizan diferentes tablas y graficos fundamentales para desarollo del documento-
* `Main.R`: Ejecuta todos los scripts anteriormente mencionados.


