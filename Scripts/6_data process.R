########## Modelos predictivos ######

#Set directory
setwd(substr(getwd(), 1, nchar(getwd()) - 8))

#limpiar en entorno
rm(list = ls())


###Librerias
library(pacman)
p_load(rio,
       tidymodels,
       sf,
       purrr)

### importamos datos

train<-import('Stores/outputs/train_kimp.rds')
test<-import('Stores/outputs/test_kimp.rds')

### Manejamos la variable en logaritmo

train$ln_price<-log(train$price)

##Eliminamos price y id
train <- train %>% select(-property_id, -price, -city)

## Creamos estrato como factor para train 
train$ESTRATO<-as.factor(train$ESTRATO)
train$localidad<-as.factor(train$localidad)
train$property_type<-as.factor(train$property_type)
train$operation_type<-as.factor(train$operation_type)
train$nombre_sector<-as.factor(train$nombre_sector)

## Creamos estrato como factor para test
test$ESTRATO<-as.factor(test$ESTRATO)
test$localidad<-as.factor(test$localidad)
test$property_type<-as.factor(test$property_type)
test$operation_type<-as.factor(test$operation_type)
test$nombre_sector<-as.factor(test$nombre_sector)

# Función para verificar si un factor tiene solo un nivel
tiene_un_nivel <- function(factor_var) {
  if (is.factor(factor_var)) {
    nlevels(factor_var) == 1
  } else {
    FALSE
  }
}

# Filtrar las columnas con un solo nivel
columnas_a_eliminar <- names(train)[sapply(train, tiene_un_nivel)]

# Eliminar las columnas con un solo nivel del dataframe
train <- train[, !names(train) %in% columnas_a_eliminar]


### Convertimos la variable train  y test en objeto sf

train<- st_as_sf(train,coords = c("lon", "lat"),crs = 4326)
test<- st_as_sf(test,coords = c("lon", "lat"),crs = 4326)

export(train, 'Stores/outputs/train_modelos.rds')
export(test, 'Stores/outputs/test_modelos.rds')

