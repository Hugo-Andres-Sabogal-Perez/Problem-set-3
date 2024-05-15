###Limpieza e imputación por KNN

#directorio
setwd(substr(getwd(), 1, nchar(getwd()) - 8))

#limpiar en entorno
rm(list = ls())


#Librerias
require(pacman)
p_load(tidyverse,
       rio, 
       sf)


## Importacion de los datos de training
train_text<-import('Stores/outputs/text_mining_train.csv')
train_area<-import('Stores/outputs/train_imp_area.rds')
train_geo<-import('Stores/outputs/train_geo.rds')

###Manipulas train_geo para que sea posible el pegue
ubi_train<-train_geo %>% select(property_id, geometry)
train_geo<-train_geo %>% select(-geometry) 

## Importacion de los datos de testing
test_text<-import('Stores/outputs/text_mining_test.csv')
test_area<-import('Stores/outputs/test_imp_area.rds')
test_geo<-import('Stores/outputs/test_geo.rds')
test_geo<-as.data.frame(test_geo)

###Manipulas train_geo para que sea posible el pegue
ubi_test<-test_geo %>% select(property_id, geometry)
test_geo<-test_geo %>% select(-geometry) 


####Realiamos join de train
train<-left_join(train_geo, train_text, by='property_id')
train<-left_join(train, train_area, by='property_id')


###Miramos algo



