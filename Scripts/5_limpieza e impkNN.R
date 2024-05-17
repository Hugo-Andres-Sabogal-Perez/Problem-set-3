###Limpieza e imputación por KNN

#directorio
setwd(substr(getwd(), 1, nchar(getwd()) - 8))

#limpiar en entorno
rm(list = ls())


#Librerias
require(pacman)
p_load(tidyverse,
       rio, 
       sf,
       VIM)


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


###Manipulas train_geo para que sea posible el pegue
ubi_test<-test_geo %>% select(property_id, geometry)
test_geo<-test_geo %>% select(-geometry) 


####Realiamos join de train
train<-left_join(train_geo, train_text, by='property_id')
train<-left_join(train, train_area, by='property_id')


####Realiamos join de test
test<-left_join(test_geo, test_text, by='property_id')
test<-left_join(test, test_area, by='property_id')

###limpiamos consola de R
list <- ls()
list <- list[!(list %in% c("train", "test", 'ubi_train', 'ubi_test'))]
rm(list = list)
rm(list)


### Quitamos area, surface, bathrooms (nos interesa area_num,n_baños)

train<-train %>% select(-area, -bathrooms, -surface_covered, -surface_total)
test<-test %>% select(-area, -bathrooms, -surface_covered, -surface_total)

### Calculamos el precio por metro cuadrado (solo se puede en train)

train<-train %>% mutate(pmetro_sqr=price/area_num)

#### Eliminamos precio del metro mayor a 50000 millones y tambien aptos area>1200 y casas area>2000
train<-train %>% subset(pmetro_sqr <= 50000000 | is.na(pmetro_sqr))  %>% 
                 subset(!(area_num > 1200 & property_type == "Apartamento")| is.na(area_num)) %>% 
                 subset(!(area_num > 2000 & property_type == "Casa")| is.na(area_num)) %>%
                 subset(!(area_num < 15 & property_type == "Apartamento")| is.na(area_num)) %>%
                 subset(!(area_num < 25 & property_type == "Casa")| is.na(area_num)) %>%
                 subset(!(n_baños > 10 | is.na(n_baños)))

#### asignamos missinsg para el mismo caso en test (es preferible la imputacion por KNN)

test$area_num[test$area_num > 2000 & test$property_type == "Apartamento"] <- NA
test$area_num[test$area_num > 6000 & test$property_type == "Casa"] <- NA

## Regresamos a missing posbiles valores mal imputados

test$n_baños[test$n_baños > 13 ] <- NA

##Realizamos intersección  de variables (solo se pierde 1 variable)

test<- test %>% select(intersect(names(train), names(test)))
train<- train %>% select(intersect(names(train), names(test)))


### Convertimos categoricas y dummys en factores
for (i in 35:80) {
  train[[i]] <- as.factor(train[[i]])
}

for (i in 35:80) {
  test[[i]] <- as.factor(test[[i]])
}

#Convertimos año y mes en factor (dado que no es serie de tiempo se tomara como categoria)

train$year<- as.factor(train$year)
train$month<- as.factor(train$month)
test$year<- as.factor(test$year)
test$month<- as.factor(test$month)

## Borramos price para test

test<- test %>% select(-price)


## Exportamos posibles bases para estadisticas descriptivas
export(train, "Stores/outputs/train_estat_desc.rds")
export(test, "Stores/outputs/test_estat_desc.rds")


#### Imputación por KNN (este codigo se demora demasiado, se demora varios minutos 1 hora aprox)
train_kimp<-kNN(train)
test_imp<-kNN(test)

train_kimp<-train_kimp[,1:82]
test_kimp<-test_imp[,1:81]


export(train_kimp, "Stores/outputs/train_kimp.rds")
export(test_kimp, "Stores/outputs/train_kimp.rds")


##### dejar lat y lon en las bases


#limpiar en entorno
rm(list = ls())

train_kimp<-import('Stores/outputs/train_kimp.rds')
test_kimp<-import('Stores/outputs/test_kimp.rds')


#### recuperamos ubicaciípn

train_ori<-import('Stores/inputs/train.csv')
test_ori<-import('Stores/inputs/test.csv')


train_ori<-train_ori %>% select(property_id, lat, lon)
test_ori<-test_ori %>% select(property_id, lat, lon)

train_kimp<-left_join(train_kimp,train_ori , by='property_id')
test_kimp<-left_join(test_kimp,test_ori , by='property_id')

export(train_kimp, "Stores/outputs/train_kimp.rds")
export(test_kimp, "Stores/outputs/train_kimp.rds")

