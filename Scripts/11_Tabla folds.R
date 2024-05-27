### Set directory
setwd(substr(getwd(), 1, nchar(getwd()) - 8))

##Limpiar entorno
rm(list = ls())
# Librerias:

require(pacman)

p_load(rio,
       tidyverse,
       spatialsample,
       tidymodels,
       sf,
       bonsai,
       lightgbm, 
       raster,
       stringr,
       nnet, 
       glmnet,
       yardstick)

###Importamos datos, 
train_estat <- import('Stores/outputs/train_estat_desc.rds')
train_estat <- train_estat %>% dplyr::select(property_id, price)

##Llamamamoosl base original para obtener la ubicacion
train_original<- import('Stores/inputs/train.csv')
train_original <-  train_original  %>% dplyr::select(property_id, lat, lon)

### Hacemos left_join

train<-left_join(train_estat, train_original, by='property_id')

### CReamos objeto sf
train<-st_as_sf(train, coords = c("lon", "lat"), crs = 4326)

##Creamos folds
set.seed(10001)
block_folds <- spatial_block_cv(train, v = 5)
autoplot(block_folds)

##Creamos folds (del 1 a 5)
fold1<-as.tibble(block_folds[[1]][[1]])
fold2<-as.tibble(block_folds[[1]][[2]])
fold3<-as.tibble(block_folds[[1]][[3]])
fold4<-as.tibble(block_folds[[1]][[4]])
fold5<-as.tibble(block_folds[[1]][[5]])

##quitamos geometry
fold1<-fold1 %>% dplyr::select(-geometry)
fold2<-fold2 %>% dplyr::select(-geometry)
fold3<-fold3 %>% dplyr::select(-geometry)
fold4<-fold4 %>% dplyr::select(-geometry)
fold5<-fold5 %>% dplyr::select(-geometry)


###Convertimos train en tibble
train<- as.tibble(train)
train<-train %>% dplyr::select(-geometry)

###Creamos test fold
testf1<- anti_join(train, fold1)
testf2 <- anti_join(train, fold2)
testf3 <-  anti_join(train, fold3)
testf4 <-  anti_join(train, fold4)
testf5 <-  anti_join(train, fold5)

####

base_bestmodel<-import('Stores/inputs/ensamble_train.csv')
base_bestmodel$property_id<-train$property_id
  
###hiperparametros ya entrendaos
alpha_value <- 0.111
lambda_value <- 1e-10

##Ajustamos datos
x_train<- base_bestmodel %>% dplyr::select(-property_id, -price)
y_train<- base_bestmodel %>% dplyr::select(price)

##Convertimos en matrices
y_train<-as.matrix(y_train)
x_train<-as.matrix(x_train)

##Ajustamos modelo
elastic_net_model <- glmnet(x_train, y_train, alpha=alpha_value, lambda=lambda_value)

### Predecimos dentro de muestra

pred<-as.vector(predict(elastic_net_model,s=lambda_value, newx=x_train))

base_bestmodel$property_id<-train$property_id
base_bestmodel$pred_price<-pred


## Alistamos DF               
pred_train<- as.data.frame(base_bestmodel)
pred_train<-pred_train%>% dplyr::select(property_id, price, pred_price) 
pred_train$pred_price<-round(pred_train$pred_price, -6)

### Ahora recuperemos los folds
testf1<-left_join(testf1, pred_train,by='property_id')
testf2<-left_join(testf2, pred_train,by='property_id')
testf3<-left_join(testf3, pred_train,by='property_id')
testf4<-left_join(testf4, pred_train,by='property_id')
testf5<-left_join(testf5, pred_train,by='property_id')


### creamos una variable que sea la diferencia
testf1$error<- testf1$price.x-testf1$pred_price.x
testf2$error<- testf2$price.x-testf2$pred_price
testf3$error<- testf3$price.x-testf3$pred_price
testf4$error<- testf4$price.x-testf4$pred_price
testf5$error<- testf5$price.x-testf5$pred_price

### Sacamos las veces que estiam por encima como %
count(testf1[testf1$error>=0,])/nrow(testf1)
count(testf2[testf2$error>=0,])/nrow(testf2)
count(testf3[testf3$error>=0,])/nrow(testf3)
count(testf4[testf4$error>=0,])/nrow(testf4)
count(testf5[testf5$error>=0,])/nrow(testf5)


### Calculamos MAE
mean(abs(testf1$error))
mean(abs(testf2$error))
mean(abs(testf3$error))
mean(abs(testf4$error))
mean(abs(testf5$error))

