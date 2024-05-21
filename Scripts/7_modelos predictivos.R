#### Modelos predictivos ####
########## Modelos predictivos ######

#Set directory
setwd(substr(getwd(), 1, nchar(getwd()) - 8))

#limpiar en entorno
rm(list = ls())


###Librerias
library(pacman)

p_load(tidyverse, # Manipular dataframes
       rio, # Importar datos fácilmente
       sf, # Leer/escribir/manipular datos espaciales
       tidymodels, # Modelado de datos limpios y ordenados
       rattle, # Interfaz gráfica para el modelado de datos
       spatialsample, # Muestreo espacial para modelos de aprendizaje automático
       ranger,
       rpart,
       xgboost) 

###Improtamos datos y los convertimos a sf
train<-import('Stores/outputs/train_modelos.rds')
test<-import('Stores/outputs/test_modelos.rds')

train<- st_as_sf(train)

###Alistamos lo folds

set.seed(10001)
block_folds <- spatial_block_cv(train, v = 5)
autoplot(block_folds)

## Eliminamos geometry 
train<- as.data.frame(train)
train<- train %>% select(-geometry,-nombre_sector)
test<- as.data.frame(test)
test<- test %>% select(-geometry)

### configuramos tidymodels (receta)

rec_1 <- recipe(ln_price ~ . , data = train) %>%
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

### creamos specificación

reg_lineal <- linear_reg() %>% set_engine("lm") 

### Configuramos flujo de trabajo

workflow_1 <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(reg_lineal)

### configuramos modelos
set.seed(10001)

tune_res1 <- tune_grid(
  workflow_1,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  metrics = metric_set(mae), # metrica
)

### ver resultado
collect_metrics(tune_res1)

### Mejor metrica (en este caso es lm entonces no hay hiperparametros)
best_tune<- select_best(tune_res1, metric = "mae")

###Finalizamos flujo
res1_final <- finalize_workflow(workflow_1, best_tune)

#Sacamos los coef (para el caso de regression)
reg_coef <- fit(res1_final, data = train)

## Creamos submiission 1
pred1_ln<-as.vector(predict(reg_coef,test))
pred1_ln<-pred1_ln[[1]]

sub1<- test %>% mutate(price=exp(pred1_ln))  %>% 
                mutate(price=round(price))  %>% 
                select(property_id, price)

###export submission
export(sub1, 'Stores/submits/reg_lineal.csv')


#####CART

##Config inicial
cart<-decision_tree(cost_complexity = tune()) %>% set_mode("regression") %>% set_engine(engine = "rpart")  

##workflow 2
workflow_2 <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(cart)

## grilla

grid_cart<- expand.grid(cost_complexity = seq(0, 0.05, 0.0005))

##Corremos modelos
tune_res2 <- tune_grid(
  workflow_2,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  metrics = metric_set(mae), # metrica
  grid=grid_cart)

##ver mejor modelos
best_tune2<- select_best(tune_res2, metric = "mae")
best_tune2

##finalizar flujo de trabajo
res_final2 <- finalize_workflow(workflow_2, best_tune2)

###3 capturamos los parametros
param2 <- fit(res_final2, data = train)

###Cremos submit 2

sub2<- test %>% mutate(ln_pred=predict(param2, test,type='raw'))  %>%
                mutate(price=exp(ln_pred))  %>%
                mutate(price=round(price)) %>%
                select(property_id, price)

export(sub2, 'Stores/submits/CART.csv')
                

#### random forest


##Config inicial
rf_model <- rand_forest(
  mtry = tune(), # Parámetro a ajustar
  trees = 500,
  min_n = tune()
) %>%
  set_engine("ranger", splitrule = "variance") %>%
  set_mode("regression")

##workflow 3
workflow_3 <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(rf_model)

## grilla


rf_grid <- grid_random(
  mtry(range = c(1, ncol(train) - 1)),
  min_n(range = c(2, 10)),
  size = 20  # Número de combinaciones aleatorias a generar
)

##Corremos modelos
tune_res3 <- tune_grid(
  workflow_3,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  metrics = metric_set(mae), # metrica
  grid=rf_grid )

##ver mejor modelos
best_tune3<- select_best(tune_res3, metric = "mae")
best_tune3

##finalizar flujo de trabajo
res_final3 <- finalize_workflow(workflow_3, best_tune3)

###3 capturamos los parametros
param3 <- fit(res_final3, data = train)

###Cremos submit 3

ln_pred3<-predict(param3, test,type='raw')

sub3<- test %>% mutate(ln_pred=ln_pred3[[1]])  %>%
  mutate(price=exp(ln_pred))  %>%
  mutate(price=round(price)) %>%
  select(property_id, price)

export(sub3, 'Stores/submits/randomforest.csv')


##### boosting


## config del modelo
xgb_model <- boost_tree(
  trees = tune(),          # nrounds
  tree_depth = tune(),     # max_depth
  learn_rate = tune(),     # eta
  loss_reduction = tune(), # gamma
  min_n = tune(),          # min_child_weight
  sample_size = tune(),    # subsample
  mtry = tune()            # colsample_bytree
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

##workflow 4
workflow_4 <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(xgb_model )

###grilla

xgb_grid <- expand.grid(
  trees = c(250, 500),           # nrounds
  tree_depth = c(1, 2),          # max_depth
  learn_rate = c(0.1, 0.01),     # eta
  loss_reduction = c(0, 1),      # gamma
  min_n = c(10, 25),             # min_child_weight
  sample_size = c(0.7),          # subsample
  mtry=c(5,10,20,30,40,50,60,74)         # colsample_bytree
)



##Corremos modelos
tune_res4 <- tune_grid(
  workflow_4,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  metrics = metric_set(mae), # metrica
  grid=xgb_grid )

##mejores parametros
best_tune4<- select_best(tune_res4, metric = "mae")
best_tune4

##finalizar flujo de trabajo
res_final4 <- finalize_workflow(workflow_4, best_tune4)

###3 capturamos los parametros
param4 <- fit(res_final4, data = train)



###ajustadmos submit
sub4<- test %>% mutate(ln_pred=predict(param4, test,type='raw'))  %>%
  mutate(price=exp(ln_pred))  %>%
  mutate(price=round(price)) %>%
  select(property_id, price)


export(sub4, 'Stores/submits/xgboosting.csv')
