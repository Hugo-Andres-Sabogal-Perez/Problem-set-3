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
       rpart) 

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
cart<-decision_tree(mode = "regression") %>%set_engine(engine = "rpart") 

##workflow 2
workflow_2 <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(cart)

## grilla

grid_cart<- expand.grid(cp = seq(0, 0.03, 0.00001))

##Corremos modelos
tune_res1 <- tune_grid(
  workflow_2,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  metrics = metric_set(mae), # metrica
  grid=grid_cart)

predict8 <- test_H   %>% 
  mutate(ln_ing_lab = predict(xb_forest, newdata = test_H, type = "raw")    ## predicted class labels
  )  %>% select(id,ln_ing_lab, lnlptest)
