rm(list = ls())

setwd('/Users/juansilva/Documents/GitHub/Problem-set-3/Stores/outputs')

# Librerias:
require(rio)
require(tidyverse)
require(sf)
require(leaflet)
require(osmdata)
require(raster)
require(stringr)
require(tidymodels)
require(spatialsample)
require(lightgbm)
require(bonsai)

# Importamos datos de entrenamiento y pruebas:
train = readRDS('train_modelos.rds')
test = readRDS('test_modelos.rds')

train <- st_as_sf(train)

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
lightgbm <- boost_tree(trees = tune(),
                     tree_depth = 7, 
                     min_n = tune(),
                     loss_reduction = 0,
                     mtry = tune(),
                     learn_rate = .05) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

grid <- expand.grid(trees = c(570, 572, 574, 576, 580, 582, 584, 586, 588),
                    min_n = c(26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39),
                    mtry = c(31, 32))

### Configuramos flujo de trabajo

workflow <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(lightgbm)

### configuramos modelos
set.seed(10001)

tune_lightgbm <- tune_grid(
  workflow,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  metrics = metric_set(mae),
  grid = grid)

### ver resultado
collect_metrics(tune_lightgbm)

### Mejor metrica 
best_tune<- select_best(tune_lightgbm, metric = "mae")

###Finalizamos flujo
lightgbm_final <- finalize_workflow(workflow, best_tune)
lightgbm_final_fit <- fit(lightgbm_final, data = train)

# Predicciones:
predict_lightgbm <- test %>% 
  mutate(lnprice = predict(lightgbm_final_fit, new_data = test, type = 'numeric')    ## predicted class labels
  )  %>% select(property_id,lnprice)

# Retornar el precio:
predict_lightgbm = predict_lightgbm %>% 
  mutate(price = exp(lnprice)) %>% select(property_id,price)

# redondear el precio:
lightgbm_floor = predict_lightgbm %>% 
  mutate(price = floor(price/100000)*100000) %>% select(property_id,price)
lightgbm_round = predict_lightgbm %>% 
  mutate(price = round(price, -5)) %>% select(property_id,price)
lightgbm_round$price = unlist(lightgbm_round$price)
lightgbm_floor$price = unlist(lightgbm_floor$price)

export(lightgbm_round, 'lightgbm_round_3.csv')
export(lightgbm_floor, 'lightgbm_floor_3.csv')
