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
XGBreg <- boost_tree(trees = tune(),
                     tree_depth = tune(), 
                     min_n = 25,
                     loss_reduction = 0,
                     sample_size = tune(), 
                     mtry = tune(),
                     learn_rate = .1) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

grid <- expand.grid(trees = c(400, 500, 600),
                    tree_depth = c(4, 6, 8),
                    sample_size = c(.8, .9, 1),
                    mtry = c(26, 38, 50))

### Configuramos flujo de trabajo

workflow <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(XGBreg)

### configuramos modelos
set.seed(10001)

tune_xgb <- tune_grid(
  workflow,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  metrics = metric_set(mae),
  grid = grid)

### ver resultado
collect_metrics(tune_xgb)

### Mejor metrica 
best_tune<- select_best(tune_xgb, metric = "mae")

###Finalizamos flujo
xgb_final <- finalize_workflow(workflow, best_tune)
xgb_final_fit <- fit(xgb_final, data = train)

# Predicciones:
predict_xgb <- test %>% 
  mutate(lnprice = predict(xgb_final_fit, new_data = test, type = "raw")    ## predicted class labels
  )  %>% select(property_id,lnprice)

# Retornar el precio:
predict_xgb = predict_xgb %>% 
  mutate(price = exp(lnprice)) %>% select(property_id,price)

###export submission
export(predict_xgb, 'XGBreg.csv')

# redondear el precio:
xgb_round = predict_xgb %>% 
  mutate(price = round(price, -5)) %>% select(property_id,price)

export(xgb_round, 'XGBreg_round.csv')

