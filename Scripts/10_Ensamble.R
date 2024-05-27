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
       glmnet)

# Importamos datos de entrenamiento y pruebas:
train = readRDS('Stores/outputs/train_modelos.rds')
test = readRDS('Stores/outputs/test_modelos.rds')


Train = read_csv('Stores/inputs/train.csv')

Train$ln_price = log(Train$price)

train <- st_as_sf(train)

###Alistamos lo folds
set.seed(10001)
block_folds <- spatial_block_cv(train, v = 5)
autoplot(block_folds)

## Eliminamos geometry 
train<- as.data.frame(train)
train<- train %>% dplyr::select(-geometry,-nombre_sector)
test<- as.data.frame(test)
test<- test %>%  dplyr::select(-geometry)

### configuramos tidymodels (receta)
rec_1 <- recipe(ln_price ~ . , data = train) %>%
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

# Excluimos la variable de resultado:
train_sin_price = train %>% select(-ln_price)
train_price = train %>% select(ln_price)
train_price$price = exp(train_price$ln_price)
# Creamos especificación de XGB:
XGBreg <- boost_tree(trees = 600,
                     tree_depth = 4, 
                     min_n = 25,
                     loss_reduction = 0,
                     sample_size = 1, 
                     mtry = 50,
                     learn_rate = .1) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

workflow <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(XGBreg)


xgb_final <- finalize_workflow(workflow, XGBreg)
xgb_final_fit <- fit(xgb_final, data = train)

# Creamos especificación de lightgbm:
lightgbm <- boost_tree(trees = 500,
                       tree_depth = 6, 
                       min_n = 30,
                       loss_reduction = 0,
                       mtry = 45,
                       learn_rate = .05) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

workflow <- workflow() %>% 
  # Agregar la receta de preprocesamiento de datos. En este caso la receta 1
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(lightgbm)

lightgbm_final <- finalize_workflow(workflow, lightgbm)
lightgbm_final_fit <- fit(lightgbm_final, data = train)

# Predecimos en la base de entrenamiento sin la Y:
predict_xgb <- train_sin_price %>% 
  mutate(lnprice = predict(xgb_final_fit, new_data = train_sin_price, type = "raw")    ## predicted class labels
  )  %>% select(lnprice)

predict_xgb = predict_xgb %>% 
  mutate(price = exp(lnprice)) %>% select(price)

xgb_round = predict_xgb %>% 
  mutate(price = round(price, -5)) %>% select(price)

predict_lightgbm <- train_sin_price %>% 
  mutate(lnprice = predict(lightgbm_final_fit, new_data = train_sin_price, type = 'numeric')    ## predicted class labels
  )  %>% select(lnprice)

predict_lightgbm = predict_lightgbm %>% 
  mutate(price = exp(lnprice)) %>% select(price)

lightgbm_round = predict_lightgbm %>% 
  mutate(price = round(price, -5)) %>% select(price)
lightgbm_round$price = unlist(lightgbm_round$price)

# Predecimos en la base de testeo:
test_xgb <- test %>% 
  mutate(lnprice = predict(xgb_final_fit, new_data = test, type = "raw")    ## predicted class labels
  )  %>% select(property_id, lnprice)

test_xgb = test_xgb %>% 
  mutate(price = exp(lnprice)) %>% select(property_id,price)

xgb_round_test = test_xgb %>% 
  mutate(price = round(price, -5)) %>% select(property_id,price)

test_lightgbm <- test %>% 
  mutate(lnprice = predict(lightgbm_final_fit, new_data = test, type = 'numeric')    ## predicted class labels
  )  %>% select(property_id,lnprice)

test_lightgbm = test_lightgbm %>% 
  mutate(price = exp(lnprice)) %>% select(property_id,price)

lightgbm_round_test = test_lightgbm %>% 
  mutate(price = round(price, -5)) %>% select(property_id,price)
lightgbm_round_test$price = unlist(lightgbm_round_test$price)

GBM_test = xgb_round_test %>% left_join(lightgbm_round_test, by = c('property_id' = 'property_id')) 
colnames(GBM_test) = c('property_id', 'xgb', 'lightgbm')

# Creamos la combinación lineal (Stacking):
GBM = cbind(train_price$price, xgb_round, lightgbm_round) 
colnames(GBM) = c('price', 'xgb', 'lightgbm')

# Predecimos:
validation_split <- vfold_cv(GBM, v = 5, strata = price)

tune_spec <- linear_reg(penalty = tune(),
                        mixture = tune()) %>%
  set_engine("glmnet")

grid = grid_regular(penalty(), 
                    mixture(),
                    levels = list(penalty = 100,
                                  mixture = 10))

### Configuramos flujo de trabajo

workflow <- workflow() %>% 
  add_model(tune_spec) %>%
  add_formula(price ~ .)

tune_res <- workflow %>% 
  tune_grid(validation_split,
            grid = grid,
            metrics = metric_set(mae))

collect_metrics(tune_res)

### Mejor metrica 
best_tune<- select_best(tune_res, metric = "mae")

###Finalizamos flujo
res_final <- finalize_workflow(workflow, best_tune)

#Sacamos los coef (para el caso de regression)
reg_coef <- fit(res_final, data = GBM)

## Creamos submiission 1
pred_ln<-as.vector(predict(reg_coef,GBM_test))
pred_ln<-pred_ln[[1]]

sub <- GBM_test %>% mutate(price=pred_ln)  %>% 
  mutate(price=round(price, -5))  %>% 
  select(property_id, price)

export(sub, 'ensamble_xgb_lightgbm_EN.csv')
export(GBM, 'ensamble_train.csv')
export(GBM_test, 'ensamble_test.csv')



######Creamos tabla de ultima seccion del documento

test_fold1<-anti_join(train, a, by = "property_id")
