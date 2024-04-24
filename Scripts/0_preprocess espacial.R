#Script manipulacion de datos de porblem set 3

#FIjamos directorio
setwd(substr(getwd(), 1, nchar(getwd()) - 8))

#limpiar entorno
rm(list = ls())


#librerias
library(rio)
library(tidyverse)
require(sf)
require(leaflet)


##importamos datos de entrenamiento y pruebas
train_su<-import('Stores/inputs/train.csv')
test_su<-import('Stores/inputs/test.csv')

#Importamos información adicional (estrato, ... )
manzanas_estr<-st_read("Stores/inputs/manz_estrato/ManzanaEstratificacion.shp")
localidades<- st_read("Stores/inputs/loca/Loca.shp")

#Convertirmos los dataframe a tipo de datos espacial (sf)
convert_dfsf<-function(datos){
  
  datos_sf <- st_as_sf(datos, coords = c("lon", "lat"), crs = 4326)
  
  return(datos_sf)
  
}

train_sf<-convert_dfsf(train_su)
test_sf<-convert_dfsf(test_su)

## Asegurarnos de que tenga el mismo sistema de cordanas
manzanas_estr <- st_transform(manzanas_estr, st_crs(train_sf))

###Asignamos estrato 
sf_use_s2(FALSE) ### Linea necesaria pára correr el codigo

st_nearest_feature(train_sf, manzanas_estr)

train_sf<-st_join(train_sf, manzanas_estr, join = st_nearest_feature)
test_sf<-st_join(test_sf, manzanas_estr, join = st_nearest_feature)


##Ejemmplo con la oobservacion 3
leaflet() %>%
  addTiles() %>%
  addCircles(data = train_sf[1,])  %>% 
  addPolygons(data = manzanas_estr[3408,])

##Ejemmplo con la oobservacion 3
leaflet() %>%
  addTiles() %>%
  addCircles(data = train_sf)  

#Asignamos localidad

#agisnamos mismo sistema de cordenadas
localidades <- st_transform(localidades, st_crs(train_sf))
with<-st_within(x=train_sf,y=localidades)

train_sf<-st_join(train_sf, localidades, join = st_within)
test_sf<-st_join(test_sf, localidades, join = st_within)


