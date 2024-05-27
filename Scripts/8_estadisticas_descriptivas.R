### Set directory
setwd(substr(getwd(), 1, nchar(getwd()) - 8))

##Limpiar entorno
rm(list = ls())


###llamamos librerias
library(pacman)
p_load(rio,
       tidyverse,
       sf,
       tidytable,
       xtable,
       leaflet,
       osmdata,
       ggsn, 
       cowplot)


###Iimportamos datos
test_estat_des<-import('Stores/outputs/test_estat_desc.rds')
train_estat_des<- import("Stores/outputs/train_estat_desc.rds")

summary(train_estat_des)
sd(train_estat_des$price)

#Tabla 1: estadisticas descriptivas 

tabla <- data.frame(train_estat_des$rooms, train_estat_des$bedrooms, train_estat_des$n_baños, train_estat_des$n_parqueaderos, train_estat_des$ESTRATO, train_estat_des$avaluo_catastromanz, train_estat_des$pisos,train_estat_des$area_num) 
vars <- length(colnames(tabla))
EstDesc <- data.frame(
  "Variable" = colnames(tabla), "Observaciones" = rep(NA, vars), "Media" = rep(NA, vars),
  "Desviacion_Estandar" = rep(NA, vars), "Min" = rep(NA, vars), "Max" = rep(NA, vars))

for (col in colnames(tabla)) {
  df <- tabla %>% select(col)
  Obs <- nrow(df)
  mean <- mean(as.numeric(unlist(df)), na.rm = T)
  sd <- sqrt(var(df, na.rm = T))
  min <- min(as.numeric(unlist(df)), na.rm = T)
  max <- max(as.numeric(unlist(df)), na.rm = T)
  
  EstDesc[EstDesc$Variable == col, 2] <- Obs
  EstDesc[EstDesc$Variable == col, 3] <- mean
  EstDesc[EstDesc$Variable == col, 4] <- sd
  EstDesc[EstDesc$Variable == col, 5] <- min
  EstDesc[EstDesc$Variable == col, 6] <- max
}

require(xtable)
tabla_estadisticas <- xtable(EstDesc, type = "latex")
print(tabla_estadisticas, file = "Views/estadisticas.tex")

#Tabla 2: Estadisticas de la variable de interés
mean_value <- mean(train_estat_des$price)
sd_value <- sd(train_estat_des$price)
min_value <- min(train_estat_des$price)
max_value <- max(train_estat_des$price)

# Crear una tabla con las estadísticas descriptivas
descriptive_stats_basic <- data.frame(
  mean = mean_value,
  sd = sd_value,
  min = min_value,
  max = max_value
)
latex_table <- xtable(descriptive_stats_basic)
print(latex_table, type = "latex", file = "Views/descriptive_stats.tex", include.rownames = FALSE)

#Histograma de la variable de interés
#Logaritmo
train_estat_des$ln_precio <- log(train_estat_des$price) 

histograma_precio <- ggplot(train_estat_des, aes(x = ln_precio)) +
  geom_histogram(color = "lightsalmon", fill = "lightsalmon") +
  xlab("Precio(log)") +
  ylab("Frecuencia") +
  theme_bw()
histograma_precio

ggsave("Views/histograma1.pdf", width = 6, height = 4, plot = histograma_precio)

#Normal
histograma_precio2 <- ggplot(train_estat_des, aes(x = price)) +
  geom_histogram(color = "lightsalmon", fill = "lightsalmon") +
  xlab("Precio") +
  ylab("Frecuencia") +
  theme_bw()
histograma_precio2

ggsave("Views/histograma2.pdf", width = 6, height = 4, plot = histograma_precio2)

#Gráficas de Dispersión

dispersion1 <- ggplot(train_estat_des, aes(x = distcc_nearest, y = ln_precio)) +
  geom_point(color = "slategray1") +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue4") +
  xlab("Distancia al cc más cercano") +
  ylab("Precio del inmueble")
dispersion1
ggsave("Views/dispersion1.pdf", width = 6, height = 4, plot = dispersion1)

dispersion2 <- ggplot(train_estat_des, aes(x = distpark_nearest, y = ln_precio)) +
  geom_point(color = "slategray1") +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue4") +
  xlab("Distancia al parque más cercano") +
  ylab("Precio del inmueble")
dispersion2
ggsave("Views/dispersion2.pdf", width = 6, height = 4, plot = dispersion2)

dispersion3 <- ggplot(train_estat_des, aes(x = distrestbar_nearest, y = ln_precio)) +
  geom_point(color = "slategray1") +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue4") +
  xlab("Distancia al restaurante/Bar más cercano") +
  ylab("Precio del inmueble")
dispersion3
ggsave("Views/dispersion3.pdf", width = 6, height = 4, plot = dispersion3)

dispersion4 <- ggplot(train_estat_des, aes(x = distciclo_nearest, y = ln_precio)) +
  geom_point(color = "slategray1") +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue4") +
  xlab("Distancia a la ciclo vía más cercana") +
  ylab("Precio del inmueble")
dispersion4
ggsave("Views/dispersion4.pdf", width = 6, height = 4, plot = dispersion4)


###Creamos matriz de scatter plots

scatter_comb<-plot_grid(dispersion1,
             dispersion2, 
             dispersion3, 
             dispersion4, 
             ncol = 2,
             rel_widths = c(5,5))

scatter_comb

ggsave("Views/scatter_conjunto.pdf", width = 6, height = 4, plot = scatter_comb)



#Tabla de otras características
table(train_estat_des$balcon)
table(train_estat_des$bbq)
table(train_estat_des$deposito)
table(train_estat_des$ascensor)
table(train_estat_des$patio)



####Mapas

#### Sacamos vias de bogota de osmdata (esta linea es demorada, ya no es necesario ejecutarla)
streets_primary<- opq(bbox=getbb('Bogotá Colombia')) %>% 
  add_osm_feature(key='highway', value='primary') %>%
  osmdata_sf()

streets_primary<-streets_primary$osm_lines


streets_trunk<- opq(bbox=getbb('Bogotá Colombia')) %>% 
  add_osm_feature(key='highway', value='trunk') %>%
  osmdata_sf()

streets_trunk<-streets_trunk$osm_lines

streets_secondary<- opq(bbox=getbb('Bogotá Colombia')) %>% 
  add_osm_feature(key='highway', value='secondary') %>%
  osmdata_sf()

streets_secondary<-streets_secondary$osm_lines



### Sacamos limite de bogota
bogota_lim<-st_read('Stores/inputs/loca/Loca.shp')
bogota_lim <- st_transform(bogota_lim, crs = 4326)


#Mapas de estaciones de transmilenio y SITP
transmi <- st_read('Stores/inputs/transmi/estaciones-de-transmilenio.shp')
transmi <- st_transform(transmi, crs = 4326)
sitp<-st_read("Stores/inputs/psitp/PSITP.shp")
sitp<-st_transform(sitp, crs = 4326)



##Mapa con ggplot (mapa 1 transmi)
map1 <- ggplot() +
  geom_sf(data=bogota_lim, fill=NA, size=0.8,color='orange')+
  geom_sf(data = streets_primary, fill = NA, size = 0.6, col='gray') +
  geom_sf(data = streets_trunk, fill = NA, size = 0.6, col='gray') +
  geom_sf(data = streets_secondary, fill = NA, size = 0.6, col='gray')+
  geom_sf(data = transmi, fill = NA, size = 0.8, col='red')+
  coord_sf(xlim=c(-74.2,-74.03), ylim=c(4.57, 4.8)) +
  theme_bw()

map1

ggsave("Views/mapatransmi.pdf", width = 6, height = 4, plot = map1)


##Mapa con ggplot (mapa 2 sitp)


map2 <- ggplot() +
  geom_sf(data=bogota_lim, fill=NA, size=0.8,color='orange')+
  geom_sf(data = streets_primary, fill = NA, size = 0.6, col='gray45') +
  geom_sf(data = streets_trunk, fill = NA, size = 0.6, col='gray45') +
  geom_sf(data = streets_secondary, fill = NA, size = 0.6, col='gray45')+
  geom_sf(data = sitp, fill = NA, size = 0.8, col='blue', alpha=0.1)+
  coord_sf(xlim=c(-74.22,-74.02), ylim=c(4.5, 4.85)) +
  theme_bw()

map2

ggsave("Views/mapasitp.pdf", width = 6, height = 4, plot = map2)


