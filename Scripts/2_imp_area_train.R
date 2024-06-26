#### Imputaciones de la variable area
setwd(substr(getwd(), 1, nchar(getwd()) - 8))

rm(list = ls())
# Set directory:

###Importamos librerias
require(pacman)
p_load(rio,
       tidyverse)


##Importamos datos
train_imp<-import('Stores/inputs/train.csv')


###Filtramos para solo area

train_imp<- train_imp %>% select(property_id, surface_total, surface_covered, description,title,bathrooms)

###Creamos la variables surfcae

train_imp <- train_imp %>% 
             mutate(surface=ifelse(is.na(surface_total), surface_covered, surface_total)) %>%
             mutate(surface=ifelse(!is.na(surface_total) & !is.na(surface_covered), pmax(surface_total, surface_covered),surface))


sum(is.na(train_imp$surface))

## Creamos dos funciones para identicar el numero que va antes de la terminos relacionados con metros
palabra_anteriordes<-function(palabra, datos){
  
  cadena<-paste0('.*?([[:alnum:]]+)\\s+', palabra,'.*')
  
  datos[,palabra] <- gsub(cadena, "\\1", datos$description)
  
  datos[[palabra]] <- ifelse(grepl("\\s", datos[[palabra]]), NA, datos[[palabra]])
  
  

  return(datos)
  
}

palabra_anteriorti<-function(palabra, datos){
  
  cadena<-paste0('.*?([[:alnum:]]+)\\s+', palabra,'.*')
  
  datos[,palabra] <- gsub(cadena, "\\1", datos$title)
  
  datos[[palabra]] <- ifelse(grepl("\\s", datos[[palabra]]), NA, datos[[palabra]])
  
  
  
  return(datos)
  
}

#creamos variables
train_imp<-palabra_anteriorti(palabra = 'm2', train_imp) 
train_imp<-train_imp %>% rename('m2ti'='m2')


##Creamos mas varaibles que pueden contener la info de area
train_imp<-palabra_anteriordes(palabra = 'mt2', train_imp)
train_imp<-palabra_anteriordes('metros', train_imp)
train_imp<-palabra_anteriordes('m2', train_imp)
train_imp<-palabra_anteriordes('mts', train_imp)
train_imp<-palabra_anteriordes('mts2', train_imp)
train_imp<-palabra_anteriordes('mtrs', train_imp)
train_imp<-palabra_anteriordes('m', train_imp)

##Identificamos otras posibles formas de llenar missigns de area con titulo
train_imp <- train_imp %>% 
    mutate(numero_mti=str_extract(title, "\\b\\d+(?=m)")) %>% 
    mutate(numero_m2de=str_extract(description, "\\b\\d+(?=m2)")) %>%
    mutate(numero_mtrs=str_extract(description, "\\b\\d+(?=mtrs)")) %>%
    mutate(numero_mt2=str_extract(description, "\\b\\d+(?=mt2)")) %>%
    mutate(numero_mts=str_extract(description, "\\b\\d+(?=mts)"))
    

#Juntamos todo en una sola variables (por ahora solo las numericas)
train_imp <- train_imp %>%
  mutate(area_num = surface) %>%
  mutate(area_num = ifelse(is.na(area_num), as.numeric(mt2), area_num)) %>%
  mutate(area_num = ifelse(is.na(area_num), as.numeric(metros), area_num)) %>%
  mutate(area_num = ifelse(is.na(area_num), as.numeric(m2), area_num)) %>%
  mutate(area_num = ifelse(is.na(area_num), as.numeric(m2ti), area_num)) %>%
  mutate(area_num = ifelse(is.na(area_num), as.numeric(mts), area_num)) %>%
  mutate(area_num = ifelse(is.na(area_num), as.numeric(mts2), area_num)) %>%
  mutate(area_num = ifelse(is.na(area_num), as.numeric(numero_mti), area_num)) %>%
  mutate(area_num = ifelse(is.na(area_num), as.numeric(numero_m2de), area_num)) %>% 
  mutate(area_num = ifelse(is.na(area_num), as.numeric(numero_mts), area_num))  %>%   
  mutate(area_num = ifelse(is.na(area_num), as.numeric(numero_mts), area_num))
  

#Juntamos todo en una sola variables (por ahora solo las cadenas de caracteres)

train_imp <- train_imp %>%
  mutate(area_str = as.character(surface)) %>%
  mutate(area_str = ifelse(is.na(area_str), as.character(mt2), area_str)) %>%
  mutate(area_str = ifelse(is.na(area_str), as.character(metros), area_str)) %>%
  mutate(area_str = ifelse(is.na(area_str), as.character(m2), area_str)) %>%
  mutate(area_str = ifelse(is.na(area_str), as.character(m2ti), area_str))%>%
  mutate(area_str = ifelse(is.na(area_str), as.character(mts), area_str)) %>%
  mutate(area_str = ifelse(is.na(area_str), as.character(mts2), area_str)) %>%
  mutate(area_str = ifelse(is.na(area_str), as.character(numero_mti), area_str)) %>%
  mutate(area_str = ifelse(is.na(area_str), as.character(numero_m2de), area_str)) 



###Ultimas modificaciones (cambiamos de string a numeros)
train_imp <- train_imp %>%
            mutate(area_str=gsub("\\bmil\\b", "1000", area_str, ignore.case = TRUE)) %>%
            mutate(area_str=gsub("\\btrecientos\\b", "300", area_str, ignore.case = TRUE)) %>%
            mutate(area_str=ifelse(grepl("^veinti", area_str), "22", area_str)) %>%
            mutate(area_str=gsub("\\bochenta\\b", "80", area_str, ignore.case = TRUE))  %>%
            mutate(area_str=gsub("\\bcincuenta\\b", "50", area_str, ignore.case = TRUE))  %>%
            mutate(area_str=gsub("\\btreinta\\b", "30", area_str, ignore.case = TRUE)) %>%
            mutate(area_str=gsub("\\bveinte\\b", "20", area_str, ignore.case = TRUE))

###Ultimas modificaciones (los convertimos a tipo numeric)
train_imp <- train_imp %>%
             mutate(area_num=ifelse(is.na(area_num), as.numeric(area_str), area_num))


#### tratamos baños

train_imp <- train_imp %>% select(property_id, area_num, bathrooms, description)




##Creamos mas varaibles que pueden contener la info baños
train_imp<-palabra_anteriordes(palabra = 'banos', train_imp)
train_imp<-palabra_anteriordes(palabra = 'bano', train_imp)


#Pasamos de string a numero (banos)
train_imp <- train_imp %>% 
  mutate(banos=gsub("\\bdos\\b", "2", banos, ignore.case = TRUE),
         banos=gsub("\\btres\\b", "3", banos, ignore.case = TRUE),
         banos=gsub("\\bcuatro\\b", "4", banos, ignore.case = TRUE),
         banos=gsub("\\bcinco\\b", "5", banos, ignore.case = TRUE),
         banos=gsub("\\bseis\\b", "6", banos, ignore.case = TRUE),
         banos=gsub("\\bun\\b", "1", banos, ignore.case = TRUE) )

#Pasamos de string a numero (bano)
train_imp <- train_imp %>% 
  mutate(bano=gsub("\\bdos\\b", "2", bano, ignore.case = TRUE),
         bano=gsub("\\btres\\b", "3", bano, ignore.case = TRUE),
         bano=gsub("\\bcuatro\\b", "4", bano, ignore.case = TRUE),
         bano=gsub("\\bcinco\\b", "5", bano, ignore.case = TRUE),
         bano=gsub("\\bseis\\b", "6", bano, ignore.case = TRUE),
         bano=gsub("\\bun\\b", "1", bano, ignore.case = TRUE))


## imputamos valores    
train_imp <- train_imp %>%
             mutate(n_baños=ifelse(is.na(bathrooms),as.numeric(banos),bathrooms),
                    n_baños=ifelse(is.na(n_baños),as.numeric(bano),n_baños))


###Dejmos vivo solo area_num

train_imp <- train_imp %>% select(property_id, area_num, n_baños)

#Exportamos datos
export(train_imp, 'Stores/outputs/train_imp_area.rds')
            

