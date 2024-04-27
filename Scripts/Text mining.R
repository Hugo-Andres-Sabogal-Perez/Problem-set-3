rm(list = ls())
# Set directory:
setwd('/Users/juansilva/Desktop/Universidad/Semestre 8/Big Data/Set3')

# Llamamos las librerías necesarias para la realización del trabajo
require(pacman)
require(tidyverse)
require(rio)
require(caret)
require(gridExtra)
require(skimr)
require(tidytable)
require(VIM)
require(leaps)
require(koRpus)
require(hunspell)
require(textstem)

# Importamos bases de datos
train <- read.table(unz('train.csv.zip', 'train.csv'), header = T, sep = ",")
test <- read.table('test.csv', header = T, sep = ",")

# Tipos de variables:
train$description = as.character(train$description)

### Creacion de variables de texto:
texto = train %>% select(c('property_id', 'title', 'description'))

# Extraccion de numero de pisos:
# A: Estructura mas restrictiva:
pisos = '(\\w+|\\d+)\\s+(pisos|plantas|niveles)\\b'

PISOS <- regmatches(texto$description, regexec(pisos, texto$description))
texto$pisos <- sapply(PISOS, function(x) ifelse(length(x) > 1, x[2], NA))

# B: Estructura menos restrictiva:
pisos = '(\\w+|\\d+)\\s*(pisos|plantas|niveles)\\b'

PISOS <- regmatches(texto$description, regexec(pisos, texto$description))
texto$pisosb <- sapply(PISOS, function(x) ifelse(length(x) > 1, x[2], NA))

# Extraccion de area: 
area = "(\\w+|\\d+)\\s+(m2|mt2|mts2|metros cuadrados|metros)\\b"

AREA = regmatches(texto$description, regexec(area, texto$description))
texto$area <- sapply(AREA, function(x) ifelse(length(x) > 1, x[2], NA))

# Parqueaderos:
parqueaderos = '(\\w+|\\d+)\\s+(parqueaderos)\\b'

PAR = regmatches(texto$description, regexec(parqueaderos, texto$description))
texto$n_parqueaderos = sapply(PAR, function(x) ifelse(length(x) > 1, x[2], NA))

# Cuartos:
cuartos = 
  '(\\w+|\\d+)\\s+(cuartos|alcobas|piezas|habitaciones|cuarto|alcoba|pieza|habitacion)\\b'

CUAR = regmatches(texto$description, regexec(cuartos, texto$description))
texto$n_cuartos = sapply(CUAR, function(x) ifelse(length(x) > 1, x[2], NA))

# Correccion de errores ortograficos:
texto$desc_corregido = lapply(texto$description, 
                          function(palabras) hunspell(palabras))

# Stemming y lemantizacion:
texto$desc_corregido_lemma <- lapply(texto$desc_corregido, 
                               function(palabras) lemmatize_words(palabras, language = "es"))


#
palabras = c()
palabras = append(palabras, lapply(texto$desc_corregido_lemma, 
                                   function(desc) unlist(desc)))
palabras = unlist(palabras)
unicas = unique(palabras)

# Eliminacion de palabras vacias:
vacias = c('el', 'la', 'los', 'las', 'de', 'y', 'a', 'para', 'como', 'muy',
           'o', 'por', 'es', 'se', 'le', 'que', 'al', 'tu', 'tus', 'si', 'asi')

unicas = unicas[!(unicas %in% vacias)]

unicas = sort(unicas)

# Definicion de amenities:
ascensor = c('aascensor', 'accesor', 'acsensor', 'acsensores', 'ancensor', 'ascendor',
             'ascendores', 'ascensor', 'ascensorcocina', 'ascensores', 'ascensorplanta',
             'ascesor', 'ascesores', 'asensor', 'asensores', 'asensoresbano', 'aseosres',
             '')

agente = c('agente', 'agenteinmobiliario', 'agentes', 'agentye')

alfombra = c('alfombra', 'alfombrada', 'alfombradas', 'alfombrado', 'alfombrados', 
             'alfombras', 'alfombre')
deposito = c('almacemaniento', 'almacenaje', 'almacenamiemto', 'almacenamiento', 
             'almacenamientos', 'almacenar', 'almacenas', 'almacenimiento')
amoblado = c('amobaldo', 'amoblad', 'amoblada', 'amobladas', 'amoblado', 'amoblados',
             'amoblamiento', 'amobledo', 'amueblamiento')
antiguo = c('antigedad', 'antigua', 'antiguas', 'antiguedad', 'antiguedades', 
            'antiguo', 'antiguos')
antirruido = c('antirruido', 'antirruidos', 'antiruido', 'antiruidos', 'antruido',
               'antruidos')
antisismo = c('antisisimica', 'antisismica', 'antisismico', 'antissmica')
apartaestudio = c('aparaestudio', 'aparataestudio', 'aparatestudio', 'aparatestudios',
                  'aparestudio', 'aparstudio', 'apartaestudi', 'apartaestudio', 
                  'apartaestudios', 'apartaesudio', 'apartaetudios', 'apartestudio', 
                  'apartoestudio', 'aptaestudio', 'aptartaestudio', 'aptaestudio', 
                  'aptoestudio')
arboles = c('arbol', 'arboleda', 'arboledajunto', 'arboles', 'arbolizada', 'arbolizadas',
            'arborizacia', 'arborizacin', 'arborizacion', 'arborizada', 'arborizadas',
            'arborizado', 'arborizados', 'arobicos')
hotel = c('apartahotel', )

duplex = c('apartamentoduplex', )

balcon = c('balacon', 'balc', 'balca', 'balccon', 'balcn', 'balcncomedor', 
           'balcncuarto', 'balcon', 'balconbano', 'balconcito', 'balcone',
           'balconen', 'balcones', 'balconestudio', 'balconnes', 'balcoon')

bbq = c('bbbq', 'bba', 'bbo', 'bbq', 'bbqbano', 'bbqentorno', 'bbqgimnasio', 
        'bbqinformes', 'bbqjacuzzisaunasalon', 'bbqq', 'bbqs', 'bbqsalon', 
        'bbqy', 'bbqzona', 'asadero', 'asado', 'asador', 'asadora', 'asadores',
        'asados', 'asar', 'barbacoaparque', 'barbacoas', 'barbeque', 'barbicue',
        'barbikiu', 'barbiquiu', 'barbq', 'barbqadministracion')



