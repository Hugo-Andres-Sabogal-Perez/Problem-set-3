rm(list = ls())
# Set directory:
setwd('/Users/mapaosuna/Desktop/Octavo Semestre/Big Data/Talleres/Taller 3/Problem-set-3/Stores/inputs')

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

train <- read.table('train.csv', header = T, sep = ",")
test <- read.table('test.csv', header = T, sep = ",")

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

unicas[5250:10498]

# Definicion de amenities:

distrito = c("distrital","distritales","distrito","diudad","dito","ditos")
documento = c("documentaci", "documentacin", "documentacion", "documentada","documentalmente","documentis", "documentos","documetos",
              "documnetos")
dolar = c("dolar","dolares","dollarcity")
domiciliario =c("domiciliario", "domiciliarios", "domicilio", "domicilios")
domingo = c("domingo","domingos","dominio","dominos")
domo = c("domo","domos","domotica","domoticavisita","domotico","domotizacion","domotizada","domotizado", "domotizados","doomo")
donalds=c("donald","donalds","donalls")
dormitorio=c("dormir","dormitori","dormitorio","dormitorios")
garaje = c("dosgarajes")
deposito = c("dposito","dpsito")
parqueadero =c("dosparqueaderos")
drywall = c("draywall","driwall","dywall")
duplex = c("dplex","dublex", "dueplex","dulex","dulpex","duolex","dupex","dupkex","dupl","duples","duplexapartamento","duplexde","duplez",)
drogueria =c("drogeria","drogerias","drogierias","drogruerias","droguer","droguera","drogueras","drogueria","droguerias","drouerias")
ducha = c("ducha","duchas")
ductos = c("ducteria","ducto","ductos")
duque = c("duque","duquesa")
durazno = c("durazno")
dvd = c("dvd")
apartamento = c("eapartamento")
economia = c("econmica","econmicamente","econmicas","econmico","econmicos","economia","economica","economicas","economice","economico","economicos")
ecopetrol = c("ecopetrol")
etc = c("ect","ector")
edificio = c("edeficio","edf","edfgicio","edficio","edfificio","edi","edicficio","edicicio","edicio","edidifico","edif","edifcio","edificabilidad","edificables",
             "edificaci","edificacia","edificacin","edificacion","edificaciones","edificado","edificar","edifici","edificio","edificioedificio","edificion","edificioproyecto","edificios","edificioss","edifico","edificvio",
             "edifiicio","edifiico","edoficio","efidicio","efificio","eidificio","electricasedificio","eledificio")
lavanderia = c("elavanderia","elavado")
conjunto = c("elconjunto")
caseta = c("elctricocaseta")
vigilancia = c("elctricovigilancia")
caldera = c("electricacaldera")
elefante = c("elefante","elefantes")
elevador = c("eleva","elevacin","elevacion","elevado","elevadointeriorsala","elevador","elevadores")
embajada = c("embajada","embajadas","embajador")
embargos = c("embargos","embargada")
emergencia = c("emergencia","emergencias")
enfermeria = c("emfermeria","enfermera","enfermer","enfermeria")
inmueble = c("encuentratuinmueble")
madera = c("enmadera")
cocina = c("entradacocina", "ocina")
sala = c("entradasala")
transporte = c("entrasporte")
pisos = c("entrepiso","entrepisos")
rios = c("entrerrios")
equipamiento = c("equipaje","equipamento","equipamentos","equipamientos","equipo","equipos")
escaleras = c("escala" ,"escaleras","escales", "escalones")
escoba = c("escoba","escobas","escobero")
escritorio = c("escritorio","escritorios")
escuela = c("escuela","escuelas")
espacios = c("esopacios","espa","espaacio","espacio","espacioas","espacion","espacioos","espacios","espaciosa","espaciosas","espacioso","espaciosos","espaios",
             "espcio","espeacio","especio","especios")
espejo = c("espejo","espejos")






