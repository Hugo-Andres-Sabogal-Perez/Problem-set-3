# Parte Gabi
rm(list = ls())
# Set directory:
setwd('/Users/gabrielaperez/Desktop/repositorios/Problem-Set-3/Stores/inputs')
# Llamamos las librerías necesarias para la realización del trabajo
install.packages("koRpus")
install.packages("hunspell")
install.packages("textstem")

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

unicas[10499:15743]

# Definicion de amenities:

notaria = c("notara", "notaras", "notaria", "notarias", "notars")
nuevo = c("nueva", "nuevas", "nuevecito", "nuevesito", "nuevo", "nuevos", "nuevoy")
obra = c("oba", "obra", "obrador", "obras", "obrero")
occidente = c("occ", "occidentales", "occidente", "occientede", "occiental", "ocidental", "ocidente")
ocupado = c("ocupa", "ocupacion", "ocupada", "ocupado", "ocupan", "ocupando", "ocupantes", "ocupar", "ocuparlo")
odontologia = c("odo", "ondontol", "odontolgicos", "odontologicas", "odontologico", "odontologicos", "ontologica")
oficina = c("oficina", "oficias", "oficinas", "oficinascarrera", "oficionas", "oficna")
olimpica = c("olimpica", "olimpia", "olmpica", "olympica")
oriente = c("orie", "orienta", "orientales", "oriente")
oscuro = c("oscura", "oscurece", "oscuro", "oscuridad")
outlet = c("oulet", "outles")
parqueadero = c("paequeadero", "paqueadero", "paquadero", "paqueaderos", "paquear", "paqueo", "paraqueaderos", "parfqueaderos", "parkeadero", "parq", "parqeadero", "parqeaderos", "parqeuadero", 
                "parqeuaderos", "parqieaderos", "parqiueaderos", "parqqueadero", "parqueadero", "parquaderos", "parquaderos", "parquaedero", "parquea", "parquead", "parqueadaderos", "parqueadeeo", 
                "parqueadeo", "parqueadeors", "parqueadeos", "parqueaderas", "parqueaderi", "parqueadero", "parqueaderoa", "parqueaderode", "parqueaderodentro", "parqueaderoel", "parqueaderony", "parqueaderopara", 
                "parqueaderopiso", "parqueaderos", "parqueaderosala", "parqueaderoscubiertos", "parqueaderosdepositoedificio", "parqueaderosdepsitbalcn", "parqueaderosdescripcin", "parqueaderosegundo", "parqueaderoservicio", 
                "parqueaderosmoderno", "parqueaderostiene", "parqueaderosy", "parqueaderoy", "parqueadertos", "parqueadeto", "parqueado", "parqueadores", "parqueadrero", "parqueadro", "parqueadros", "parqueaero", "parqueaeros", 
                "parqueamos", "parquear", "parqueasero", "parqueaseros", "parquedadero", "parquedaderos", "parquedaero", "parquedaeros", "parquedaro", "parquedaros", "parquedeaderos", "parquedero", "parquederos", "parqueo", "parqueos",
                "parqueqadero", "parqueqderos", "parquer", "parqueradero", "parquesderos", "parquessaderos", "parquiaderos")
paisaje = c("paisaje", "paisajes", "pajisajismo", "paisajista", "paisajistico", "pasaje")
panaderia = c("panader", "panadera", "panaderas", "panaderia", "panaderias")
panoramico = c("panora", "panoramia", "panoramica", "panoramicas", "panoramico", "panoramicogymsala", "panoramicos", "panormaica", "panormica", "panormicas", "panormico", "panormicos")
papeleria = c("papeler", "papelera", "papeleras", "papeleria", "papelerias")
parque = c("paque", "paqrue", "paques", "parke", "parket", "parkets", "parkwey", "parqs", "parqu", "parque", "parquebarrios", "parquecito", "parquede", "parqueinfantil", "parques", "parquesbarrios", "parqus", "parqie", "paruqe", "paruqes", "parway", "pasamano", "pasamanos", "patinar", "patines", "patineta", "patinetas", "entreparques")
paradero = c("parada", "paradas", "paradero", "paraderos")
parrilla = c("parrila", "parrillada", "parrillas", "parrillera", "parrillero")
parroquia = c("parroquia", "parroquial", "parroquias")
pasillo = c("pasillo", "pasillos")
pasteleria = c("pasteleras", "pasteleria")
patio = c("patiio", "patiecito", "patiosegundo")
pavimentado = c("pavimentada", "pavimentadas", "pavimentado", "pavimentados", "pavimento")
peatonal = c("peatonal", "peatonales", "peatonalmente", "peatones")
peliculas = c("pelculas", "pelicula", "peliculas")
peluqueria = c("pelo", "peluquer", "peluquera", "peluqueras", "peluqueria", "peluquerias")
penthouse = c("penhaouse", "penhause", "penhouse", "penhuose", "pentahouse", "penthause", "penthose", "penthouseen", "penthousse", "pethouse", "ph")
pequeño = c("pequena", "pequenas", "pequeno", "pequenos", "pequeos", "peuqena", "piccola")
perro = c("perrera", "perrito", "perros", "petfriendly")
persiana = c("persianas", "persiana", "persionas")
pescaderia = c("pescaderia", "pescadero", "pescador")
piscina = c("picina", "pileta")
piedra = c("piedra", "piedras", "pierda", "pierdas")
pingpong = c("pingpong", "pinpog", "pinpon", "pinpong")
pintado = c("pintada", "pintadas", "pintado")
