rm(list = ls())
# Set directory:
setwd(substr(getwd(), 1, nchar(getwd()) - 8))
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
require(numbers)

# Importamos bases de datos
train <- import('Stores/inputs/train.csv')

# Tipos de variables:
train$description = as.character(train$description)

### Creacion de variables de texto:
texto = train %>% select(c('property_id', 'title', 'description'))

# Extraccion de numero de pisos:
# A: Estructura mas restrictiva:
pisos = '(\\w+|\\d+)\\s+(pisos|plantas|niveles)\\b'

PISOS <- regmatches(texto$description, regexec(pisos, texto$description))
texto$pisos <- sapply(PISOS, function(x) ifelse(length(x) > 1, x[2], NA))

# Extraccion de area: 
area = "(\\w+|\\d+)\\s+(m2|mt2|mts2|metros cuadrados|metros)\\b"

AREA = regmatches(texto$description, regexec(area, texto$description))
texto$area <- sapply(AREA, function(x) ifelse(length(x) > 1, x[2], NA))

"Parentesis de HUGO
train$area<-texto$area

train<- train %>% mutate(area2=ifelse(is.na(surface_total), surface_covered, surface_total))
train<- train %>% mutate(area2=ifelse(is.na(area2), area,as.character(area2)))

sum(is.na(train$area2))"

"Dato: con imputacion de area se tienen 19.584 missing values"




# Parqueaderos:
parqueaderos = '(\\w+|\\d+)\\s+(parqueaderos)\\b'

PAR = regmatches(texto$description, regexec(parqueaderos, texto$description))
texto$n_parqueaderos = sapply(PAR, function(x) ifelse(length(x) > 1, x[2], NA))

# Cuartos:
cuartos = 
  '(\\w+|\\d+)\\s+(cuartos|alcobas|piezas|habitaciones|cuarto|alcoba|pieza|habitacion)\\b'

CUAR = regmatches(texto$description, regexec(cuartos, texto$description))
texto$n_cuartos = sapply(CUAR, function(x) ifelse(length(x) > 1, x[2], NA))

# Correccion de variables:
numeros_escritos <- c( "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
numeros_numericos <- as.character(2:10)

texto <- texto %>%
  mutate(pisos = str_replace_all(pisos, setNames(numeros_numericos,numeros_escritos)),
         area = str_replace_all(area, setNames(numeros_numericos,numeros_escritos)),
         n_parqueaderos = str_replace_all(n_parqueaderos, setNames(numeros_numericos,numeros_escritos)),
         n_cuartos = str_replace_all(n_cuartos, setNames(numeros_numericos,numeros_escritos)))

texto$pisos = as.numeric(texto$pisos)
texto$pisos <- lapply(texto$pisos, 
                      function(x) if_else(is.na(x), 1, x))
texto$pisos <- lapply(texto$pisos, 
                      function(x) if_else(x > 10, 1, x))

texto$n_parqueaderos = as.numeric(texto$n_parqueaderos)
texto$n_parqueaderos <- lapply(texto$n_parqueaderos, 
                      function(x) if_else(is.na(x), 0, x))
texto$n_parqueaderos <- lapply(texto$n_parqueaderos, 
                      function(x) if_else(x > 10, 1, x))

texto$n_cuartos = as.numeric(texto$n_cuartos)
texto$n_cuartos <- lapply(texto$n_cuartos, 
                               function(x) if_else(is.na(x), 1, x))
texto$n_cuartos <- lapply(texto$n_cuartos, 
                               function(x) if_else(x > 20, 1, x))


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
### Yisus
ascensor = c('aascensor', 'accesor', 'acsensor', 'acsensores', 'ancensor', 'ascendor',
             'ascendores', 'ascensor', 'ascensorcocina', 'ascensores', 'ascensorplanta',
             'ascesor', 'ascesores', 'asensor', 'asensores', 'asensoresbano', 'aseosres',
             'escensor', "eleva","elevacin","elevacion","elevado","elevadointeriorsala",
             "elevador","elevadores")

agente = c('agente', 'agenteinmobiliario', 'agentes', 'agentye')

alfombra = c('alfombra', 'alfombrada', 'alfombradas', 'alfombrado', 'alfombrados', 
             'alfombras', 'alfombre')
deposito = c('almacemaniento', 'almacenaje', 'almacenamiemto', 'almacenamiento', 
             'almacenamientos', 'almacenar', 'almacenas', 'almacenimiento', "bodegaje",
             "depoito", "deposio", "depositio", "deposito", "depositoa", "depositoapartamento",
             "depositobalconel", "depositoconjunto", "depositoconsta", "depositocortinasconjunto",
             "depositoedificio", "depositoel", "depositoestrato", "depositoexcelente",
             "depositola", "depositomuy", "depositoparqueaderos", "depositopiso",
             "depositos", "depositose", "depositotodo", "depostio", "depostios", "deposto",
             "depsito", "depsitos", "depsitoscalentador", "depsitostano", "depsoito",
             "desposito", "dposito","dpsito")
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
hotel = c('apartahotel')

duplex = c('apartamentoduplex', "deplex", "dplex","dublex", "dueplex","dulex",
           "dulpex","duolex","dupex","dupkex","dupl","duples","duplexapartamento",
           "duplexde","duplez")

balcon = c('balacon', 'balc', 'balca', 'balccon', 'balcn', 'balcncomedor', 
           'balcncuarto', 'balcon', 'balconbano', 'balconcito', 'balcone',
           'balconen', 'balcones', 'balconestudio', 'balconnes', 'balcoon', 
           "blacon", "blacones", "conbalcon", "depositobalconel")

bbq = c('bbbq', 'bba', 'bbo', 'bbq', 'bbqbano', 'bbqentorno', 'bbqgimnasio', 
        'bbqinformes', 'bbqjacuzzisaunasalon', 'bbqq', 'bbqs', 'bbqsalon', 
        'bbqy', 'bbqzona', 'asadero', 'asado', 'asador', 'asadora', 'asadores',
        'asados', 'asar', 'barbacoaparque', 'barbacoas', 'barbeque', 'barbicue',
        'barbikiu', 'barbiquiu', 'barbq', 'barbqadministracion', "parrila", 
        "parrillada", "parrillas", "parrillera", "parrillero")
billar =c("billar", "billares", "billarpool")
inteligente = c("biometrica", "biometrico", "biometricos", "biomtrica", "biomtrico")
blindaje = c("blindada", "blindadas", "blindado", "blindados", "blindaje", 
             "blindex")
calefaccion = c("biochimenea","calefaccin", "calefaccion", "calefactor", 
                "calenatdor", "calendor", "calentaador", "calentador", 
                "calentadores", "calentardor", "chiimenea", "chimeea", 
                "chimemea", "chimenea", "chimeneas", "chimenena", "chimenrea",
                "cimenea", "climatizada", "climatizadas", "climatizado", 
                "climenea", "depsitoscalentador","electricacaldera")
canchas = c("cancahas", "cancha", "canchaa", "canchas", "cnacha")
seguridad = c("celador", "celadores", "celadur", "celadura", "celaduria",
              "cercaco", "cercad", "cercada", "cercado", "cercas", "cerco",
              "citfono", "citfonos", "citoffonos", "citofona", "citofoni", 
              "citofonia", "citofono", "citofonos", "cmara", "cmaras", "elctricovigilancia")
clubhouse = c("clubhause", "clubhpuse", "conjuntoclub")
conjunto = c("condominio", "condominios", "condonimio", "conjnto", "conjun", 
             "conjuno", "conjunta", "conjunto", "conjuntocerradoubicada", 
             "conjuntoclub", "conjuntode", "conjuntos", "conjuto" ,
             "conjuunto", "cunjunto", "depositoconjunto", "depositocortinasconjunto", "onjunto", "elconjunto")
gym = c("congimnasio", 'gimnasio', 'gym')
cowork = c("cowork", "coworkin", "coworking", "coworkins")

### Mapa
drywall = c("draywall","driwall","dywall")
edificio = c("edeficio","edf","edfgicio","edficio","edfificio","edi","edicficio","edicicio","edicio","edidifico","edif","edifcio","edificabilidad","edificables",
             "edificaci","edificacia","edificacin","edificacion","edificaciones","edificado","edificar","edifici","edificio","edificioedificio","edificion","edificioproyecto","edificios","edificioss","edifico","edificvio",
             "edifiicio","edifiico","edoficio","efidicio","efificio","eidificio","electricasedificio","eledificio")
lavanderia = c("elavanderia","elavado")

embargos = c("embargos","embargada")
enfermeria = c("emfermeria","enfermera","enfermer","enfermeria")
escaleras = c("escala" ,"escaleras","escales", "escalones")

### Gabi
nuevo = c("nueva", "nuevas", "nuevecito", "nuevesito", "nuevo", "nuevos", "nuevoy")
obra = c("oba", "obra", "obrador", "obras", "obrero")
oficina = c("oficina", "oficias", "oficinas", "oficinascarrera", "oficionas", "oficna")
oscuro = c("oscura", "oscurece", "oscuro", "oscuridad")
parqueadero = c("paequeadero", "paqueadero", "paquadero", "paqueaderos", "paquear", "paqueo", "paraqueaderos", "parfqueaderos", "parkeadero", "parq", "parqeadero", "parqeaderos", "parqeuadero", 
                "parqeuaderos", "parqieaderos", "parqiueaderos", "parqqueadero", "parqueadero", "parquaderos", "parquaderos", "parquaedero", "parquea", "parquead", "parqueadaderos", "parqueadeeo", 
                "parqueadeo", "parqueadeors", "parqueadeos", "parqueaderas", "parqueaderi", "parqueadero", "parqueaderoa", "parqueaderode", "parqueaderodentro", "parqueaderoel", "parqueaderony", "parqueaderopara", 
                "parqueaderopiso", "parqueaderos", "parqueaderosala", "parqueaderoscubiertos", "parqueaderosdepositoedificio", "parqueaderosdepsitbalcn", "parqueaderosdescripcin", "parqueaderosegundo", "parqueaderoservicio", 
                "parqueaderosmoderno", "parqueaderostiene", "parqueaderosy", "parqueaderoy", "parqueadertos", "parqueadeto", "parqueado", "parqueadores", "parqueadrero", "parqueadro", "parqueadros", "parqueaero", "parqueaeros", 
                "parqueamos", "parquear", "parqueasero", "parqueaseros", "parquedadero", "parquedaderos", "parquedaero", "parquedaeros", "parquedaro", "parquedaros", "parquedeaderos", "parquedero", "parquederos", "parqueo", "parqueos",
                "parqueqadero", "parqueqderos", "parquer", "parqueradero", "parquesderos", "parquessaderos", "parquiaderos", "dosgarajes", "dosparqueaderos")
paisaje = c("paisaje", "paisajes", "pajisajismo", "paisajista", "paisajistico", "pasaje")
panoramico = c("panora", "panoramia", "panoramica", "panoramicas", "panoramico", "panoramicogymsala", "panoramicos", "panormaica", "panormica", "panormicas", "panormico", "panormicos")
patio = c("patiio", "patiecito", "patiosegundo")
pavimentado = c("pavimentada", "pavimentadas", "pavimentado", "pavimentados", "pavimento")
penthouse = c("penhaouse", "penhause", "penhouse", "penhuose", "pentahouse", "penthause", "penthose", "penthouseen", "penthousse", "pethouse", "ph")
pequeño = c("pequena", "pequenas", "pequeno", "pequenos", "pequeos", "peuqena", "piccola")
perro = c("perrera", "perrito", "perros", "petfriendly")
piscina = c("picina", "pileta")
pingpong = c("pingpong", "pinpog", "pinpon", "pinpong")
pintado = c("pintada", "pintadas", "pintado")
verde = c("paque", "paqrue", "paques", "parke", "parket", "parkets", "parkwey", "parqs", "parqu", "parque", "parquebarrios", "parquecito", "parquede", "parqueinfantil", "parques", "parquesbarrios", "parqus", "parqie", "paruqe", "paruqes", "parway", "pasamano", "pasamanos", "patinar", "patines", "patineta", "patinetas", "entreparques", 
          'arbol', 'arboleda', 'arboledajunto', 'arboles', 'arbolizada', 'arbolizadas','arborizacia', 'arborizacin', 'arborizacion', 'arborizada', 'arborizadas','arborizado', 'arborizados', 'arobicos', "bosque", "bosques")

# Creacion de variables a partir de la descripcion:
texto$desc_corregido_lemma <- lapply(texto$desc_corregido_lemma, unlist)

texto$ascensor = lapply(texto$desc_corregido_lemma,
                        function(x) {ifelse(any(x %in% ascensor), 1, 0)})

texto$agente = lapply(texto$desc_corregido_lemma,
                        function(x) {ifelse(any(x %in% agente), 1, 0)})

texto$alfombra = lapply(texto$desc_corregido_lemma,
                      function(x) {ifelse(any(x %in% alfombra), 1, 0)})

texto$deposito = lapply(texto$desc_corregido_lemma,
                        function(x) {ifelse(any(x %in% deposito), 1, 0)})

texto$amoblado = lapply(texto$desc_corregido_lemma,
                        function(x) {ifelse(any(x %in% amoblado), 1, 0)})

texto$antiguo = lapply(texto$desc_corregido_lemma,
                        function(x) {ifelse(any(x %in% antiguo), 1, 0)})

texto$antirruido = lapply(texto$desc_corregido_lemma,
                       function(x) {ifelse(any(x %in% antirruido), 1, 0)})

texto$antisismo = lapply(texto$desc_corregido_lemma,
                          function(x) {ifelse(any(x %in% antisismo), 1, 0)})

texto$apartaestudio = lapply(texto$desc_corregido_lemma,
                         function(x) {ifelse(any(x %in% apartaestudio), 1, 0)})

texto$hotel = lapply(texto$desc_corregido_lemma,
                             function(x) {ifelse(any(x %in% hotel), 1, 0)})

texto$duplex = lapply(texto$desc_corregido_lemma,
                     function(x) {ifelse(any(x %in% duplex), 1, 0)})

texto$balcon = lapply(texto$desc_corregido_lemma,
                      function(x) {ifelse(any(x %in% balcon), 1, 0)})

texto$bbq = lapply(texto$desc_corregido_lemma,
                      function(x) {ifelse(any(x %in% bbq), 1, 0)})

texto$billar = lapply(texto$desc_corregido_lemma,
                   function(x) {ifelse(any(x %in% billar), 1, 0)})

texto$inteligente = lapply(texto$desc_corregido_lemma,
                      function(x) {ifelse(any(x %in% inteligente), 1, 0)})

texto$blindaje = lapply(texto$desc_corregido_lemma,
                           function(x) {ifelse(any(x %in% blindaje), 1, 0)})

texto$calefaccion = lapply(texto$desc_corregido_lemma,
                        function(x) {ifelse(any(x %in% calefaccion), 1, 0)})

texto$canchas = lapply(texto$desc_corregido_lemma,
                           function(x) {ifelse(any(x %in% canchas), 1, 0)})

texto$seguridad = lapply(texto$desc_corregido_lemma,
                       function(x) {ifelse(any(x %in% seguridad), 1, 0)})

texto$clubhouse = lapply(texto$desc_corregido_lemma,
                         function(x) {ifelse(any(x %in% clubhouse), 1, 0)})

texto$conjunto = lapply(texto$desc_corregido_lemma,
                         function(x) {ifelse(any(x %in% conjunto), 1, 0)})

texto$gym = lapply(texto$desc_corregido_lemma,
                        function(x) {ifelse(any(x %in% gym), 1, 0)})

texto$cowork = lapply(texto$desc_corregido_lemma,
                   function(x) {ifelse(any(x %in% cowork), 1, 0)})

texto$drywall = lapply(texto$desc_corregido_lemma,
                      function(x) {ifelse(any(x %in% drywall), 1, 0)})

texto$edificio = lapply(texto$desc_corregido_lemma,
                       function(x) {ifelse(any(x %in% edificio), 1, 0)})

texto$lavanderia = lapply(texto$desc_corregido_lemma,
                        function(x) {ifelse(any(x %in% lavanderia), 1, 0)})

texto$embargos = lapply(texto$desc_corregido_lemma,
                          function(x) {ifelse(any(x %in% embargos), 1, 0)})

texto$enfermeria = lapply(texto$desc_corregido_lemma,
                        function(x) {ifelse(any(x %in% enfermeria), 1, 0)})

texto$escaleras = lapply(texto$desc_corregido_lemma,
                          function(x) {ifelse(any(x %in% escaleras), 1, 0)})

texto$nuevo = lapply(texto$desc_corregido_lemma,
                         function(x) {ifelse(any(x %in% nuevo), 1, 0)})

texto$obra= lapply(texto$desc_corregido_lemma,
                     function(x) {ifelse(any(x %in% obra), 1, 0)})

texto$oficina= lapply(texto$desc_corregido_lemma,
                   function(x) {ifelse(any(x %in% oficina), 1, 0)})

texto$oscuro= lapply(texto$desc_corregido_lemma,
                      function(x) {ifelse(any(x %in% oscuro), 1, 0)})

texto$parqueadero= lapply(texto$desc_corregido_lemma,
                     function(x) {ifelse(any(x %in% parqueadero), 1, 0)})

texto$paisaje= lapply(texto$desc_corregido_lemma,
                          function(x) {ifelse(any(x %in% paisaje), 1, 0)})

texto$panoramico= lapply(texto$desc_corregido_lemma,
                      function(x) {ifelse(any(x %in% panoramico), 1, 0)})

texto$patio = lapply(texto$desc_corregido_lemma,
                         function(x) {ifelse(any(x %in% patio), 1, 0)})

texto$pavimentado = lapply(texto$desc_corregido_lemma,
                     function(x) {ifelse(any(x %in% pavimentado), 1, 0)})

texto$penthouse = lapply(texto$desc_corregido_lemma,
                           function(x) {ifelse(any(x %in% penthouse), 1, 0)})

texto$pequeño = lapply(texto$desc_corregido_lemma,
                         function(x) {ifelse(any(x %in% pequeño), 1, 0)})

texto$perro = lapply(texto$desc_corregido_lemma,
                       function(x) {ifelse(any(x %in% perro), 1, 0)})

texto$piscina = lapply(texto$desc_corregido_lemma,
                     function(x) {ifelse(any(x %in% piscina), 1, 0)})

texto$pingpong = lapply(texto$desc_corregido_lemma,
                       function(x) {ifelse(any(x %in% pingpong), 1, 0)})

texto$pintado = lapply(texto$desc_corregido_lemma,
                        function(x) {ifelse(any(x %in% pintado), 1, 0)})

texto$verde = lapply(texto$desc_corregido_lemma,
                       function(x) {ifelse(any(x %in% verde), 1, 0)})

# Ultimas modificaciones:
for(col in colnames(texto)[10:54]){
  C = unlist(texto[[col]])
  texto[[col]] = C
}

i = 1 
while(i <= nrow(texto)){
  if(texto$duplex[i] == 1){
    texto$pisos[i] = 2
    i = i + 1
  }
  else{
    i = i + 1
  }
}

# Exportar la base de datos:
texto = texto %>% select(-c('title', 'description', 'desc_corregido', 'desc_corregido_lemma'))
write.csv(x = texto, file = "Stores/outputs/text_mining_train.csv", row.names = FALSE)

# Correccion de la variable AREA:
texto <- read.table("text_mining.csv", header = T, sep = ",")

numeros_escritos <- c('cero', 'uno', 'dos', 'tres', 'cuatro', 'cinco', 'seis', 'siete', 'ocho', 'nueve',
                       'diez', 'once', 'doce', 'trece', 'catorce', 'quince', 'dieciséis', 'diecisiete', 'dieciocho', 'diecinueve',
                       'veinte', 'veintiuno', 'veintidós', 'veintitrés', 'veinticuatro', 'veinticinco', 'veintiséis', 'veintisiete', 'veintiocho', 'veintinueve',
                       'treinta', 'treinta y uno', 'treinta y dos', 'treinta y tres', 'treinta y cuatro', 'treinta y cinco', 'treinta y seis', 'treinta y siete', 'treinta y ocho', 'treinta y nueve',
                       'cuarenta', 'cuarenta y uno', 'cuarenta y dos', 'cuarenta y tres', 'cuarenta y cuatro', 'cuarenta y cinco', 'cuarenta y seis', 'cuarenta y siete', 'cuarenta y ocho', 'cuarenta y nueve',
                       'cincuenta', 'cincuenta y uno', 'cincuenta y dos', 'cincuenta y tres', 'cincuenta y cuatro', 'cincuenta y cinco', 'cincuenta y seis', 'cincuenta y siete', 'cincuenta y ocho', 'cincuenta y nueve',
                       'sesenta', 'sesenta y uno', 'sesenta y dos', 'sesenta y tres', 'sesenta y cuatro', 'sesenta y cinco', 'sesenta y seis', 'sesenta y siete', 'sesenta y ocho', 'sesenta y nueve',
                       'setenta', 'setenta y uno', 'setenta y dos', 'setenta y tres', 'setenta y cuatro', 'setenta y cinco', 'setenta y seis', 'setenta y siete', 'setenta y ocho', 'setenta y nueve',
                       'ochenta', 'ochenta y uno', 'ochenta y dos', 'ochenta y tres', 'ochenta y cuatro', 'ochenta y cinco', 'ochenta y seis', 'ochenta y siete', 'ochenta y ocho', 'ochenta y nueve',
                       'noventa', 'noventa y uno', 'noventa y dos', 'noventa y tres', 'noventa y cuatro', 'noventa y cinco', 'noventa y seis', 'noventa y siete', 'noventa y ocho', 'noventa y nueve',
                       'cien', 'ciento uno', 'ciento dos', 'ciento tres', 'ciento cuatro', 'ciento cinco', 'ciento seis', 'ciento siete', 'ciento ocho', 'ciento nueve',
                       'ciento diez', 'ciento once', 'ciento doce', 'ciento trece', 'ciento catorce', 'ciento quince', 'ciento dieciséis', 'ciento diecisiete', 'ciento dieciocho', 'ciento diecinueve',
                       'ciento veinte', 'ciento veintiuno', 'ciento veintidós', 'ciento veintitrés', 'ciento veinticuatro', 'ciento veinticinco', 'ciento veintiséis', 'ciento veintisiete', 'ciento veintiocho', 'ciento veintinueve',
                       'ciento treinta', 'ciento treinta y uno', 'ciento treinta y dos', 'ciento treinta y tres', 'ciento treinta y cuatro', 'ciento treinta y cinco', 'ciento treinta y seis', 'ciento treinta y siete', 'ciento treinta y ocho', 'ciento treinta y nueve',
                       'ciento cuarenta', 'ciento cuarenta y uno', 'ciento cuarenta y dos', 'ciento cuarenta y tres', 'ciento cuarenta y cuatro', 'ciento cuarenta y cinco', 'ciento cuarenta y seis', 'ciento cuarenta y siete', 'ciento cuarenta y ocho', 'ciento cuarenta y nueve',
                       'ciento cincuenta', 'ciento cincuenta y uno', 'ciento cincuenta y dos', 'ciento cincuenta y tres', 'ciento cincuenta y cuatro', 'ciento cincuenta y cinco', 'ciento cincuenta y seis', 'ciento cincuenta y siete', 'ciento cincuenta y ocho', 'ciento cincuenta y nueve',
                       'ciento sesenta', 'ciento sesenta y uno', 'ciento sesenta y dos', 'ciento sesenta y tres', 'ciento sesenta y cuatro', 'ciento sesenta y cinco', 'ciento sesenta y seis', 'ciento sesenta y siete', 'ciento sesenta y ocho', 'ciento sesenta y nueve',
                       'ciento setenta', 'ciento setenta y uno', 'ciento setenta y dos', 'ciento setenta y tres', 'ciento setenta y cuatro', 'ciento setenta y cinco', 'ciento setenta y seis', 'ciento setenta y siete', 'ciento setenta y ocho', 'ciento setenta y nueve',
                       'ciento ochenta', 'ciento ochenta y uno', 'ciento ochenta y dos', 'ciento ochenta y tres',
                       'ciento ochenta y cuatro', 'ciento ochenta y cinco', 'ciento ochenta y seis', 'ciento ochenta y siete', 'ciento ochenta y ocho', 'ciento ochenta y nueve',
                       'ciento noventa', 'ciento noventa y uno', 'ciento noventa y dos', 'ciento noventa y tres', 'ciento noventa y cuatro', 'ciento noventa y cinco', 'ciento noventa y seis', 'ciento noventa y siete', 'ciento noventa y ocho', 'ciento noventa y nueve',
                       'doscientos', 'doscientos uno', 'doscientos dos', 'doscientos tres', 'doscientos cuatro', 'doscientos cinco', 'doscientos seis', 'doscientos siete', 'doscientos ocho', 'doscientos nueve',
                       'doscientos diez', 'doscientos once', 'doscientos doce', 'doscientos trece', 'doscientos catorce', 'doscientos quince', 'doscientos dieciséis', 'doscientos diecisiete', 'doscientos dieciocho', 'doscientos diecinueve',
                       'doscientos veinte', 'doscientos veintiuno', 'doscientos veintidós', 'doscientos veintitrés', 'doscientos veinticuatro', 'doscientos veinticinco', 'doscientos veintiséis', 'doscientos veintisiete', 'doscientos veintiocho', 'doscientos veintinueve',
                       'doscientos treinta', 'doscientos treinta y uno', 'doscientos treinta y dos', 'doscientos treinta y tres', 'doscientos treinta y cuatro', 'doscientos treinta y cinco', 'doscientos treinta y seis', 'doscientos treinta y siete', 'doscientos treinta y ocho', 'doscientos treinta y nueve',
                       'doscientos cuarenta', 'doscientos cuarenta y uno', 'doscientos cuarenta y dos', 'doscientos cuarenta y tres', 'doscientos cuarenta y cuatro', 'doscientos cuarenta y cinco', 'doscientos cuarenta y seis', 'doscientos cuarenta y siete', 'doscientos cuarenta y ocho', 'doscientos cuarenta y nueve',
                       'doscientos cincuenta', 'doscientos cincuenta y uno', 'doscientos cincuenta y dos', 'doscientos cincuenta y tres', 'doscientos cincuenta y cuatro', 'doscientos cincuenta y cinco', 'doscientos cincuenta y seis', 'doscientos cincuenta y siete', 'doscientos cincuenta y ocho', 'doscientos cincuenta y nueve',
                       'doscientos sesenta', 'doscientos sesenta y uno', 'doscientos sesenta y dos', 'doscientos sesenta y tres', 'doscientos sesenta y cuatro', 'doscientos sesenta y cinco', 'doscientos sesenta y seis', 'doscientos sesenta y siete', 'doscientos sesenta y ocho', 'doscientos sesenta y nueve',
                       'doscientos setenta', 'doscientos setenta y uno', 'doscientos setenta y dos', 'doscientos setenta y tres', 'doscientos setenta y cuatro', 'doscientos setenta y cinco', 'doscientos setenta y seis', 'doscientos setenta y siete', 'doscientos setenta y ocho', 'doscientos setenta y nueve',
                       'doscientos ochenta', 'doscientos ochenta y uno', 'doscientos ochenta y dos', 'doscientos ochenta y tres', 'doscientos ochenta y cuatro', 'doscientos ochenta y cinco', 'doscientos ochenta y seis', 'doscientos ochenta y siete', 'doscientos ochenta y ocho', 'doscientos ochenta y nueve',
                       'doscientos noventa', 'doscientos noventa y uno', 'doscientos noventa y dos', 'doscientos noventa y tres', 'doscientos noventa y cuatro', 'doscientos noventa y cinco', 'doscientos noventa y seis', 'doscientos noventa y siete', 'doscientos noventa y ocho', 'doscientos noventa y nueve',
                       'trescientos', 'trescientos uno', 'trescientos dos', 'trescientos tres', 'trescientos cuatro', 'trescientos cinco', 'trescientos seis', 'trescientos siete', 'trescientos ocho', 'trescientos nueve',
                       'trescientos diez', 'trescientos once', 'trescientos doce', 'trescientos trece', 'trescientos catorce', 'trescientos quince', 'trescientos dieciséis', 'trescientos diecisiete', 'trescientos dieciocho', 'trescientos diecinueve',
                       'trescientos veinte', 'trescientos veintiuno', 'trescientos veintidós', 'trescientos veintitrés', 'trescientos veinticuatro', 'trescientos veinticinco', 'trescientos veintiséis', 'trescientos veintisiete', 'trescientos veintiocho', 'trescientos veintinueve',
                       'trescientos treinta', 'trescientos treinta y uno', 'trescientos treinta y dos', 'trescientos treinta y tres', 'trescientos treinta y cuatro', 'trescientos treinta y cinco', 'trescientos treinta y seis', 'trescientos treinta y siete', 'trescientos treinta y ocho', 'trescientos treinta y nueve',
                       'trescientos cuarenta', 'trescientos cuarenta y uno', 'trescientos cuarenta y dos', 'trescientos cuarenta y tres', 'trescientos cuarenta y cuatro', 'trescientos cuarenta y cinco', 'trescientos cuarenta y seis', 'trescientos cuarenta y siete', 'trescientos cuarenta y ocho', 'trescientos cuarenta y nueve',
                       'trescientos cincuenta', 'trescientos cincuenta y uno', 'trescientos cincuenta y dos', 'trescientos cincuenta y tres', 'trescientos cincuenta y cuatro', 'trescientos cincuenta y cinco', 'trescientos cincuenta y seis', 'trescientos cincuenta y siete', 'trescientos cincuenta y ocho', 'trescientos cincuenta y nueve',
                       'trescientos sesenta', 'trescientos sesenta y uno', 'trescientos sesenta y dos', 'trescientos sesenta y tres', 'trescientos sesenta y cuatro', 'trescientos sesenta y cinco', 'trescientos sesenta y seis', 'trescientos sesenta y siete', 'trescientos sesenta y ocho', 'trescientos sesenta y nueve',
                       'trescientos setenta', 'trescientos setenta y uno', 'trescientos setenta y dos', 'trescientos setenta y tres', 'trescientos setenta y cuatro', 'trescientos setenta y cinco', 'trescientos setenta y seis', 'trescientos setenta y siete', 'trescientos setenta y ocho', 'trescientos setenta y nueve',
                       'trescientos ochenta', 'trescientos ochenta y uno', 'trescientos ochenta y dos', 'trescientos ochenta y tres', 'trescientos ochenta y cuatro', 'trescientos ochenta y cinco', 'trescientos ochenta y seis', 'trescientos ochenta y siete', 'trescientos ochenta y ocho', 'trescientos ochenta y nueve',
                       'trescientos noventa', 'trescientos noventa y uno', 'trescientos noventa y dos', 'trescientos noventa y tres', 'trescientos noventa y cuatro', 'trescientos noventa y cinco', 'trescientos noventa y seis', 'trescientos noventa y siete', 'trescientos noventa y ocho', 'trescientos noventa y nueve',
                       'cuatrocientos', 'cuatrocientos uno', 'cuatrocientos dos', 'cuatrocientos tres', 'cuatrocientos cuatro', 'cuatrocientos cinco', 'cuatrocientos seis', 'cuatrocientos siete', 'cuatrocientos ocho', 'cuatrocientos nueve',
                       'cuatrocientos diez', 'cuatrocientos once', 'cuatrocientos doce', 'cuatrocientos trece', 'cuatrocientos catorce', 'cuatrocientos quince', 'cuatrocientos dieciséis', 'cuatrocientos diecisiete', 'cuatrocientos dieciocho', 'cuatrocientos diecinueve',
                       'cuatrocientos veinte', 'cuatrocientos veintiuno', 'cuatrocientos veintidós', 'cuatrocientos veintitrés', 'cuatrocientos veinticuatro', 'cuatrocientos veinticinco', 'cuatrocientos veintiséis', 'cuatrocientos veintisiete', 'cuatrocientos veintiocho', 'cuatrocientos veintinueve',
                       'cuatrocientos treinta', 'cuatrocientos treinta y uno', 'cuatrocientos treinta y dos', 'cuatrocientos treinta y tres', 'cuatrocientos treinta y cuatro', 'cuatrocientos treinta y cinco', 'cuatrocientos treinta y seis', 'cuatrocientos treinta y siete', 'cuatrocientos treinta y ocho', 'cuatrocientos treinta y nueve',
                       'cuatrocientos cuarenta', 'cuatrocientos cuarenta y uno', 'cuatrocientos cuarenta y dos', 'cuatrocientos cuarenta y tres', 'cuatrocientos cuarenta y cuatro', 'cuatrocientos cuarenta y cinco', 'cuatrocientos cuarenta y seis', 'cuatrocientos cuarenta y siete', 'cuatrocientos cuarenta y ocho', 'cuatrocientos cuarenta y nueve',
                       'cuatrocientos cincuenta', 'cuatrocientos cincuenta y uno', 'cuatrocientos cincuenta y dos', 'cuatrocientos cincuenta y tres', 'cuatrocientos cincuenta y cuatro', 'cuatrocientos cincuenta y cinco', 'cuatrocientos cincuenta y seis', 'cuatrocientos cincuenta y siete', 'cuatrocientos cincuenta y ocho', 'cuatrocientos cincuenta y nueve',
                       'cuatrocientos sesenta', 'cuatrocientos sesenta y uno', 'cuatrocientos sesenta y dos', 'cuatrocientos sesenta y tres', 'cuatrocientos sesenta y cuatro', 'cuatrocientos sesenta y cinco', 'cuatrocientos sesenta y seis', 'cuatrocientos sesenta y siete', 'cuatrocientos sesenta y ocho', 'cuatrocientos sesenta y nueve',
                       'cuatrocientos setenta', 'cuatrocientos setenta y uno', 'cuatrocientos setenta y dos', 'cuatrocientos setenta y tres', 'cuatrocientos setenta y cuatro', 'cuatrocientos setenta y cinco', 'cuatrocientos setenta y seis', 'cuatrocientos setenta y siete', 'cuatrocientos setenta y ocho', 'cuatrocientos setenta y nueve',
                       'cuatrocientos ochenta', 'cuatrocientos ochenta y uno', 'cuatrocientos ochenta y dos', 'cuatrocientos ochenta y tres', 'cuatrocientos ochenta y cuatro', 'cuatrocientos ochenta y cinco', 'cuatrocientos ochenta y seis', 'cuatrocientos ochenta y siete', 'cuatrocientos ochenta y ocho', 'cuatrocientos ochenta y nueve',
                       'cuatrocientos noventa', 'cuatrocientos noventa y uno', 'cuatrocientos noventa y dos', 'cuatrocientos noventa y tres', 'cuatrocientos noventa y cuatro', 'cuatrocientos noventa y cinco', 'cuatrocientos noventa y seis', 'cuatrocientos noventa y siete', 'cuatrocientos noventa y ocho', 'cuatrocientos noventa y nueve',
                       'quinientos', 'quinientos uno', 'quinientos dos', 'quinientos tres', 'quinientos cuatro', 'quinientos cinco', 'quinientos seis', 'quinientos siete', 'quinientos ocho', 'quinientos nueve',
                       'quinientos diez', 'quinientos once', 'quinientos doce', 'quinientos trece', 'quinientos catorce', 'quinientos quince', 'quinientos dieciséis', 'quinientos diecisiete', 'quinientos dieciocho', 'quinientos diecinueve',
                       'quinientos veinte', 'quinientos veintiuno', 'quinientos veintidós', 'quinientos veintitrés', 'quinientos veinticuatro', 'quinientos veinticinco', 'quinientos veintiséis', 'quinientos veintisiete', 'quinientos veintiocho', 'quinientos veintinueve',
                       'quinientos treinta', 'quinientos treinta y uno', 'quinientos treinta y dos', 'quinientos treinta y tres', 'quinientos treinta y cuatro', 'quinientos treinta y cinco', 'quinientos treinta y seis', 'quinientos treinta y siete', 'quinientos treinta y ocho', 'quinientos treinta y nueve',
                       'quinientos cuarenta', 'quinientos cuarenta y uno', 'quinientos cuarenta y dos', 'quinientos cuarenta y tres', 'quinientos cuarenta y cuatro', 'quinientos cuarenta y cinco', 'quinientos cuarenta y seis', 'quinientos cuarenta y siete', 'quinientos cuarenta y ocho', 'quinientos cuarenta y nueve',
                       'quinientos cincuenta', 'quinientos cincuenta y uno', 'quinientos cincuenta y dos', 'quinientos cincuenta y tres', 'quinientos cincuenta y cuatro', 'quinientos cincuenta y cinco', 'quinientos cincuenta y seis', 'quinientos cincuenta y siete', 'quinientos cincuenta y ocho', 'quinientos cincuenta y nueve',
                       'quinientos sesenta', 'quinientos sesenta y uno', 'quinientos sesenta y dos', 'quinientos sesenta y tres', 'quinientos sesenta y cuatro', 'quinientos sesenta y cinco', 'quinientos sesenta y seis', 'quinientos sesenta y siete', 'quinientos sesenta y ocho', 'quinientos sesenta y nueve',
                       'quinientos setenta', 'quinientos setenta y uno', 'quinientos setenta y dos', 'quinientos setenta y tres', 'quinientos setenta y cuatro', 'quinientos setenta y cinco', 'quinientos setenta y seis', 'quinientos setenta y siete', 'quinientos setenta y ocho', 'quinientos setenta y nueve',
                       'quinientos ochenta', 'quinientos ochenta y uno', 'quinientos ochenta y dos', 'quinientos ochenta y tres', 'quinientos ochenta y cuatro', 'quinientos ochenta y cinco', 'quinientos ochenta y seis', 'quinientos ochenta y siete', 'quinientos ochenta y ocho', 'quinientos ochenta y nueve',
                       'quinientos noventa', 'quinientos noventa y uno', 'quinientos noventa y dos', 'quinientos noventa y tres', 'quinientos noventa y cuatro', 'quinientos noventa y cinco', 'quinientos noventa y seis', 'quinientos noventa y siete', 'quinientos noventa y ocho', 'quinientos noventa y nueve',
                       'seiscientos', 'seiscientos uno', 'seiscientos dos', 'seiscientos tres', 'seiscientos cuatro', 'seiscientos cinco', 'seiscientos seis', 'seiscientos siete', 'seiscientos ocho', 'seiscientos nueve',
                       'seiscientos diez', 'seiscientos once', 'seiscientos doce', 'seiscientos trece', 'seiscientos catorce', 'seiscientos quince', 'seiscientos dieciséis', 'seiscientos diecisiete', 'seiscientos dieciocho', 'seiscientos diecinueve',
                       'seiscientos veinte', 'seiscientos veintiuno', 'seiscientos veintidós', 'seiscientos veintitrés', 'seiscientos veinticuatro', 'seiscientos veinticinco', 'seiscientos veintiséis', 'seiscientos veintisiete', 'seiscientos veintiocho', 'seiscientos veintinueve',
                       'seiscientos treinta', 'seiscientos treinta y uno', 'seiscientos treinta y dos', 'seiscientos treinta y tres', 'seiscientos treinta y cuatro', 'seiscientos treinta y cinco', 'seiscientos treinta y seis', 'seiscientos treinta y siete', 'seiscientos treinta y ocho', 'seiscientos treinta y nueve',
                       'seiscientos cuarenta', 'seiscientos cuarenta y uno', 'seiscientos cuarenta y dos', 'seiscientos cuarenta y tres', 'seiscientos cuarenta y cuatro', 'seiscientos cuarenta y cinco', 'seiscientos cuarenta y seis', 'seiscientos cuarenta y siete', 'seiscientos cuarenta y ocho', 'seiscientos cuarenta y nueve',
                       'seiscientos cincuenta', 'seiscientos cincuenta y uno', 'seiscientos cincuenta y dos', 'seiscientos cincuenta y tres', 'seiscientos cincuenta y cuatro', 'seiscientos cincuenta y cinco', 'seiscientos cincuenta y seis', 'seiscientos cincuenta y siete', 'seiscientos cincuenta y ocho', 'seiscientos cincuenta y nueve',
                       'seiscientos sesenta', 'seiscientos sesenta y uno', 'seiscientos sesenta y dos', 'seiscientos sesenta y tres', 'seiscientos sesenta y cuatro', 'seiscientos sesenta y cinco', 'seiscientos sesenta y seis', 'seiscientos sesenta y siete', 'seiscientos sesenta y ocho', 'seiscientos sesenta y nueve',
                       'seiscientos setenta', 'seiscientos setenta y uno', 'seiscientos setenta y dos', 'seiscientos setenta y tres', 'seiscientos setenta y cuatro', 'seiscientos setenta y cinco', 'seiscientos setenta y seis', 'seiscientos setenta y siete', 'seiscientos setenta y ocho', 'seiscientos setenta y nueve',
                       'seiscientos ochenta', 'seiscientos ochenta y uno', 'seiscientos ochenta y dos', 'seiscientos ochenta y tres', 'seiscientos ochenta y cuatro', 'seiscientos ochenta y cinco', 'seiscientos ochenta y seis', 'seiscientos ochenta y siete', 'seiscientos ochenta y ocho', 'seiscientos ochenta y nueve',
                       'seiscientos noventa', 'seiscientos noventa y uno', 'seiscientos noventa y dos', 'seiscientos noventa y tres', 'seiscientos noventa y cuatro', 'seiscientos noventa y cinco', 'seiscientos noventa y seis', 'seiscientos noventa y siete', 'seiscientos noventa y ocho', 'seiscientos noventa y nueve',
                       'setecientos', 'setecientos uno', 'setecientos dos', 'setecientos tres', 'setecientos cuatro', 'setecientos cinco', 'setecientos seis', 'setecientos siete', 'setecientos ocho', 'setecientos nueve',
                       'setecientos diez', 'setecientos once', 'setecientos doce', 'setecientos trece', 'setecientos catorce', 'setecientos quince', 'setecientos dieciséis', 'setecientos diecisiete', 'setecientos dieciocho', 'setecientos diecinueve',
                       'setecientos veinte', 'setecientos veintiuno', 'setecientos veintidós', 'setecientos veintitrés', 'setecientos veinticuatro', 'setecientos veinticinco', 'setecientos veintiséis', 'setecientos veintisiete', 'setecientos veintiocho', 'setecientos veintinueve',
                       'setecientos treinta', 'setecientos treinta y uno', 'setecientos treinta y dos', 'setecientos treinta y tres', 'setecientos treinta y cuatro', 'setecientos treinta y cinco', 'setecientos treinta y seis', 'setecientos treinta y siete', 'setecientos treinta y ocho', 'setecientos treinta y nueve',
                       'setecientos cuarenta', 'setecientos cuarenta y uno', 'setecientos cuarenta y dos', 'setecientos cuarenta y tres', 'setecientos cuarenta y cuatro', 'setecientos cuarenta y cinco', 'setecientos cuarenta y seis', 'setecientos cuarenta y siete', 'setecientos cuarenta y ocho', 'setecientos cuarenta y nueve',
                       'setecientos cincuenta', 'setecientos cincuenta y uno', 'setecientos cincuenta y dos', 'setecientos cincuenta y tres', 'setecientos cincuenta y cuatro', 'setecientos cincuenta y cinco', 'setecientos cincuenta y seis', 'setecientos cincuenta y siete', 'setecientos cincuenta y ocho', 'setecientos cincuenta y nueve',
                       'setecientos sesenta', 'setecientos sesenta y uno', 'setecientos sesenta y dos', 'setecientos sesenta y tres', 'setecientos sesenta y cuatro', 'setecientos sesenta y cinco', 'setecientos sesenta y seis', 'setecientos sesenta y siete', 'setecientos sesenta y ocho', 'setecientos sesenta y nueve',
                       'setecientos setenta', 'setecientos setenta y uno', 'setecientos setenta y dos', 'setecientos setenta y tres', 'setecientos setenta y cuatro', 'setecientos setenta y cinco', 'setecientos setenta y seis', 'setecientos setenta y siete', 'setecientos setenta y ocho', 'setecientos setenta y nueve',
                       'setecientos ochenta', 'setecientos ochenta y uno', 'setecientos ochenta y dos', 'setecientos ochenta y tres', 'setecientos ochenta y cuatro', 'setecientos ochenta y cinco', 'setecientos ochenta y seis', 'setecientos ochenta y siete', 'setecientos ochenta y ocho', 'setecientos ochenta y nueve',
                       'setecientos noventa', 'setecientos noventa y uno', 'setecientos noventa y dos', 'setecientos noventa y tres', 'setecientos noventa y cuatro', 'setecientos noventa y cinco', 'setecientos noventa y seis', 'setecientos noventa y siete', 'setecientos noventa y ocho', 'setecientos noventa y nueve',
                       'ochocientos', 'ochocientos uno', 'ochocientos dos', 'ochocientos tres', 'ochocientos cuatro', 'ochocientos cinco', 'ochocientos seis', 'ochocientos siete', 'ochocientos ocho', 'ochocientos nueve',
                       'ochocientos diez', 'ochocientos once', 'ochocientos doce', 'ochocientos trece', 'ochocientos catorce', 'ochocientos quince', 'ochocientos dieciséis', 'ochocientos diecisiete', 'ochocientos dieciocho', 'ochocientos diecinueve',
                       'ochocientos veinte', 'ochocientos veintiuno', 'ochocientos veintidós', 'ochocientos veintitrés', 'ochocientos veinticuatro', 'ochocientos veinticinco', 'ochocientos veintiséis', 'ochocientos veintisiete', 'ochocientos veintiocho', 'ochocientos veintinueve',
                       'ochocientos treinta', 'ochocientos treinta y uno', 'ochocientos treinta y dos', 'ochocientos treinta y tres', 'ochocientos treinta y cuatro', 'ochocientos treinta y cinco', 'ochocientos treinta y seis', 'ochocientos treinta y siete', 'ochocientos treinta y ocho', 'ochocientos treinta y nueve',
                       'ochocientos cuarenta', 'ochocientos cuarenta y uno', 'ochocientos cuarenta y dos', 'ochocientos cuarenta y tres', 'ochocientos cuarenta y cuatro', 'ochocientos cuarenta y cinco', 'ochocientos cuarenta y seis', 'ochocientos cuarenta y siete', 'ochocientos cuarenta y ocho', 'ochocientos cuarenta y nueve',
                       'ochocientos cincuenta', 'ochocientos cincuenta y uno', 'ochocientos cincuenta y dos', 'ochocientos cincuenta y tres', 'ochocientos cincuenta y cuatro', 'ochocientos cincuenta y cinco', 'ochocientos cincuenta y seis', 'ochocientos cincuenta y siete', 'ochocientos cincuenta y ocho', 'ochocientos cincuenta y nueve',
                       'ochocientos sesenta', 'ochocientos sesenta y uno', 'ochocientos sesenta y dos', 'ochocientos sesenta y tres', 'ochocientos sesenta y cuatro', 'ochocientos sesenta y cinco', 'ochocientos sesenta y seis', 'ochocientos sesenta y siete', 'ochocientos sesenta y ocho', 'ochocientos sesenta y nueve',
                       'ochocientos setenta', 'ochocientos setenta y uno', 'ochocientos setenta y dos', 'ochocientos setenta y tres', 'ochocientos setenta y cuatro', 'ochocientos setenta y cinco', 'ochocientos setenta y seis', 'ochocientos setenta y siete', 'ochocientos setenta y ocho', 'ochocientos setenta y nueve',
                       'ochocientos ochenta', 'ochocientos ochenta y uno', 'ochocientos ochenta y dos', 'ochocientos ochenta y tres', 'ochocientos ochenta y cuatro', 'ochocientos ochenta y cinco', 'ochocientos ochenta y seis', 'ochocientos ochenta y siete', 'ochocientos ochenta y ocho', 'ochocientos ochenta y nueve',
                       'ochocientos noventa', 'ochocientos noventa y uno', 'ochocientos noventa y dos', 'ochocientos noventa y tres', 'ochocientos noventa y cuatro', 'ochocientos noventa y cinco', 'ochocientos noventa y seis', 'ochocientos noventa y siete', 'ochocientos noventa y ocho', 'ochocientos noventa y nueve',
                       'novecientos', 'novecientos uno', 'novecientos dos', 'novecientos tres', 'novecientos cuatro', 'novecientos cinco', 'novecientos seis', 'novecientos siete', 'novecientos ocho', 'novecientos nueve',
                       'novecientos diez', 'novecientos once', 'novecientos doce', 'novecientos trece', 'novecientos catorce', 'novecientos quince', 'novecientos dieciséis', 'novecientos diecisiete', 'novecientos dieciocho', 'novecientos diecinueve',
                       'novecientos veinte', 'novecientos veintiuno', 'novecientos veintidós', 'novecientos veintitrés', 'novecientos veinticuatro', 'novecientos veinticinco', 'novecientos veintiséis', 'novecientos veintisiete', 'novecientos veintiocho', 'novecientos veintinueve',
                       'novecientos treinta', 'novecientos treinta y uno', 'novecientos treinta y dos', 'novecientos treinta y tres', 'novecientos treinta y cuatro', 'novecientos treinta y cinco', 'novecientos treinta y seis', 'novecientos treinta y siete', 'novecientos treinta y ocho', 'novecientos treinta y nueve',
                       'novecientos cuarenta', 'novecientos cuarenta y uno', 'novecientos cuarenta y dos', 'novecientos cuarenta y tres', 'novecientos cuarenta y cuatro', 'novecientos cuarenta y cinco', 'novecientos cuarenta y seis', 'novecientos cuarenta y siete', 'novecientos cuarenta y ocho', 'novecientos cuarenta y nueve',
                       'novecientos cincuenta', 'novecientos cincuenta y uno', 'novecientos cincuenta y dos', 'novecientos cincuenta y tres', 'novecientos cincuenta y cuatro', 'novecientos cincuenta y cinco', 'novecientos cincuenta y seis', 'novecientos cincuenta y siete', 'novecientos cincuenta y ocho', 'novecientos cincuenta y nueve',
                       'novecientos sesenta', 'novecientos sesenta y uno', 'novecientos sesenta y dos', 'novecientos sesenta y tres', 'novecientos sesenta y cuatro', 'novecientos sesenta y cinco', 'novecientos sesenta y seis', 'novecientos sesenta y siete', 'novecientos sesenta y ocho', 'novecientos sesenta y nueve',
                       'novecientos setenta', 'novecientos setenta y uno', 'novecientos setenta y dos', 'novecientos setenta y tres', 'novecientos setenta y cuatro', 'novecientos setenta y cinco', 'novecientos setenta y seis', 'novecientos setenta y siete', 'novecientos setenta y ocho', 'novecientos setenta y nueve',
                       'novecientos ochenta', 'novecientos ochenta y uno', 'novecientos ochenta y dos', 'novecientos ochenta y tres', 'novecientos ochenta y cuatro', 'novecientos ochenta y cinco', 'novecientos ochenta y seis', 'novecientos ochenta y siete', 'novecientos ochenta y ocho', 'novecientos ochenta y nueve',
                       'novecientos noventa', 'novecientos noventa y uno', 'novecientos noventa y dos', 'novecientos noventa y tres', 'novecientos noventa y cuatro', 'novecientos noventa y cinco', 'novecientos noventa y seis', 'novecientos noventa y siete', 'novecientos noventa y ocho', 'novecientos noventa y nueve',
                       'mil')
numeros_numericos <- as.character(0:1000)

texto <- texto %>%
  mutate(area = str_replace_all(area, setNames(numeros_numericos,numeros_escritos)))

texto$area = as.numeric(texto$area)
texto$area <- lapply(texto$area, 
                      function(x) if_else(x > 1000, 1000, x))


AREA = texto %>% select(c('property_id', 'area'))
AREA = AREA %>% left_join(train[,c('property_id', 'surface_total', 'surface_covered')])

AREA$area = as.numeric(AREA$area)
AREA$area <- lapply(AREA$area, 
                     function(x) if_else(x > 601 | x < 50, NA, x))
i = 1 
while(i <= nrow(AREA)){
  if(is.na(AREA$surface_total[i])){
    AREA$surface_total[i] = AREA$surface_covered[i]
    i = i + 1
  }
  else{
    i = i + 1
  }
}

i = 1 
while(i <= nrow(AREA)){
  if(is.na(AREA$surface_total[i])){
    AREA$surface_total[i] = AREA$area[i]
    i = i + 1
  }
  else{
    i = i + 1
  }
}

i = 1 
while(i <= nrow(AREA)){
  if(!is.na(AREA$surface_total[i]) & !is.na(AREA$surface_covered[i]) & AREA$surface_total[i] < AREA$surface_covered[i]){
    AREA$surface_total[i] = AREA$surface_covered[i]
    i = i + 1
  }
  else{
    i = i + 1
  }
}

AREA = AREA %>% select(-c('property_id', 'surface_total'))
write.csv(x = AREA, file = "text_mining_area.csv", row.names = FALSE)

