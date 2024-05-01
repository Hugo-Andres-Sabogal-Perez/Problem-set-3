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
gym = c("congimnasio", )
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
