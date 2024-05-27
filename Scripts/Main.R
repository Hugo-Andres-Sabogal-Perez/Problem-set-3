####### Script main #######

setwd(substr(getwd(), 1, nchar(getwd()) - 8))

rm(list = ls()) #LIMPIAR ENTORNO
###Correr 


#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('0_preprocess espacial.R', encoding = 'UTF-8')

#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('1_imp_area_test.R', encoding = 'UTF-8')

#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('2_imp_area_train.R',  encoding = 'UTF-8')

#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('3_Text mining test.R',  encoding = 'UTF-8')

#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('4_Text mining train.R',  encoding = 'UTF-8')


#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('5_limpieza e impkNN.R',  encoding = 'UTF-8')

#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('6_data process.R',  encoding = 'UTF-8')

#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('7_modelos predictivos HS.R',  encoding = 'UTF-8')

#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('8_estadisticas_descriptivas.R',  encoding = 'UTF-8')

#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('9_LightGBM.R',  encoding = 'UTF-8')

#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('10_Ensamble.R',  encoding = 'UTF-8')

#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('11_Tabla folds.R',  encoding = 'UTF-8')

#Correr script 
setwd(paste0(getwd(),'/Scripts'))
source('12_XGB.R',  encoding = 'UTF-8')



