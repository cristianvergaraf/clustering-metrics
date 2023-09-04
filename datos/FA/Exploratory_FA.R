####### Define the parameter lists ##########

library(psych)
library(GPArotation)
library(dplyr)
library(corrplot)

###########################################################################################
###########################################################################################

## Cargar datos completos ####

setwd("C:/Users/CRISTIAN/github/clustering-metrics/datos/datos_completos")

archivos <- list.files(path = ".", pattern = "*.csv$")

lista_datos <- lapply(archivos, read.csv, fileEncoding = "ISO-8859-1") %>% lapply(dplyr::select, -c("Ciudades","X"))

names(lista_datos) = archivos


lapply(lista_datos,KMO)


fa(lista_datos$df_datos_PTrans.csv,4, rotate = "varimax", fm = "minchi")

completos_PTRANS <- lista_datos$df_datos_PTrans.csv

########################################################################################
########################################################################################

## Vaerificar eliminar variables una a una en funcion a la distribucion y el numero de outliers que existen ####
### Primer intento corresponde a eliminar SQUARE_MN muhcos outliers


lista_sel_44 <- lapply(lista_datos, dplyr::select, -c("SQUARE_MN","AREA_AM", "DIVISION", "Vehiculos","SHAPE_MD","PobH")) # Tenemos el normalizer

lapply(lista_sel_44,KMO)

PTRANS_44 <- lista_sel_44$df_datos_PTrans.csv
NORM_44 <- lista_sel_44$df_datos_Normalizer.csv

########################################################################################################

lista_sel_varios <- lapply(lista_datos, dplyr::select, -c("SQUARE_MN","AREA_AM", "DIVISION", "Vehiculos","SHAPE_MD","PobH","PobM","LPI","SQUARE_MD","FRAC_MD","T_Viv_Prin",
                                                          "T_Viv_Sec","Viv_vacias","MESH","PAFRAC","AREA_MN",)) # Tenemos el normalizer
lapply(lista_sel_varios,KMO)

PTRANS32<- lista_sel_varios$df_datos_PTrans.csv
NORM32<- lista_sel_varios$df_datos_Normalizer.csv

#########################################################################

 
lista_ocio <- lapply(lista_datos, dplyr::select, -c("SQUARE_MN","AREA_AM", "DIVISION", "Vehiculos","SHAPE_MD","PobH","PobM","LPI","SQUARE_MD","FRAC_MD","T_Viv_Prin",

                                                                                                                  "T_Viv_Sec","Viv_vacias","MESH","PAFRAC","AREA_MN","OCIO")) # Tenemos el normalizer
Maxabs_ocio <- lista_ocio$df_datos_Maxabs.csv
MinMax_ocio <- lista_ocio$df_datos_MinMax.csv
Norm_ocio <- lista_ocio$df_datos_Normalizer.csv
Ptrans_ocio <- lista_ocio$df_datos_PTrans.csv
Rscaler_ocio <- lista_ocio$df_datos_Rscaler.csv
Std_ocio <- lista_ocio$df_datos_std.csv


#######################################################


lista_sel1 <- lapply(lista_datos, dplyr::select, c("LPI","AREA_MN","AREA_AM","GYRATE_MN","SHEI","NP","DIVISION","SPLIT","SHAPE_MN", "FRAC_MD","IJI","LSI","TE","ED",
                                "RNMDP_2020","PobT","Vehiculos","T_Viviendas","T_Viv_Sec","COM","ED_SING","EQUIP","IND","OCIO","OFI","RES_PLU", "RES_UNI"))
   
lapply(lista_sel1,KMO) ## Tenemos dos opciones


Norm_sel1 <-  lista_sel1$df_datos_Normalizer.csv
PTrans_sel1 <-  lista_sel1$df_datos_PTrans.csv

################################################################################
################################################################################

lista_sel2 <- lapply(lista_datos, dplyr::select, c("LPI","AREA_MN","GYRATE_MN","SHEI","NP","SPLIT","SHAPE_MN", "FRAC_MD","IJI","LSI","TE","ED",
                                               "RNMDP_2020","PobT","T_Viviendas","T_Viv_Sec","COM","ED_SING","EQUIP","IND","OCIO","OFI","RES_PLU","RES_UNI"))
lapply(lista_sel2,KMO)


Norm_sel2 <- lista_sel2$df_datos_Normalizer.csv
PTrans_sel2 <- lista_sel2$df_datos_PTrans.csv


################################################################################
#################################################################################


lista_sel3 <- lapply(lista_datos, dplyr::select, c("LPI","AREA_MN","GYRATE_MN","SHEI","SPLIT","SHAPE_MN", "FRAC_MD","IJI","LSI","TE","ED",
                                                   "RNMDP_2020","T_Viviendas","COM","ED_SING","EQUIP","IND","OCIO","OFI","RES_PLU","RES_UNI"))
lapply(lista_sel3,KMO)

Norm_sel3 <- lista_sel3$df_datos_Normalizer.csv
PTrans_sel3 <- lista_sel3$df_datos_PTrans.csv

######################################################################################################

lista_sel4 <- lapply(lista_datos, dplyr::select, c("LPI","AREA_MN","GYRATE_MN","SHEI","SPLIT","SHAPE_MN", "FRAC_MD","IJI","LSI","TE","ED",
                                                     "RNMDP_2020","T_Viviendas","COM","ED_SING","EQUIP","IND","OCIO","OFI","RES_PLU"))

lapply(lista_sel4,KMO)

Norm_sel4 <- lista_sel4$df_datos_Normalizer.csv


## Nuevas pruebas de seleccion de variables ####

### 26.06.2023 ###


lista_seleccion_general1 <- lapply(lista_datos, dplyr::select, c("TA","LPI","AREA_MN","AREA_AM","AREA_MD","GYRATE_MN","GYRATE_AM","GYRATE_MD","SHEI","SIDI","SPLIT","MESH","SHAPE_MN", "PAFRAC", "SHAPE_MD","PARA_MN","PARA_MD","FRAC_MD","IJI","LSI","TE","ED",
                                                   "RNMDP_2020","PobT", "Vehiculos", "T_Viv_Prin","T_Viv_Sec", "COM","ED_SING","EQUIP","IND","OCIO","OFI","RES_PLU","RES_UNI"))



lista_seleccion_general <- lapply(lista_datos, dplyr::select, c("TA","LPI","AREA_MN","AREA_AM","AREA_MD","GYRATE_MN","GYRATE_AM","GYRATE_MD","SHEI","SIDI","SPLIT","MESH","SHAPE_MN", "PAFRAC", "SHAPE_MD","PARA_MN","PARA_MD","FRAC_MD","IJI","LSI","TE","ED",
                                                                "RNMDP_2020","PobT", "Vehiculos", "T_Viv_Prin","T_Viv_Sec", "COM","ED_SING","EQUIP","IND","OFI","RES_PLU","RES_UNI"))



datos_seleccion_general <- list(lista_seleccion_general$df_datos_Maxabs.csv,lista_seleccion_general$df_datos_MinMax.csv,lista_seleccion_general$df_datos_Normalizer.csv, lista_seleccion_general$df_datos_Normalizer.csv,
                               lista_seleccion_general$df_datos_PTrans.csv, lista_seleccion_general$df_datos_Rscaler.csv, lista_seleccion_general$df_datos_std.csv)

nombres_seleccion_general <- c("df_datos_Maxabs","df_datos_MinMax","df_datos_Normalizer", "df_datos_Normalizer",
                               "df_datos_PTrans", "df_datos_Rscaler", "df_datos_std")


n_factors <- c(3,4,5)
fm_methods <- c("minchi")
#rotate_methods <- c("varimax","quartimax","bentlerT","equamax","varimin","geominT")
rotate_methods <- c("promax","oblimin","bentlerQ","geominQ")

setwd("C:/Users/CRISTIAN/github/clustering-metrics")

source("function_search_FA_parameters.R")

library(dplyr)

factor_analysis_export(datos_seleccion_general, nombres_seleccion_general, n_factors, fm_methods, rotate_methods)


#########################################################


## Nuevas pruebas de seleccion de variables ####

### 27.06.2023 ###


## Cargar datos completos ####

setwd("C:/Users/CRISTIAN/github/clustering-metrics/datos/datos_completos")

archivos <- list.files(path = ".", pattern = "*.csv$")



lista_datos <- lapply(archivos, read.csv, fileEncoding = "ISO-8859-1") %>% lapply(dplyr::select, -c("Ciudades","X"))

names(lista_datos) = archivos


lista_seleccion_1 <- lapply(lista_datos, dplyr::select, c("TA","LPI","AREA_MN","AREA_AM","AREA_MD","GYRATE_MN","GYRATE_AM","GYRATE_MD","SHEI","SIDI","SPLIT","MESH","SHAPE_MN","PAFRAC","SHAPE_MD","FRAC_MD","IJI","LSI","TE","ED",
                                                                 "RNMDP_2020","PobT", "Vehiculos","T_Viv_Prin", "COM","ED_SING","EQUIP","IND","OFI","RES_PLU","RES_UNI"))



datos_seleccion_1 <- list(lista_seleccion_1$df_datos_Maxabs.csv,lista_seleccion_1$df_datos_MinMax.csv,lista_seleccion_1$df_datos_Normalizer.csv, lista_seleccion_1$df_datos_Normalizer.csv,
                                lista_seleccion_1$df_datos_PTrans.csv, lista_seleccion_1$df_datos_Rscaler.csv, lista_seleccion_1$df_datos_std.csv)

nombres_seleccion_1 <- c("df_datos_Maxabs","df_datos_MinMax","df_datos_Normalizer", "df_datos_Normalizer",
                               "df_datos_PTrans", "df_datos_Rscaler", "df_datos_std")


n_factors <- c(4,5)
fm_methods <- c("minchi")
rotate_methods <- c("varimax","quartimax","bentlerT","equamax","varimin","geominT")
#rotate_methods <- c("promax","oblimin")

setwd("C:/Users/CRISTIAN/github/clustering-metrics")

source("function_search_FA_parameters.R")

library(dplyr)

factor_analysis_export(datos_seleccion_1, nombres_seleccion_1, n_factors, fm_methods, rotate_methods)


################################################################################################################
#### Explicando cosas locas que se deben explicar ####. 








##########################################################################

#data_list = list(completos_PTRANS,PTRANS_44,NORM_44,PTRANS32,NORM32, Maxabs_ocio)

#data_list <- list(completos_PTRANS,PTRANS_44,NORM_44,PTRANS32,NORM32, Maxabs_ocio, MinMax_ocio, Norm_ocio, Ptrans_ocio, Rscaler_ocio,Std_ocio, Norm_sel1, PTrans_sel1,
 #                Norm_sel2,PTrans_sel2,Norm_sel3,PTrans_sel3)

#datos = c("completos_PTRANS","PTRANS_44","NORM_44","PTRANS32","NORM32", "Maxabs_ocio")


#data_list <- list(Norm_sel2,PTrans_sel2,Norm_sel3,PTrans_sel3,Norm_sel4)
#datos <- c("Norm_sel2","PTrans_sel2","Norm_sel3","PTrans_sel3","Norm_sel4")


n_factors <- c(2, 3,4,5)
fm_methods <- c("minchi")
rotate_methods <- c("varimax","quartimax","bentlerT","equamax","varimin","geominT")



parameter_combinations <- tidyr::expand_grid(data = datos, n_factors = as.integer(n_factors),
                                             fm_methods = fm_methods,
                                             rotate_methods = rotate_methods)


#View(parameter_combinations)

########################################################################

#datos <- c("PTRANS32","NORM32","Maxabs_ocio","MinMax_ocio","Norm_ocio","Ptrans_ocio","Rscaler_ocio","Std_ocio")

#data_list <- list(PTRANS32,NORM32,Maxabs_ocio,MinMax_ocio,Norm_ocio,Ptrans_ocio,Rscaler_ocio,Std_ocio)



setwd("C:/Users/CRISTIAN/github/clustering-metrics")

source("function_search_FA_parameters.R")

library(dplyr)

factor_analysis_export(data_list, datos, n_factors, fm_methods, rotate_methods)

fa(lista_ocio,nfactors  = 3, fm = "minchi", rotate = "promax")

library(psych)

lista_ocio
  
#################################################################################

## Test de la funcion factor_analysis_export ####

# Uno de los fundamentos se basa en la siguiente función.

n_factors <- c(2,3)

fm <- c("minchi")
rotate <- c("varimax")
datos = c("completos_PTRANS","NORM32","PTRANS_44")

View(parameter_combinations)

###############################


fa_result <- fa(r = PTRANS_44 , nfactors = 4, fm = 'minchi', rotate = "varimax")

data_names = paste("data",seq(length(data_list)), sep = "")

iterations = 1

n = 4

par <- data.frame(Parameters = paste("data = ", data_names[iterations],"nfactors =", n, "fm =", fm, "rotate =", rotate))

var_resul <- df_factors(fa_result)

cbind(par, var_resul)

for (data in data_list){
  print(data)
}
