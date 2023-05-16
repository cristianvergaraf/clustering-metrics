rm(list=ls())
library(sf)

#Creamos una función que calcule el porcentaje de uso en cada ciudad
fun <- function(x){
  area_uso <- aggregate(x$AREA, list(x$USO), FUN = sum)
  area_total <- sum(x$AREA)
  area_uso$x <- (area_uso$x / area_total) * 100
  orden <- match(resultados$USO, area_uso$Group.1)
  return (area_uso$x[orden])
}

#Obtenemos una lista con el nombre de los archivos a analizar
archivos <- list.files(pattern = ".shp")
#Creamos una tabla donnde guardar los resultados
resultados <- as.data.frame(sort(unique(st_set_geometry(st_read(archivos[1]),NULL)[, c("USO")])))
resultados <- subset(resultados, resultados[,1] != "OTROS")
colnames(resultados) <- "USO"

#Obtenemos el porcentaje de uso de cada categoría en cada ciudad
for (y in (1:(length(archivos)))) {
  try({
    tabla <- as.data.frame(st_set_geometry(st_read(archivos[y]), NULL))[, c("AREA", "USO")]
    tabla <- subset(tabla, tabla[,2] %in% resultados$USO)
    resultados[, ncol(resultados) + 1] <- fun(tabla)
    colnames(resultados)[y+1] <- archivos[y]
  }) 
}

warnings()
