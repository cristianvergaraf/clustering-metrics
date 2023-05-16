library(psych)
library(GPArotation)
library(dplyr)
library(corrplot)
library(dplyr)

setwd("C:/projectos/manifolds/ciudades/datos")

ciudades_mm <- read.csv("ciudades_mm.csv")
ciudades_st <- read.csv("ciudades_st.csv")

## Cambio en la matriz quitando variables poco relacionadas.

ciudades_mm = select(ciudades_mm, -c("X","index"))
ciudades_st = select(ciudades_st, -c("X", "index"))

ciudades_mm_1 = select(ciudades_mm, -c("HOS_REST")) # No hay mucho efecto
ciudades_st_1 = select(ciudades_st, -c("HOS_REST")) # No hay mucho efecto

ciudades_mm_2 = select(ciudades_mm_1, -c("EQUIP_EDU")) # Mejora levemente
ciudades_st_2 = select(ciudades_st_1, -c("EQUIP_EDU")) # Mejora levemente

ciudades_mm_3 = select(ciudades_mm_2, -c("ED_SING")) # Mejora sustancialmente
ciudades_st_3 = select(ciudades_st_2, -c("ED_SING")) # Mejora sustancialmente

ciudades_mm_4 = select(ciudades_mm_3, -c("RES_UNI_MX"))
ciudades_st_4 = select(ciudades_st_3, -c("RES_UNI_MX"))

ciudades_mm_5 = select(ciudades_mm_4, -c("RES_UNI"))
ciudades_st_5 = select(ciudades_st_4, -c("RES_UNI"))

ciudades_mm_6 = select(ciudades_mm_5, -c("SIN_EDIF"))
ciudades_st_6 = select(ciudades_st_5, -c("SIN_EDIF"))



#### Evaluar cambios eliminado puntos atipicos mas influyentes.
# not in aplicado a R https://www.statology.org/not-in-r/

ciudades_mm_ex = filter(ciudades_mm, !(ciudades %in% c("Malaga", "Santa", "Avila")))
ciudades_st_ex = filter(ciudades_st, !(ciudades %in% c("Malaga", "Santa", "Avila")))

ciudades_mm_ex1 = filter(ciudades_mm_1, !(ciudades %in% c("Malaga", "Santa", "Avila")))
ciudades_st_ex1 = filter(ciudades_st_1, !(ciudades %in% c("Malaga", "Santa", "Avila")))

ciudades_mm_ex2 = filter(ciudades_mm_2, !(ciudades %in% c("Malaga", "Santa", "Avila")))
ciudades_st_ex2 = filter(ciudades_st_2, !(ciudades %in% c("Malaga", "Santa", "Avila")))

ciudades_mm_ex3 = filter(ciudades_mm_3, !(ciudades %in% c("Malaga", "Santa", "Avila")))
ciudades_st_ex3 = filter(ciudades_st_3, !(ciudades %in% c("Malaga", "Santa", "Avila")))

ciudades_mm_ex4 = filter(ciudades_mm_4, !(ciudades %in% c("Malaga", "Santa", "Avila")))
ciudades_st_ex4 = filter(ciudades_st_4, !(ciudades %in% c("Malaga", "Santa", "Avila")))

ciudades_mm_ex5 = filter(ciudades_mm_5, !(ciudades %in% c("Malaga", "Santa", "Avila")))
ciudades_st_ex5 = filter(ciudades_st_5, !(ciudades %in% c("Malaga", "Santa", "Avila")))

ciudades_mm_ex6 = filter(ciudades_mm_6, !(ciudades %in% c("Malaga", "Santa", "Avila")))
ciudades_st_ex6 = filter(ciudades_st_6, !(ciudades %in% c("Malaga", "Santa", "Avila")))

############################################################################################
############################################################################################

dim(ciudades_mm)

KMO(ciudades_mm[,3:17]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st[,3:17]) ## Estos resultados tienen error matriz singular

KMO(ciudades_mm_1[,3:16]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st_1[,3:16]) ## Estos resultados tienen error matriz singular

KMO(ciudades_mm_2[,3:15]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st_2[,3:15]) ## Estos resultados tienen error matriz singular

KMO(ciudades_mm_3[,3:14]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st_3[,3:14]) ## Estos resultados tienen error matriz singular

KMO(ciudades_mm_4[,3:13]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st_4[,3:13]) ## Estos resultados tienen error matriz singular

KMO(ciudades_mm_5[,3:12]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st_5[,3:12]) ## Estos resultados tienen error matriz singular

KMO(ciudades_mm_6[,3:11]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st_6[,3:11]) ## Estos resultados tienen error matriz singular

##############################################3

KMO(ciudades_mm_ex[,3:17])
KMO(ciudades_st_ex[,3:17])

KMO(ciudades_mm_ex1[,3:16]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st_ex1[,3:16]) ## Estos resultados tienen error matriz singular

KMO(ciudades_mm_ex2[,3:15]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st_ex2[,3:15]) ## Estos resultados tienen error matriz singular


KMO(ciudades_mm_ex3[,3:14]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st_ex3[,3:14]) ## Estos resultados tienen error matriz singular

KMO(ciudades_mm_ex4[,3:13]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st_ex4[,3:13]) ## Estos resultados tienen error matriz singular

KMO(ciudades_mm_ex5[,3:12]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st_ex5[,3:12]) ## Estos resultados tienen error matriz singular

KMO(ciudades_mm_ex6[,3:11]) ##  Resolver el problema de la matriz singular
KMO(ciudades_st_ex6[,3:11]) ## Estos resultados tienen error matriz singular


### No vemos cambios si quitamos los outliers en el KMO


#################################

cor_met1 <- cor(ciudades_mm[,3:17])
cor_met2<- cor(ciudades_st[,3:17])

cor_met3 <- cor(ciudades_mm_ex5[,3:12])
cor_met4<- cor(ciudades_st_ex5[,3:12])


dim(ciudades_mm)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF",
                          "#77AADD", "#4477AA"))

windows()

corrplot(cor_met1, method = "shade", shade.col = NA, 
         tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", type = "upper", 
         diag = F, number.digits = 2, number.cex = 0.6)


corrplot(cor_met2, method = "shade", shade.col = NA, 
         tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", type = "upper", 
         diag = F, number.digits = 2, number.cex = 0.6)


corrplot(cor_met3, method = "shade", shade.col = NA, 
         tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", type = "upper", 
         diag = F, number.digits = 2, number.cex = 0.6)

corrplot(cor_met4, method = "shade", shade.col = NA, 
         tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", type = "upper", 
         diag = F, number.digits = 2, number.cex = 0.6)


fa.parallel(ciudades_mm_ex5[,3:12], fm = "minres", fa = "fa", n.iter = 500, ylab = "Eigenvalue") # Entre 3 y 4

fa.parallel(ciudades_st_ex5[,3:12], fm = "minres", fa = "fa", n.iter = 500, ylab = "Eigenvalue") ## Entre 3 y 4

### A pesar de las fallas 

### Hasta aqui incluimos con edificar lo que tiene un valor de mas alto de KMO 6.

fa(ciudades_mm_5[,3:12],3, rotate = "varimax", fm = "minchi")
fa(ciudades_mm_5[,3:12],3, rotate = "promax", fm = "minchi")

# ciudad_mm_6 no incluye Sin edificar

fa(ciudades_mm_6[,3:11],3, rotate = "varimax", fm = "minchi")
fa(ciudades_mm_6[,3:11],3, rotate = "promax", fm = "minchi")


###########################################################################3

fa(ciudades_mm_ex5[,2:10],2, rotate = "varimax", fm = "minchi")
fa(ciudades_mm_ex5[,2:10],2, rotate = "promax", fm = "minchi")


fa(ciudades_mm_5[,2:10],4, rotate = "varimax", fm = "minchi")
fa(ciudades_mm_5[,2:10],4, rotate = "promax", fm = "minchi")

