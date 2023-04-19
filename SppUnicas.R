#Ahora lo que vamos a hacer es una lista de las especies unicas por sitio de muestreo 

#librerias
library(tidyverse)

#Primero necesitamos importar la tabla donde qiim2 hizo la clasificación taxonomica a nivel 7

dfSpp<- read.csv("./level-7.csv", row.names = 1)


####### Para Pathways  ########

#dfSpp <- read.csv( "./path_abun_unstrat_descrip.csv",sep="\t" , row.names = 2)

#dfSpp <- dfSpp[,c(2:9)]

######################################



dfSpp<- as.data.frame(t(dfSpp)) 
#SI SON PATHTWAYS COMENTAME 





#hacemos un nuevo df que tenga la suma de los puntos de muestreo por sitio 
SisalPuerto<- rowSums(dfSpp[,7:8])  
DzilamPuerto <- rowSums(dfSpp[,3:4])
DzilamBocas <- rowSums(dfSpp[,1:2])
ElPalmar <- rowSums(dfSpp[,5:6])
Spp <- row.names(dfSpp)

#Unimos los valores que generamos en un df que tenga todo por sitio de muestreo 
dfSitiosLevel7 <- as.data.frame(cbind(SisalPuerto,ElPalmar,DzilamBocas,DzilamPuerto))

#Generamos el df ahora se puede borrar las values de sumas 

rm(SisalPuerto)
rm(DzilamPuerto)
rm(DzilamBocas )
rm(ElPalmar)
rm(dfSpp)



#Ahora lo que vamos a hacer es convertir los valores numericos en presencia y asucencia con un 1 y 0 
dfSitiosLevel7<- dfSitiosLevel7%>% mutate_all(~ifelse(. > 0, 1, .))

#Con lo que tenemos procedemos a hacer un segundo df pero ahora a nivel de estatus de conservación 
#recordando que los sitios por estatus son SisalPuerto y DzilamPuerto (alterados) y ElPalmar y DzilamBocas (Conservados)

Conservados <- rowSums(dfSitiosLevel7[,2:3])
Alterados <- rowSums(dfSitiosLevel7[,c(1,4)])

#los unimos un nuevo df de estatus 

dfEstatus <- as.data.frame(cbind(Conservados,Alterados))

#Se borran las sumas
rm(Conservados)
rm(Alterados)


#Se hace lo mismo para la regiones 

RegionDzilam <- rowSums(dfSitiosLevel7[,3:4])
RegionPalmar<- rowSums(dfSitiosLevel7[,1:2])

dfRegiones <- as.data.frame(cbind(RegionDzilam,RegionPalmar))


#removemos las sumas

rm(RegionDzilam)
rm(RegionPalmar)


#Ahora procedemos a determinar cuales especies son unicas para cada condiciones; Estatus de conservación y Region. 


#Primero hacemos que los dos df sean binarios (1 y 0 )
dfEstatus<- dfEstatus%>% mutate_all(~ifelse(. > 0, 1, .))
dfRegiones <- dfRegiones %>% mutate_all(~ifelse(. > 0, 1, .))


#Ahora para los estatus de conservacipon 

#Primero filtro los Spp que tiene valores de 0 (que no estan en la condicion)
SppConservado <- dfEstatus %>% filter( dfEstatus$Conservados > 0)
SppAlterado <-dfEstatus %>% filter(dfEstatus$Alterados > 0 )


#hacemos lista con las Spp que son unicas 

ListaSppAlterado <- row.names(SppAlterado)
ListaSppConservado <- row.names(SppConservado) 

#Eliminamos el df 




#Con lo anterior tenemos cuales Spp estan en cada estatus de conservación ahora queda sacar cuales son unicas para condiciones, para esto se usa setdiff

UnicosSppConcervado <- setdiff(ListaSppConservado,ListaSppAlterado)
UnicosSppAlterado <- setdiff(ListaSppAlterado,ListaSppConservado)

#Eliminamos Listas
rm(ListaSppAlterado)
rm(ListaSppConservado)

#Se hace lo mismos para las Regiones 

SppDzilam <- dfRegiones %>% filter(dfRegiones$RegionDzilam > 0)
SppElPalmar <- dfRegiones %>% filter(dfRegiones$RegionPalmar >0 )


ListaSppDzilam<- row.names(SppDzilam)
ListaSppElPalmar <- row.names(SppElPalmar)

#Con lo anterior tenemos cuales Spp estan en cada eregion .Ahora queda sacar cuales son unicas para condiciones, para esto se usa setdiff

UnicosSppDzilam <- setdiff(ListaSppDzilam,ListaSppElPalmar)
UnicosSppElPalmar <- setdiff(ListaSppElPalmar,ListaSppDzilam)

#Eliminamos listas y df

rm(SppDzilam)
rm(SppElPalmar)
rm(ListaSppDzilam)
rm(ListaSppElPalmar)
rm(SppConservado)
rm(SppAlterado)
rm(Spp)

write.csv(UnicosSppAlterado, "./UnicosSppAlterado.csv")
write.csv(UnicosSppConcervado , "./UnicosSppConservado.csv")
write.csv(UnicosSppDzilam, "./UnicosSppDzilam.csv")
write.csv(UnicosSppElPalmar, "./UnicosSppElPalmar.csv")


