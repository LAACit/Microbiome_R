#Ahora lo que vamos a hacer es una lista de las especies unicas por sitio de muestreo 


library(tidyverse)


#Aqui se modifica el archivo que se quiere escojer 

if(exists("Rutas") ==TRUE){
  
   Titulo <- "Rutas metabolicas"
   dfSpp <- read.csv( "./PICRUSTs2_Tablas/KO_pred_metagenome_unstrat_descrip.tsv",sep="\t", row.names = 1)
   dfSpp <- dfSpp[,c(2:9)]
   
}
if(exists("Taxonomia") ==TRUE){
  Titulo <- "Taxonomia"
  
  dfSpp<- read.csv("./Taxonomias/level-3.csv", row.names = 1)
  dfSpp<- as.data.frame(t(dfSpp)) 
}

#Obtenemos la longitud del df 

Observaciones <- as.numeric(length(row.names(dfSpp)))

#hacemos un nuevo df que tenga la suma de los puntos de muestreo por sitio para conocer la aportación por sitio de muestreo 
  
DzilamBocas <- rowSums(dfSpp[,1:2])
DzilamPuerto <- rowSums(dfSpp[,3:4])
ElPalmar <- rowSums(dfSpp[,5:6])
SisalPuerto<- rowSums(dfSpp[,7:8])


Spp <- row.names(dfSpp)

#Unimos los valores que generamos en un df que tenga todo por sitio de muestreo 
dfSitiosLevel7 <- as.data.frame(cbind(SisalPuerto,ElPalmar,DzilamBocas,DzilamPuerto))



#Generamos el df y ahora se puede borrar las values de sumas 

rm(SisalPuerto)
rm(DzilamPuerto)
rm(DzilamBocas )
rm(ElPalmar)
rm(dfSpp)


#Ahora lo que vamos a hacer es convertir los valores numericos en presencia y asucencia con un 1 y 0. Esto ayuda a futuro porque queremos haer una comparación 

dfSitiosLevel7<- dfSitiosLevel7%>% mutate_all(~ifelse(. > 0, 1, .))

#Ahora hacemos un segundo df pero ahora a nivel de estatus de conservación 

#recordando que los sitios por estatus son SisalPuerto y DzilamPuerto (alterados) y ElPalmar y DzilamBocas (Conservados). Se suman las columas que corresponden a cada estatus y se guardan como lista 

Conservados <- rowSums(dfSitiosLevel7[,2:3])
Alterados <- rowSums(dfSitiosLevel7[,c(1,4)])

#los unimos un nuevo df de estatus 

dfEstatus <- as.data.frame(cbind(Conservados,Alterados))

#Se borran las sumas
rm(Conservados)
rm(Alterados)


#Se hace lo mismo para la regiones, sumamos cada sitio y lo almacenamos en su región correspondiente 
RegionPalmar<- rowSums(dfSitiosLevel7[,1:2])
RegionDzilam <- rowSums(dfSitiosLevel7[,3:4])


#Hacemos el df de regiones
dfRegiones <- as.data.frame(cbind(RegionDzilam,RegionPalmar))


#removemos las sumas

rm(RegionDzilam)
rm(RegionPalmar)


#Ahora procedemos a determinar cuales especies son unicas para cada condiciones; Estatus de conservación y Region. 


#Primero hacemos que los dos df sean binarios (1 y 0 ). Esto se hace porque al momento de hacer las sumas de los df Estatus y Región las sumas pueden dar 0(ausencia) o 1(presencia) o 2(presencia). Ejemplo, imaginemos que el dfSitiosLevel7  tiene las siguinetes filas;  

#Taxa 1  SisalPuerto(1) DzilmaPuerto(1) ElPalmar(1) DzilmaBocas(0)
#Taxa 1  SisalPuerto(0) DzilmaPuerto(0) ElPalmar(1) DzilmaBocas(0)

#Al suma por estatuspor estatus de conservación quedaria; 

#Taxa1 Conservado(1) Alterado (2)/ Presente en conservado y alterado
#Taxa1 Conservado(1) Alterado (0)/ Presente sólo en conservado

#Haciendolo 1 y 0 Quedaria así; 

#Taxa1 Conservado(1) Alterado (1)/ Presente en conservado y alterado
#Taxa1 Conservado(1) Alterado (0)/ Presente sólo en conservado

#Se hace para estatus de conservación 


dfEstatus<- dfEstatus%>% mutate_all(~ifelse(. > 0, 1, .))
dfRegiones <- dfRegiones %>% mutate_all(~ifelse(. > 0, 1, .))


#Ahora para los estatus de conservacipon 

SppConservado <- dfEstatus %>% filter( dfEstatus$Conservados > 0)
SppAlterado <-dfEstatus %>% filter(dfEstatus$Alterados > 0 )


#Ahora extraemos los nombres de los taxones o K que hay en cada Estatus de conservación y  hacemos lista.

ListaSppAlterado <- row.names(SppAlterado)
ListaSppConservado <- row.names(SppConservado) 


#Con lo anterior tenemos cuales Taxonoes y K  estan en cada estatus de conservación. Ahora queda sacar cuales son unicas para condiciones, para esto se usa setdiff. 

UnicosSppConcervado <- setdiff(ListaSppConservado,ListaSppAlterado)
UnicosSppAlterado <- setdiff(ListaSppAlterado,ListaSppConservado)



#Al mismo tiempo obtenermos aquellas Taxas o K que están compartidas entre los dos sitios o condiciones utilizando intersect 

Compartidos_Estatus <- intersect(ListaSppConservado,ListaSppAlterado)

# Se puede comprobar que la suma de Compartido y Unicos sea igual al numero de obervaciones del df inicial 

Suma_condicion <- as.numeric(length(UnicosSppAlterado)+length(UnicosSppConcervado)+length(Compartidos_Estatus))


if(Suma_condicion!=Observaciones){
  stop("La suma de sitios por condición no es igual al numero de observaciones originales ")
}


#####Eliminamos Listas#####
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


#Ahora los compartidos 

Compartidos_Region <- intersect(ListaSppDzilam,ListaSppElPalmar)



# Se puede comprobar que la suma de Compartido y Unicos sea igual al numero de obervaciones del df inicial 

Suma_region <- as.numeric(length(UnicosSppDzilam)+length(UnicosSppElPalmar)+length(Compartidos_Region))


if(Suma_region!=Observaciones){
  stop("La suma de sitios por condición no es igual al numero de observaciones originales ")
}





#Eliminamos listas y df

rm(SppDzilam)
rm(SppElPalmar)
rm(ListaSppDzilam)
rm(ListaSppElPalmar)
rm(SppConservado)
rm(SppAlterado)
rm(Spp)
rm(Suma_condicion)
rm(Suma_region)


#Se guardan los resultados 

if(exists("guardar")){

if(exists("Taxonomia")){
write.csv(UnicosSppAlterado, "./TaxasUnicas/Condición_conservacion/Alterado/L_3UnicosAlterado.csv")
write.csv(UnicosSppConcervado , "./TaxasUnicas/Condición_conservacion/Conservado/  L_3UnicosConservado.csv")
write.csv(UnicosSppDzilam, "./TaxasUnicas/Region_Geografica/Dzilam/L_3UnicosDzilam.csv")
write.csv(UnicosSppElPalmar, "./TaxasUnicas/Region_Geografica/Palmar/ L_3UnicosPalmar.csv")
write.csv(Compartidos_Region , "./Core_Microbiome_R/Taxas_compartidosRegion.csv")
write.csv(Compartidos_Estatus , "./Core_Microbiome_R/Taxas_compartidosEstatus.csv")
}

if(exists("Rutas")){
  write.csv(UnicosSppAlterado, "./KOUnicas/K_UnicosAlterados/KO_UnicosAlterado.csv")
  write.csv(UnicosSppConcervado , "./KOUnicas/K_UnicosConservado/KO_UnicosConservado.csv")
  write.csv(UnicosSppDzilam, "./KOUnicas/K_RegionDzilam/KO_ UnicosDzilam.csv")
  write.csv(UnicosSppElPalmar, "./KOUnicas/K_RegionPalmar/KO_ UnicosPalmar.csv")
  write.csv(Compartidos_Region , "./Core_Microbiome_R/K_compartidosRegion.csv")
  write.csv(Compartidos_Estatus , "./Core_Microbiome_R/K_compartidosEstatus.csv")
}
}





