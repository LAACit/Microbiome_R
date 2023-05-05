#Pathways unicos grafica 
library(dplyr)
library(ComplexUpset)
library(ggplot2)

#Abrimos los datos que ya tenemos 

DatosPaway= read.csv("./PICRUSTs2_Tablas/pred_metagenome_unstrat_descrip_Mario.tsv", sep="\t")

names <- list(DatosPaway[,1])
rownames(DatosPaway)<- names 


#Preparamos la tabla. 
#primero hacemos que sean valores de 1 y 0 

DatosPawayCeros= DatosPaway %>% mutate_all(~ifelse(. > 0, 1, .))

#Eliminamos la columna 2 y no quedamos con las descripciones 

DatosPawayCeros= DatosPawayCeros[,2:9]

DzilamBocas <- rowSums(DatosPawayCeros[,1:2])
DzilamPuerto <- rowSums(DatosPawayCeros[,3:4])
ElPalmar <- rowSums(DatosPawayCeros[,5:6])
SisalPuerto<- rowSums(DatosPawayCeros[,7:8])


dfSitiosLevel7 <- as.data.frame(cbind(SisalPuerto,ElPalmar,DzilamBocas,DzilamPuerto))

Conservados <- rowSums(dfSitiosLevel7[,2:3])
Alterados <- rowSums(dfSitiosLevel7[,c(1,4)])


dfSitiosLevel7 <- as.data.frame(cbind(Conservados,Alterados))

dfSitiosLevel7<- dfSitiosLevel7%>% mutate_all(~ifelse(. > 0, 1, .))
sitios <- colnames(dfSitiosLevel7)

dfSitiosLevel7[sitios] <- dfSitiosLevel7[sitios]==1

ComplexUpset::upset(dfSitiosLevel7,sitios,name='Sitios',width_ratio=0.1,  n_intersections= 'all')+
  ggtitle("Datos Origen Mario ")









#Ahora los acemos booleanos 
#Eliminamos el archivo original para ahorrar espacio y renombramos 
rm(DatosPaway)
DatosPathways <- DatosPawayCeros
rm(DatosPawayCeros)
#Ahora sí a booleanos 

#1) primero extraemos los nombres de los sitios de uesteo 
sitios <- colnames(DatosPathways)

#Se aplica la condicione de ==1 para dar valores de verdadero y falso segun su valor 
DatosPathways[sitios] <- DatosPathways[sitios]==1
#vems que los datos anden bien :)
View(DatosPathways)


#procedemos a hacer la grafica 

ComplexUpset::upset(DatosPathways, sitios, name='Sitios',width_ratio=0.1,  n_intersections= 'all',
  queries=list(
    
   
  upset_query( set='DZ14',fill='green'),
  upset_query(set='DZ15',fill='green'),
  
  upset_query( set='DZ24',fill='red'),
  upset_query(set='DZ25',fill='red'),
  
  upset_query( set='SIS24',fill='red'),
  upset_query(set='SIS25',fill='red'),
  
  upset_query( set='PM14',fill='green'),
  upset_query(set='PM15',fill='green')
  
 
                    
        ))+ ggtitle('Pathways PICRUSt 2')


