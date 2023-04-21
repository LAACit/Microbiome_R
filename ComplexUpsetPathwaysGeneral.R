#Pathways unicos grafica 
library(dplyr)
library(ComplexUpset)
library(ggplot2)

#Abrimos los datos que ya tenemos ñ

DatosPaway= read.csv("../../Tesis/Microbiome_R/PicrustrTablas/pred_metagenome_unstrat_descrip (1).tsv", sep="\t", row.names = 1 )

#Visualizamos
View(DatosPaway)

#Preparamos la tabla. 
#primero hacemos que sean valores de 1 y 0 

DatosPawayCeros= DatosPaway %>% mutate_all(~ifelse(. > 0, 1, .))

#Eliminamos la columna 2 y no quedamos con las descripciones 

DatosPawayCeros= DatosPawayCeros[,2:9]

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


