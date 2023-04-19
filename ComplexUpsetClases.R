#install.packages("ComplexUpset")
library(ComplexUpset)
library(ggplot2)

# Abirir los datos de taxonomia a  Nivel de Clase 
Nivel3Puntos <- read.csv("./Taxonomias/level-3.csv", row.names = 1)

#Se modifican los valores para que sean 0 y 1 y se puedan realizar los diagramas upset
Nivel3Puntos<- Nivel3Puntos %>% mutate_all(~ifelse(. > 0, 1, .))
Nivel3Puntos <- as.data.frame(t(Nivel3Puntos))

Sitios<-colnames(Nivel3Puntos)

Nivel3Puntos[Sitios] <- Nivel3Puntos[Sitios]==1
t(head(Nivel3Puntos[Sitios], 3))


#Se hace el diagrma upset 

ComplexUpset::upset(Nivel3Puntos,Sitios, name='Clases',width_ratio=0.1,
                    base_annotations=list( 'Intersection size'=intersection_size(counts=FALSE)))




ComplexUpset::upset(Nivel3Puntos,Sitios, name='Clases',width_ratio=0.1, n_intersection=  60,
      base_annotations=list( 'Intersection size'=intersection_size(counts=TRUE)),
     
      #Ahora se van a resaltar en colores las intersecciones de interes 
     #Primero en color rojo intersecciones de Sitios Dzilam 
     
       queries=list(
        
         upset_query(
         intersect=c('PM14','PM15','SIS25', 'SIS24'),
         color='orange',
         fill='orange',
         only_components=c('intersections_matrix', 'Intersection size')
       ),
      
       #Ahora le damos color a las secciones de set size 
        
       upset_query( set='DZ14',fill='green'),
       upset_query(set='DZ15',fill='green'),
       
       upset_query( set='DZ24',fill='red'),
       upset_query(set='DZ25',fill='red'),
     
       upset_query( set='SIS24',fill='red'),
       upset_query(set='SIS25',fill='red'),
       
       upset_query( set='PM14',fill='green'),
       upset_query(set='PM15',fill='green')
    
     )
) + ggtitle("Clases taxonomicas por sitio")










