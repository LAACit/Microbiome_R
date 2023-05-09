install.packages("gridExtra")
install.packages("paletteer")
library(gridExtra)
library(tidyverse)
library(readr)
library(data.table)
library(viridis)
library(paletteer)
library(gcookbook) 
library(ggplot2)



#Se abren los datos de rutas que tenemos predichas por el KEGG
datos<- read.csv("../Codigo/PICRUT2/PICRUStMarioDatos28Abril23/P2/KO_metagenome_out/pred_metagenome_contrib.tsv", sep='\t')


datos<- read.csv("../Codigo/PICRUT2/PICRUStMarioDatos28Abril23/P2/KEGG_pathways_out/pred_metagenome_unstrat_descrip_PATHS.csv", sep='\t')


#Verificamos que esten bien 
str(datos)




#HAcemos iun aggregate, esto nos agrupara los datos de acurdo a categorias que estan en las columnas. En este caso se agrupa por Ruta 
Datos_sumaRuta<-aggregate(datos[c("DZ14","DZ15","DZ24","DZ25","SIS24","SIS25","PM14","PM15")], by =list(datos$Ruta), FUN=sum)

#Cambamos el nombre de las columnas para facilidad de manejo de datos 
colnames(Datos_sumaRuta) <- c("grupo","DZ14","DZ15","DZ24","DZ25","SIS24","SIS25","PM14","PM15")

#Se hace lo mismo para agruparlos por PathwaysMaps

Datos_sumaMapa<-aggregate(datos[c("DZ14","DZ15","DZ24","DZ25","SIS24","SIS25","PM14","PM15")], by =list(datos$Pathway.Maps), FUN=sum)
colnames(Datos_sumaMapa) <- c("mapa","DZ14","DZ15","DZ24","DZ25","SIS24","SIS25","PM14","PM15")




#Ahora se preparan los datos para hacer graficas con ellos 

#Los hacemos una tabla larga donde la variable grupos (osea las rutas) van a sser agrupadas en la tabla por sitio de muestreo 

datos_largosRuta <- melt(Datos_sumaRuta, id.var="grupo",
                         variable.names= "variable",
                         value.name = "abundancia",
                         variable.factor = FALSE)

#Se hace los mismo pero para los mapas

datos_largosMapa <- melt(Datos_sumaMapa, id.var="mapa",
                         variable.names= "variable",
                         value.name = "abundancia",
                         variable.factor = FALSE)


#Se grafican los resultados 


ggplot(datos_largosRuta, aes(fill=grupo, x=variable, y=abundancia ))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis_d(option="H")+
  ggtitle("K por Ruta")


ggplot(datos_largosMapa, aes(fill=mapa, x=variable, y=abundancia ))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis_d(option="turbo")+
  ggtitle("N| de K de acuerdo al  al que pertenecen")


grid.arrange(q,t)


ggplot(datos_largosMapa, aes(fill=abundancia, x=variable, y=grupo ))+
  geom_tile()

geom_bar(position="stack", stat="identity")+
  scale_fill_viridis_d(option="turbo")






















dd <- Datos_sumaRuta[,2:9]
dd
row.names(dd)<-Datos_sumaRuta[,1]

s<-as.matrix(dd)


dd2<- as.matrix(dd)
heatmap(s)


hgrid.arrange(p,g,nrow=2)



