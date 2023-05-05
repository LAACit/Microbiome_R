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

datos <- read.csv("./PICRUSTs2_Tablas/pred_metagenome_unstrat_descrip_Niveles.csv", sep='\t')

str(datos)



Datos_sumaRuta<-aggregate(datos[c("DZ14","DZ15","DZ24","DZ25","SIS24","SIS25","PM14","PM15")], by =list(datos$Ruta), FUN=sum)
colnames(Datos_sumaRuta) <- c("grupo","DZ14","DZ15","DZ24","DZ25","SIS24","SIS25","PM14","PM15")

#Ahora intento de grafica 

datos_largosRuta <- melt(Datos_sumaRuta, id.var="grupo",
                     variable.names= "variable",
                     value.name = "abundancia",
                     variable.factor = FALSE)


Datos_sumaMapa<-aggregate(datos[c("DZ14","DZ15","DZ24","DZ25","SIS24","SIS25","PM14","PM15")], by =list(datos$Pathway.Maps), FUN=sum)
colnames(Datos_sumaMapa) <- c("grupo","DZ14","DZ15","DZ24","DZ25","SIS24","SIS25","PM14","PM15")

datos_largosMapa <- melt(Datos_sumaMapa, id.var="grupo",
                         variable.names= "variable",
                         value.name = "abundancia",
                         variable.factor = FALSE)

ggplot(datos_largosRuta, aes(fill=grupo, x=variable, y=abundancia ))+
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis_d(option="turbo")

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



