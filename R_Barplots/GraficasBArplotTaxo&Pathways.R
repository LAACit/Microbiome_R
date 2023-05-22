#install.packages("paletteer")
#install.packages("gcookbook")

library('tidyverse')
library(readr)
library(data.table)
library(viridis)
library(paletteer)
library(gcookbook) 
library(gridExtra)
library(grid)


#Abrimos los datos a graficar# 

XenoPathways <-read_csv("../../../Desktop/Tesis/VariosTesisV1/GraficasCartel/XenoPathways.csv")

TaxaLevelFilo <- read_csv("../../../Desktop/Tesis/VariosTesisV1/GraficasCartel/level-2.csv")

TaxaLevelClass <- read_csv("../../../Desktop/Tesis/VariosTesisV1/GraficasCartel/TaxaLevel-3.csv")

#Hacemos que tengan el mismo nombre de columnas 

Colnas<- colnames(TaxaLevelFilo)
colnames(TaxaLevelClass)<- Colnas

#Ahora hacemos tablas largas para poder graficar 



dfLarga_filo <- melt(TaxaLevelFilo, id.vars = "index", 
                  variable.name = "Sample", 
                  value.name = "Abundancia", 
                  variable.factor = FALSE)

dfLarga_class <- melt(TaxaLevelClass, id.vars="index", 
                      variable.name="Sample",
                      value.name="Abundancia",
                      variable.fracto=F)

#Graficamos para clase y filo 

#Ahora para filo 
p1<- ggplot(dfLarga_filo, aes(fill=index, y=Abundancia, x=Sample)) +
  geom_bar(position="stack", stat="identity") +
  theme_minimal()+
  labs( colour = "Clases") + 
  theme(axis.text.x = element_text( 
    angle = 0,
    size = 8)) +
  theme(legend.text = element_text(size = 8))+
  ggtitle("Clasificación taxonomica nivel de filo")+
  scale_fill_manual(values=c("#542788", "#bebada", "#fb8072", "#80b1d3", "#fdb462","#fccde5","#b3de69", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f", "#b15928"))



p2 <- ggplot(dfLarga_class, aes(fill=index, y=Abundancia, x=Sample)) + 
  geom_bar(position="stack", stat="identity") + theme_minimal()+
  theme(axis.text.x = element_text( 
    angle = 0,
    size = 8)) +
  ggtitle("Clasificación taxonomica nivel de clase")+
  theme(legend.text = element_text(size = 8))+
    scale_fill_manual(values=c("#8dd3c7", "#3CB371", "#2E8B57", "#D9F0E3", "#8DD3C7", "#FFFFB3", "#BEBADA", "#FDB462", "#FB8072", "#542788", "#FCCDE5","#b15928"))




grid.arrange(p1,p2, ncol=2)


