#install.packages("paletteer")
#install.packages("gcookbook")

library('tidyverse')
library(readr)
library(data.table)
library(viridis)
library(paletteer)
library(gcookbook) 

#Abrimos los datos a graficar# 

XenoPathways <-read_csv("../../../Desktop/Tesis/VariosTesisV1/GraficasCartel/XenoPathways.csv")
TaxaClass <- read_csv("../../../Desktop/Tesis/VariosTesisV1/GraficasCartel/TaxaLevel-3.csv")

#XenoPathways <- read_csv("Desktop/GraficasCartel/XenoPathways.csv")
#TaxaClass<- read_csv("Desktop/GraficasCartel/TaxaLevel-3.csv")

TaxLarga <- melt(TaxaClass, id.vars = "Class", 
                 variable.name = "Sample", 
                 value.name = "Abundance", 
                 variable.factor = FALSE)

dfLarga_3 <- melt(XenoPathways, id.vars = "Pathway", 
                  variable.name = "Sample", 
                  value.name = "Abundance", 
                  variable.factor = FALSE)


p <- ggplot(dfLarga_3, aes(fill=Pathway, y=Abundance, x=Sample)) + 
  geom_bar(position="stack", stat="identity") + theme_classic()+
  labs( colour = "Xenobiotics pathways ") + 
  theme(axis.text.x = element_text( 
    angle = 0,
    size = 12)) +
  theme(legend.text = element_text(size = 12))

p + scale_fill_viridis_d(option="turbo")

t <- ggplot(TaxLarga, aes(fill=Class, y=Abundance, x=Sample)) + 
  geom_bar(position="stack", stat="identity") + theme_classic()+
  labs( colour = "Xenobiotics pathways ") + 
  theme(axis.text.x = element_text( 
    angle = 0,
    size = 12)) +
  theme(legend.text = element_text(size = 12))

t + scale_fill_viridis_d(option="turbo")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#Heatmap

h <- ggplot(dfLarga_3, aes(Sample, Pathway, fill= Abundance)) + 
  geom_tile()+theme_classic()
h + scale_fill_viridis(discrete=FALSE, option = "reds") 
