#install.packages("ComplexUpset")
library(ComplexUpset)
library(ggplot2)

#Abrimos el protecto SppUnicas donde obtuvimos cuales son las Especies unicas por condición y región de muestreo 

#que vas a graficar? Usa un # para comentar lo que no quieres usar 

# Genera grafica de rutas metabolicas (KO number) basado en el 
#KO_pred_metagenome_unstrat_descrip.tsv


Rutas <-1 


# Genera grafica de Taxonomias a nivel de Clase basado en resultado de QIIME2 

Taxonomia <- 1


#Ahora corre el programa que da los valores unicos ñ

source("./SppUnicas.R")

#OJO. Se recomineda entrar a SppUnicas. R para ver qué hace y selecionar el nivel de trabajo requerido como  cambiar los archivos .csv de niveles taxonomicos o cambiar a archivos .csv de rutas metabolicas ya que estos son diferentes.


#Prepraramos la grafica para sitios por Estatus de conservación 

Estatus<-colnames(dfEstatus)

dfEstatus[Estatus] <- dfEstatus[Estatus]==1
t(head(dfEstatus[Estatus], 3))

#Se hace lo mismo para las regiones 

Regiones <- colnames(dfRegiones)
dfRegiones[Regiones]<-dfRegiones[Regiones]==1


#Se Grafica 


set_size = function(w, h, factor=1.5) {
  s = 1 * factor
  options(
    repr.plot.width=w * s,
    repr.plot.height=h * s,
    repr.plot.res=100 / factor,
    jupyter.plot_mimetypes='image/png',
    jupyter.plot_scale=1
  )
}


set_size(8, 3)

ComplexUpset::upset(dfRegiones, Regiones, name = "Regiones", width_ratio=0.1, wrap=TRUE, set_sizes=FALSE) +
  
  ggtitle(paste("Región de estudio: ", Titulo)) +
  
  ComplexUpset::upset(dfEstatus, Estatus, name = "Sitios",width_ratio=0.1, keep_empty_groups=TRUE, wrap=TRUE, set_sizes=FALSE) +
  
  ggtitle(paste("Estatus de conservación : ", Titulo))

                             