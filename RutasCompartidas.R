#Rutas compartidas
library(dplyr)
library(UpSetR)

DatosPaway= read.csv("../../../Tesis/Codigo/PICRUT2/PicrustDM6/pathways_out/path_abun_unstrat_descrip.csv", sep="\t" , row.names = 2)
View(DatosPaway)
DatosPawayCeros= DatosPaway %>% mutate_all(~ifelse(. > 0, 1, .))
DatosPawayCeros= DatosPawayCeros[,2:9]

upset(DatosPawayCeros, nsets = 8, order.by = "freq", 
      text.scale = c(1.3, 1.3, 1, 1, 2, 2))


######## Por sitio de conservació##########

DatosPawayEstatus=read.csv("../../../Tesis/Codigo/PICRUT2/PicrustDM6/pathways_out/path_abun_unstrat_descripConservAlterd.csv", sep = "\t", row.names = 1)

DatosPawayCerosStatus= DatosPawayEstatus %>% mutate_all(~ifelse(. > 0, 1, .))

DatosPawayCerosStatus$Conservado <- as.numeric(DatosPawayCerosStatus$Conservado)
DatosPawayCerosStatus$Alterado <- as.numeric(DatosPawayCerosStatus$Alterado)

#vemos que sean datos numericos 
str(DatosPawayCerosStatus)

#Realizamos el grafico ordenado por frecuencia 

upset(DatosPawayCerosStatus, order.by = "freq",  text.scale = c(1.3, 1.3, 1, 1, 2, 2))


####Vamos a conocer cuales son las rutas unicas por estatus de conservación#####

#Primero se obtienen los pathways que estan den conservados

PawayConservado<- DatosPawayCerosStatus %>% filter(DatosPawayCerosStatus$Conservado > 0)

PawayConservado$Rutas<- row.names(PawayConservado)
PawayConservado=PawayConservado$Rutas


PawayAlterado<- DatosPawayCerosStatus %>% filter(DatosPawayCerosStatus$Alterado > 0 )

PawayAlterado$Rutas<- row.names(PawayAlterado)
PawayAlterado=PawayAlterado$Rutas





InterseccionPwayEstatus <- intersect(PawayAlterado, PawayConservado)
InterseccionPwayEstatus

UnicoPawayConservado= setdiff(PawayConservado,PawayAlterado)
UnicoPawayAlterado= setdiff(PawayAlterado,PawayConservado)

UnicoPawayAlterado

write.csv(UnicoPawayAlterado, './RutasUnicasAlterados.csv')
write.csv(UnicoPawayConservado, './RutasUnicasConservado.csv')


##### Ahora por Región ############################################################3
###########################################################################
#############################################################################
###################################################################

DatosPawayRegion= read.csv("../../../Tesis/Codigo/PICRUT2/PicrustDM6/pathways_out/path_abun_unstrat_descripRegiom.csv", sep="\t" , row.names = 1)
DatosPawayCerosRegion= DatosPawayRegion %>% mutate_all(~ifelse(. > 0, 1, .))
DatosPawayCerosRegion= DatosPawayCerosRegion %>% mutate_all(~ifelse(. > 0, 1, .))

str(DatosPawayCerosRegion)
DatosPawayCerosRegion$Dzilam <- as.numeric(DatosPawayCerosRegion$Dzilam)
DatosPawayCerosRegion$Palmar <- as.numeric(DatosPawayCerosRegion$Palmar)

str(DatosPawayCerosRegion)

upset(DatosPawayCerosRegion, order.by = "freq",  text.scale = c(1.3, 1.3, 1, 1, 2, 2))

PawayPalmar<- DatosPawayCerosRegion %>% filter(DatosPawayCerosRegion$Palmar > 0 )
PawayPalmar$Rutas<- row.names(PawayPalmar)
PawayPalmar=PawayPalmar$Rutas


PawayDzilam<- DatosPawayCerosRegion %>% filter(DatosPawayCerosRegion$Dzilam > 0 )
PawayDzilam$Rutas<- row.names(PawayDzilam)
PawayDzilam=PawayDzilam$Rutas

InterseccionPwayEstatus <- intersect(PawayAlterado, PawayConservado)
InterseccionPwayEstatus

UnicoPawayPalmar= setdiff(PawayPalmar,PawayDzilam)
UnicoPawayDzilam= setdiff(PawayDzilam,PawayPalmar)

write.csv(UnicoPawayDzilam, './RutasUnicasDzilam.csv')
write.csv(UnicoPawayPalmar, './RutasUnicasPalmar.csv')