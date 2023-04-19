library(dplyr)
library(UpSetR)

Nive3Puntos <- read.csv("../../../../Downloads/level-3.csv", row.names = 1)


Nivel3PuntosCeros <- Nive3Puntos %>% mutate_all(~ifelse(. > 0, 1, .))
Nivel3PuntosCerost <- as.data.frame(t(Nivel3PuntosCeros))

upset(Nivel3PuntosCerost, nsets = 8, order.by = "freq", 
      text.scale = c(1.3, 1.3, 1, 1, 2, 1))


#######################################################

Nivel3Sitio <- read.csv("../../../../Downloads/level-3Sitio.csv", row.names=1)
Nivel3SitioCero <- Nivel3Sitio %>% mutate_all(~ifelse(. > 0, 1, .))
Nivel3SitioCerost <- as.data.frame(Nivel3SitioCero)

upset(Nivel3SitioCerost, nsets = 4, order.by = "freq", 
      text.scale = c(1.3, 1.3, 1, 1, 2, 2))

######################################################

Nivel3Estatus <- read.csv("../../../../Downloads/level-3Estatus.csv", row.names=1)

Nivel3EstatusCero <- Nivel3Estatus %>% mutate_all(~ifelse(. > 0, 1, .))
Nivel3EstatusCerost <- as.data.frame(Nivel3EstatusCero)

upset(Nivel3EstatusCerost, nsets = 4, order.by = "freq", 
      text.scale = c(1.3, 1.3, 1, 1, 2, 5
      ))

#### Para obetener los 11 que se comparten ##########

#Se hacen los vectores donde sólo estan las clases que son >0 por sitio de 
#muestreo

Conservado <-  Nivel3Estatus %>% filter(Nivel3Estatus$Conservado > 0 )
Conservado$Clases <- rownames(Conservado)
ClasesConservado <- Conservado$Clases

Alterado <- Nivel3Estatus %>% filter(Nivel3Estatus$Alterado > 0)
Alterado$Clases <- rownames(Alterado)
ClasesAlterado <- Alterado$Clases

IntersecciónEstatus <- intersect(ClasesAlterado, ClasesConservado)
IntersecciónEstatus

################## para sitio de muestreo###########

DzilamBocas <-  Nivel3Sitio %>% filter(Nivel3Sitio$DzilamBocas > 0 )
DzilamBocas$Clases <- rownames(DzilamBocas)
ClasesDzilamBocas <- DzilamBocas$Clases

DzilamPuerto <-  Nivel3Sitio %>% filter(Nivel3Sitio$Dzilam.Puerto > 0 )
DzilamPuerto$Clases <- rownames(DzilamPuerto)
ClasesDzilamPuerto <- DzilamPuerto$Clases

SisalPuerto <-  Nivel3Sitio %>% filter(Nivel3Sitio$Sisal.Puerto > 0 )
SisalPuerto$Clases <- rownames(SisalPuerto)
ClasesSisalPuerto <- SisalPuerto$Clases

ElPalmar <-  Nivel3Sitio %>% filter(Nivel3Sitio$El.Palmar > 0 )
ElPalmar$Clases <- rownames(ElPalmar)
ClasesElPalmar <- ElPalmar$Clases

ClasesPorSitio <- list( EL_Palmar = ClasesElPalmar, 
                        Sisal_Puerto = ClasesSisalPuerto, 
                        Dzilam_Puerto = ClasesDzilamPuerto,
                        Dzilam_Bocas = ClasesDzilamBocas) 

CompartidosPorSitio <- intersect(intersect(intersect(ClasesDzilamBocas, ClasesDzilamPuerto),ClasesElPalmar),ClasesSisalPuerto)

#SEgun ChatGTP se puede hacer mas eficiente con la funci+on reduce 

CompartidosPorSitio2 <- Reduce(intersect, list(ClasesDzilamBocas, ClasesDzilamPuerto, ClasesElPalmar, ClasesSisalPuerto))

write.csv( CompartidosPorSitio,  "../../../Tesis/ClasesCompartidasPorSitio")

CompartidosPorSitio
