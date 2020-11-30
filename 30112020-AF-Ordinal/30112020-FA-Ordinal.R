#--------------------------------------------------------------------------
# Creacion:          28-11-2020
# Autor:             Julio C.
# Contacto:          jcms2665@gmail.com
# Objetivo:          Análisis Factorial para variables categóricas
# Datos:             Latinobarometro_2018_Esp_R_v20190303.Rds
# Github:            https://github.com/jcms2665/LCF-UNAM/tree/main/30112020-AF-Ordinal
#--------------------------------------------------------------------------

#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#     2. Preparación de la base de datos
#     3. Correlación policórica
#     4. Ajuste del modelo
#--------------------------------------------------------------------------


#0.  Entorno de trabajo

rm(list=ls())     
graphics.off()    

library(foreign)
library(ggplot2)
library(psych)
library(dplyr)
library(psych)
library(tidyr)


#1. Cargar base

setwd("D:/OneDrive - El Colegio de México A.C/5. Proyectos/2020/18. LCF/2 Noviembre/30112020-FA-Ordinal")
latino <- readRDS("Latinobarometro_2018_Esp_R_v20190303.Rds")



# Variables:

# P15STGBSC.A --- Confianza en el Ejército
# P15STGBSC.B --- Confianza en la Policía
# P15STGBSC.C --- Confianza en la Iglesia
# P15STGBSC.D --- Confianza en el Congreso
# P15STGBSC.E --- Confianza en el Gobierno

# Respuestas:
# 1.- Mucha confianza
# 2.- Algo de confianza
# 3.- Poca confianza
# 4.- Ninguna confianza
# -1-.- No sabe
# -2-.- No responde
# -4-.- No preguntada


#2. Arreglo de la base de datos

# Filtrar por país: México y Brasil
dat<-latino%>%filter(as.numeric(IDENPA)==862 | as.numeric(IDENPA)==76)


# Filtrar variables
var<-c("P15STGBSC.A","P15STGBSC.B","P15STGBSC.C","P15STGBSC.D", "P15STGBSC.E")
dat<-dat[,var]
names(dat)<-c("Ejército","Policía","Iglesia","Congreso","Gobierno")


# Quitar respuestas inválidas
dat[dat <=0] <- NA
dat<-dat%>%drop_na()

# Tabulados

knitr::kable(table(dat$Ejército))
knitr::kable(table(dat$Iglesia))


#3. Correlación policórica 
poly_cor = polychoric(dat)
rho = poly_cor$rho
cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Correlación tetracórica", show.legend = FALSE)


#4. Ajuste del modelo
poly_model = fa(dat, nfactor=2, cor="poly", fm="mle", rotate = "none")
poly_model$loadings
fa.diagram(poly_model)


























