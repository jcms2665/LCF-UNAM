#--------------------------------------------------------------------------
# Creacion:          02-12-2020
# Autor:             Julio C.
# Contacto:          jcms2665@gmail.com
# Objetivo:          Análisis de conglomerados: k-medias
# Datos:             Latinobarometro_2018_Esp_R_v20190303.Rds
# Github:            https://github.com/jcms2665/LCF-UNAM/tree/main/02122020-KMEANS
#--------------------------------------------------------------------------

#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#     2. Arreglo de la base de datos
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
library(htmltools)
library(klaR)


#1. Cargar base

setwd("D:/OneDrive - El Colegio de México A.C/5. Proyectos/2020/18. LCF/2 Noviembre/30112020-FA-Ordinal")
latino <- readRDS("Latinobarometro_2018_Esp_R_v20190303.Rds")



# Variables:

# P15STGBSC.A --- Confianza en las Fuerzas Armadas
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

# Filtrar paises: Argentina, Bolivia, Brasil
dat<-latino%>%filter(as.numeric(IDENPA)==32 | as.numeric(IDENPA)==68 | as.numeric(IDENPA)==76)


# Filtrar variables
var<-c("REEDAD","SEXO","IDENPA","P15STGBSC.A","P15STGBSC.B","P15STGBSC.C","P15STGBSC.D", "P15STGBSC.E")
dat1<-dat[,var]
names(dat1)<-c("Edad","Sexo","País","Fuerzas Armadas","Policía","Iglesia","Congreso","Gobierno")



# Quitar respuestas inválidas
dat1[dat1 <=0] <- NA
dat1<-dat1%>%drop_na()


# Etiquetar variables (para identificar a la unidad de análisis: personas)
dat1$Edad<-factor(dat1$Edad,levels = c(1,2,3,4), labels = c("16-25 años","26-40 años","41-60 años","60 y más"))
dat1$Sexo<-factor(dat1$Sexo,levels = c(1,2), labels = c("Hombre","Mujer"))
dat1$País<-factor(dat1$País,levels = c(32,68,76), labels = c("Argentina","Bolivia","Brasil"))
View(dat1)

# Filtrar variables para el análisis
dat<-dat1[,4:8]
View(dat)

#5. Aplico el algoritmo de k-medias con 3 grupos
fit <-kmodes(dat, 3)


#6. A la base le pegamos los grupos
dat.grupos <- data.frame(dat1, fit$cluster)


#7. Interpretación
View(dat.grupos)
table(dat.grupos$fit.cluster,dat.grupos$Edad)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$Sexo)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$País)%>%prop.table(1)%>%`*`(100)%>%round(1)













