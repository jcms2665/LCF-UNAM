#--------------------------------------------------------------------------
# Creacion:          25-11-2020
# Autor:             Julio C.
# Contacto:          jcms2665@gmail.com
# Objetivo:          An谩lisis Factorial Exploratorio
# Datos:             pacientes.csv
# Ubicaci贸n:         https://github.com/jcms2665/LCF-UNAM/    
#--------------------------------------------------------------------------

#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#     2. Ajuste del modelo
#         2.1 Normalizaci贸n 
#         2.2 Prueba Kaiser-Meyer-Olkin
#     3. Factorial
#     4. Interpretaci贸n
#--------------------------------------------------------------------------


#0.  Entorno de trabajo

rm(list=ls())     
graphics.off()    

library(foreign)
library(factoextra)
library(ggplot2)
library(psych)
library(dplyr)

#1. Cargar base

setwd("D:/OneDrive - El Colegio de M閤ico A.C/5. Proyectos/2020/18. LCF/2 Noviembre/Github/25112020-AF")
pacientes<-read.csv("pacientes.csv", sep=",",header = TRUE)
View(pacientes)

#2. Ajuste del modelo

#2.1 Normalizaci贸n
norm01 <- function(x){(x-min(x))/(max(x)-min(x))}
pacientes_norm<-data.frame(apply(pacientes,2,norm01))

apply(pacientes_norm, 2, max)%>%round(2)


#2.2 Prueba Kaiser-Meyer-Olkin
#   La prueba de 0 a 1 e indica si las correlaciones entre variables son
#   suficientemente peque帽as. Valores menores a 0.5 no deben ser inclu铆dos

KMO(pacientes_norm)


#2.3 Numero de factores (Grafica de sedimentacion)
corr<-round(cor(pacientes_norm),2)
aucor=eigen(corr)
plot(1:5,aucor$values,type="l",xlab="Factores",ylab="Autovalores")


#3. Factorial
fit <- factanal(pacientes, 2, rotation="varimax")


#4. Interpretacion 
load <- fit$loadings[,1:2]
load
