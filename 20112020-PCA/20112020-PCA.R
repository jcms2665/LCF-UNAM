#--------------------------------------------------------------------------
# Creacion:          18-11-2020
# Autor:             Julio C.
# Contacto:          jcms2665@gmail.com
# Objetivo:          Análisis de Componentes Principales
# Datos:             calificacion.csv
# Ubicación:             
#--------------------------------------------------------------------------

#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#         1.1 Opción 1
#         1.2 Opción 2
#     2. Normalización
#     3. Ajuste del modelo
#         3.1 Gráfico de sedimentación 
#         3.2 Componentes principales
#     4. Correlación: CP vs Datos originales

#--------------------------------------------------------------------------


#0.  Entorno de trabajo

rm(list=ls())     
graphics.off()    

library(foreign)
library(factoextra)
library(dplyr)
library(knitr)


#1. Cargar base

#1.1 Opción 2
horario<-read.csv("D:/OneDrive - El Colegio de México A.C/5. Proyectos/2020/18. LCF/2 Noviembre/Github/20112020-PCA/horario.csv", sep=",",header = TRUE)
View(horario)

#2. Ajuste del modelo

#2.1 Correlaciones
cor(horario)%>%round(2)

#2.2 Normalización
norm01 <- function(x){(x-min(x))/(max(x)-min(x))}
horario_norm<-data.frame(apply(horario,2,norm01))
apply(horario_norm, 2, min)%>%round(2)
apply(horario_norm, 2, mean)%>%round(2)
apply(horario_norm, 2, max)%>%round(2)

#2.3 Análisis de componentes principales
acp<-prcomp(horario_norm)
acp

#2.4 Valores de los componentes
acp$x



#3. Resultados

#3.1 Gráfico de sedimentación (Varianza)
screeplot(acp,type="lines")


#3.2 Componentes principales (los 2 primeros)
cp<-data.frame(acp$x)
cp<-cp[,1:2]


#4. Correlación: Constructos vs Datos originales
cor(horario_norm, cp,use = "everything",method = c("pearson"))







