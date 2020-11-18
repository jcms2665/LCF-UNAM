#--------------------------------------------------------------------------
# Creacion:          11-11-2020
# Autor:             Julio C.
# Contacto:          jcms2665@gmail.com
# Objetivo1:         Implementar un modelo de regresión logística
# Datos:             fumadores.csv
# Ubicación:         /cloud/project/18112020-Logit/ 
#--------------------------------------------------------------------------

#               CONTENIDO

#       0.  Preparar entorno de trabajo
#       1. Cargar librerías
#       2. Cargar base de datos
#       3. Seleccionar covariables
#       4. Variables
#           4.1 Variable dependiente
#           4.2 Covariables
#       5. Ajuste del modelo
#       6. Interpretación
#--------------------------------------------------------------------------------

# PREGUNTA DE INVESTIGACIÓN:¿Qué factores influyen para que una persona fume?
# 


#0.  Preparar entorno de trabajo

rm(list=ls())                  
options(warn=-1)              


#1. Cargar librerías

library(foreign)                 
library(dplyr) 
library(stats) 

#2. Cargar base de datos

base <-read.csv("/cloud/project/18112020-Logit/fumadores.csv")
View(base)

# variables
# fuma = fumadores
#           1 = Sí  
#           0 = No 
# edad = Edad de las personas
#           1 = Niño  
#           2 = Jóven  
#           3 = Adulto 
# civil = Estado civil
#           1 = soltero  
#           2 = casado 
# actividad = Tipo de actividad
#           1 = trabaja  
#           2 = estudia 


#3. Etiquetar variables

base$fuma<-factor(base$fuma, levels = c(0,1), labels = c("No","Sí"))
base$edad<-factor(base$edad, levels = c(1,2,3), labels = c("Niño","Jóven","Adulto"))
base$civil<-factor(base$civil, levels = c(1,2), labels = c("soltero","casado"))
base$actividad<-factor(base$actividad, levels = c(1,2), labels = c("trabaja","estudia"))


#4. Seleccionar variables (2 variables)

fumadores<-base[c("fuma","___","___")]  

rm("base")



#4.1. Tipo de datos

sapply(fumadores, class) 


#4.1 Variable dependiente

table(fumadores$fuma)

#4.2 Covariables

table(fumadores$___)
table(fumadores$___)


#5. Ajuste del modelo


regresion <- glm(fuma ~___+___, 
                 data = fumadores, family = "binomial")
summary(regresion)


#6. Interpretación

momios<-exp(coefficients(regresion))%>%round(digits = 4)%>%data.frame()
momios
