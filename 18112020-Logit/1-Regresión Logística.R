#--------------------------------------------------------------------------
# Creacion:          18-11-2020
# Autor:             Julio C.
# Contacto:          jcms2665@gmail.com
# Objetivo1:         Implementar un modelo de regresión logística
# Datos:             compradores.csv
# Ubicación:         /cloud/project/18112020-Logit/     
#--------------------------------------------------------------------------

#               CONTENIDO

#       0.  Preparar entorno de trabajo
#       1. Cargar librerías
#       2. Cargar base de datos
#       3. Seleccionar covariables
#       4. Seleccionar variables
#           4.1 Variable dependiente
#           4.2 Covariables
#       5. Ajuste del modelo
#       6. Interpretación
#--------------------------------------------------------------------------------

# PREGUNTA DE INVESTIGACIÓN:¿Qué factores influyen para que una persona compre?
# 


#0.  Preparar entorno de trabajo

rm(list=ls())                  
options(warn=-1)              


#1. Cargar librerías

library(foreign)                 
library(dplyr) 
library(stats) 

#2. Cargar base de datos

base <- read.csv("/cloud/project/18112020-Logit/compradores.csv")
View(base)

# variables
# compra = Compra de las personas
#           1 = Sí  
#           0 = No 
# cliente = Tipo de cliente
#           1 = Ocasional  
#           2 = Regular 
#           3 = Frecuente 
# zona = Lugar donde viven las personas
#           1 = Sur  
#           2 = Norte 
# mascota = Tipo de mascota
#           1 = Perro  
#           2 = Gato 
#           3 = Otro

#3. Etiquetar variables (metodología)

base$compra<-factor(base$compra, levels = c(0,1), labels = c("No","Sí"))
base$cliente<-factor(base$cliente, levels = c(1,2,3), labels = c("Ocasional","Regular","Frecuente"))
base$zona<-factor(base$zona, levels = c(1,2), labels = c("Sur","Norte"))
base$mascota<-factor(base$mascota, levels = c(1,2,3), labels = c("Perro","Gato","Otro"))


#4. Seleccionar variables (2 variables)

compradores<-base[c("compra","cliente","zona")]  
rm("base")

View(compradores)


#4.1 Variable dependiente

table(compradores$compra)

#4.2 Covariables

table(compradores$cliente)
table(compradores$zona)


#5. Ajuste del modelo


regresion <- glm(compra ~cliente + zona, 
                 data = compradores, family = "binomial")
summary(regresion)


#6. Interpretación

momios<-exp(coefficients(regresion))%>%round(digits = 4)%>%data.frame()
momios
