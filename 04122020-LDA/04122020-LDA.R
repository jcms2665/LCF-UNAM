#--------------------------------------------------------------------------
# Creacion:          03-12-2020
# Autor:             Julio C.
# Contacto:          jcms2665@gmail.com
# Objetivo:          An�lisis Discriminante
# Datos:             Alumnos.csv
# Github:            
#--------------------------------------------------------------------------

#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#     2. Aplicar el modelo
#     3. Nueva observaci�n
#     4. Predicci�n
#--------------------------------------------------------------------------



#0.  Entorno de trabajo
rm(list=ls())     
graphics.off()    

library(foreign)
library(ggplot2)
library(MASS)

setwd("D:/OneDrive - El Colegio de M�xico A.C/5. Proyectos/2020/18. LCF/2 Noviembre/Github/04122020-LDA")
Alumnos<-read.csv("Alumnos.csv")
View(Alumnos)

#2. Aplicar el modelo

Alumnos$aprobado<-factor(Alumnos$aprobado, levels = c(0,1), labels = c("Reprueba","Aprueba"))

dis=lda(aprobado~hrs_estudio+hrs_fiesta+hrs_camino_escuela+calificacion_mate, data=Alumnos,prior=c(0.5,0.5))
dis

#3. Nueva observaci�n
#   Supongamos que entra un alumno nuevo. Y que:
#     hrs_estudio =4.3
#     hrs_fiesta  =1.5
#     hrs_camino  =1.3
#     calif_mate  =8.0

nuevo.alumno=rbind(c(4.3,1.5,1.3,8.0))

colnames(nuevo.alumno)=colnames(Alumnos[,2:5])
nuevo.alumno=data.frame(nuevo.alumno)

#4. Predicci�n
predict(dis,newdata =nuevo.alumno)








# Ejercicio

#Llega otro nuevo alumno con los siguientes datos:
#     hrs_estudio =1.3
#     hrs_fiesta  =0.5
#     hrs_camino  =0.3
#     calif_mate  =7.0


