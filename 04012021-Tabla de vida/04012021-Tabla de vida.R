#--------------------------------------------------------------------------
# Creacion:          04-01-2020
# Autor:             Julio C.
# Contacto:          jcms2665@gmail.com
# Objetivo:          Tabla de vida
# Datos:             tabla.csv
# Github:            http://rpubs.com/jcms2665/LifeTable
#--------------------------------------------------------------------------


#0. Entorno de trabajo
rm(list=ls())     
graphics.off()    


#1. Cargar paquetes
library(dplyr)
library(foreign)
library(RCurl)
library(foreign)


#2. Cargar base de datos
url <- "https://raw.githubusercontent.com/jcms2665/DemographyCourse/master/Data/tabla.csv"
tabla <- mutate(read.csv(textConnection(getURL(url))), n = c(diff(edad), 0))


#3. Examinar la base de datos
View(tabla)
x<-head(tabla,5)


#4. Calcular las tasas específicas de mortalidad
tabla <- mutate(tabla, nmx = D/N)


#5. Obtener el número promedio de años persona vividos en el intervalo por aquellos que murieron, es decir: nLx 
#Los vamos a tomar prestados de Keyfitz y Flieger (1971)
kfnax <- read.dta("http://data.princeton.edu/eco572/datasets/kfnax.dta")
names(kfnax)[1]<-"edad"


#6. Unimos los años persona vividos a la tabla que teníamos usando la edad como pivote
tabla <- inner_join(tabla, kfnax, by="edad")


#7. Checamos los años persona vividos para las edades de 0-1 y 1-4
cond <- rep(tabla[1,"nmx"] >= 0.107, 2) # Usamos una expresión como ayuda
tabla[1:2,"nax"] <- ifelse(cond, c(0.330, 1.352), tabla[1:2,"nax"] <- c(0.045, 1.651) + c(2.684, -2.816)* tabla[1,"nmx"])
last <- nrow(tabla) 
tabla[last,"nax"] <- 1/ tabla[last,"nmx"]


#8. Calculamos las qx y px
tabla <- mutate(tabla, q = n * nmx/(1 + (n - nax) * nmx), p = 1 - q)


#9.Bajo el supuesto de tener una tabla cerrada, consideramos que todos mueren q=1 y nadie sobrevive p=0
tabla[last, c("q","p")] = c(1, 0)


#10. Elegimos un radix l0
tabla <- mutate(tabla, lx = 100000 * cumprod( c(1, p[-last])))


#11. Calcular el número de personas que murieron entre las edades x y x+n
tabla <- mutate(tabla, d = c(-diff(lx), lx[last]))


#12.Calcular los sobrevivientes y Años Persona Vividos arriba de la edad x, es decir: nTx=???a=xnLa
tabla <- mutate(tabla, L =  (lx - d) * n +  d * nax,T = sum(L) - cumsum(L) + L)

#13. Esperanza de vida ex=nTx / lx
tabla <- mutate(tabla, e = T/lx)

#14. Tabla de vida
View(tabla)
