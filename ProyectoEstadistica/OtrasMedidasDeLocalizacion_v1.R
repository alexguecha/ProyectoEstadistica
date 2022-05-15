#Base de datos
library(readxl)
BDa2censo <- read_excel("GitHub/ProyectoEstadistica/ProyectoEstadistica/BD inversionistas depurada V6 anonimizada.xlsx")


# Media Geométrica

#Una de sus principales ventajas es que es menos sensible a valores extremos (muy grandes o muy pequeños) 
#que podrían alterar la media de una muestra estadística. Por el contrario, su principal desventaja es que 
#no puede utilizarse con números negativos.

#Función
geometric<-function(x) exp(sum(log(x))/length(x))

#-------------------------------------------------------------------------------

# Media Armónica

#La media Armónica siempre va a ser menor que la media
#geométrica porque es el reciproco de la media aritmética
#con respecto a los datos 


#Función
armonic<-function(x) 1/mean(1/x)


#-------------------------------------------------------------------------------

#Media Recortada en 100α% :

#Función:
recortada<-function(x) mean(x, trim=0.1)


#-------------------------------------------------------------------------------

#Media Winsorizada:

library(psych)#Paquete útil para calcular la media Winsorizada:

#Funcion
winsorizada<-function(x) winsor.mean(x, trim= 0.1, na.rm = TRUE)

#--------------------------------------------------------------------------------
#Cálculo Resumen de medidas de localización

options(scipen = 999)
ResumenVaribles<- c("Edad","Tasa","Ingresos Mes", "Patrimonio","Monto Inversion","Cantidad Inversiones" )
ResumenPromedio<- c(mean(BDa2censo$Edad),mean(BDa2censo$Tasa),mean(BDa2censo$IngresosMes),mean(BDa2censo$Patrimonio), mean(BDa2censo$MontoInversion),mean(BDa2censo$CantidadInversiones))
ResumenMediana<- c(median(BDa2censo$Edad),median(BDa2censo$Tasa),median(BDa2censo$IngresosMes),median(BDa2censo$Patrimonio), median(BDa2censo$MontoInversion),median(BDa2censo$CantidadInversiones))
ResumenGeometrica<- c(geometric(BDa2censo$Edad),geometric(BDa2censo$Tasa),geometric(BDa2censo$IngresosMes),geometric(BDa2censo$Patrimonio), geometric(BDa2censo$MontoInversion),geometric(BDa2censo$CantidadInversiones))
ResumenArmonica<- c(armonic(BDa2censo$Edad),armonic(BDa2censo$Tasa),armonic(BDa2censo$IngresosMes),armonic(BDa2censo$Patrimonio), armonic(BDa2censo$MontoInversion),armonic(BDa2censo$CantidadInversiones))
ResumenRecortada<- c(recortada(BDa2censo$Edad),recortada(BDa2censo$Tasa),recortada(BDa2censo$IngresosMes),recortada(BDa2censo$Patrimonio), recortada(BDa2censo$MontoInversion),recortada(BDa2censo$CantidadInversiones))
ResumenWinsorizada<- c(winsorizada(BDa2censo$Edad),winsorizada(BDa2censo$Tasa),winsorizada(BDa2censo$IngresosMes),winsorizada(BDa2censo$Patrimonio), winsorizada(BDa2censo$MontoInversion),winsorizada(BDa2censo$CantidadInversiones))

resumenMedia <- data.frame(ResumenVaribles ,ResumenPromedio,ResumenMediana,ResumenGeometrica,ResumenArmonica,ResumenRecortada,ResumenWinsorizada)

resumenMedia
