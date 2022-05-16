# Media Geométrica


#Una de sus principales ventajas es que es menos sensible a valores extremos (muy grandes o muy pequeños) 
#que podrían alterar la media de una muestra estadística. Por el contrario, su principal desventaja es que 
#no puede utilizarse con números negativos.

#Función
geometric<-function(x) exp(sum(log(x))/length(x))

#Variable : Edad
geometric(BDa2censo$Edad)
#Media Geométrica de :   36.03653
#Mean                    37.18176
#Median                  35.00000

#Variable : Tasa
geometric(BDa2censo$Tasa)
#Media Geométrica de :   0.103910
#Mean                    0.104405
#Median                  0.100000

#Variable : Ingresos Mes
geometric(BDa2censo$IngresosMes)
#Media Geométrica de :   $5017014
#Mean                    $7590004
#Median                  $4878000

#Variable : Patrimonio
geometric(BDa2censo$Patrimonio)
#Media Geométrica de :   $64676965
#Mean                    $1.570082e+08
#Median                  $1.000000e+08

#Variable : MontoInversion
geometric(BDa2censo$MontoInversion)
#Media Geométrica de :      $611385.7
#Mean                       $967367.6
#Median                     $500000.0

#Variable : CantidadInversiones
geometric(BDa2censo$CantidadInversiones)
#Media Geométrica de :   7.199434
#Mean                    12.19810
#Median                  8.000000

#-------------------------------------------------------------------------------

# Media Armónica

#La media Armónica siempre va a ser menor que la media
#geométrica porque es el reciproco de la media aritmética
#con respecto a los datos 


#Función
armonic<-function(x) 1/mean(1/x)

#Variable : Edad
armonic(BDa2censo$Edad)
#Media Armónica:   35.015

#Variable : Tasa
armonic(BDa2censo$Tasa)
#Media Armónica:   0.1034249

#Variable : Ingresos Mes
armonic(BDa2censo$IngresosMes)
#Media Armónica:   $3793266

#Variable : Patrimonio
armonic(BDa2censo$Patrimonio)
#Media Armónica:   $14267648

#Variable : MontoInversion
armonic(BDa2censo$MontoInversion)
#Media Armónica:   $430745.5

#Variable : CantidadInversiones
armonic(BDa2censo$CantidadInversiones)
#Media Armónica:   3.941165

#-------------------------------------------------------------------------------

#Media Recortada en 100α% :

#Función:
BDa2censo_ord<-BDa2censo[order(BDa2censo$Edad, BDa2censo$Tasa, BDa2censo$IngresosMes, BDa2censo$Patrimonio, BDa2censo$MontoInversion, BDa2censo$CantidadInversiones ), ]
recortada<-function(x) mean(x, trim=0.1)

#Variable : Edad
recortada(BDa2censo_ord$Edad)
#Media Recortada:   35.98493

#Variable : Tasa
recortada(BDa2censo_ord$Tasa)
#Media Recortada:   0.1040474

#Variable : Ingresos Mes
recortada(BDa2censo_ord$IngresosMes)
#Media Recortada:   $5523128

#Variable : Patrimonio
recortada(BDa2censo_ord$Patrimonio)
#Media Recortada:   $122361003

#Variable : MontoInversion
recortada(BDa2censo_ord$MontoInversion)
#Media Recortada:   $728999.5

#Variable : CantidadInversiones
recortada(BDa2censo_ord$CantidadInversiones)
#Media Recortada:   10.00131

#-------------------------------------------------------------------------------

#Media Winsorizada:

library(psych)#Paquete útil para calcular la media Winsorizada:

winsorizada<-function(x) winsor.mean(x, trim= 0.1, na.rm = TRUE)

#Variable : Edad
winsorizada(BDa2censo$Edad)
#Media winsorizada:   36.48795

#Variable : Tasa
winsorizada(BDa2censo$Tasa)
#Media winsorizada:   0.1044279

#Variable : Ingresos Mes
winsorizada(BDa2censo$IngresosMes)
#Media winsorizada:   $6008503

#Variable : Patrimonio
winsorizada(BDa2censo$Patrimonio)
#Media winsorizada:   $139643203

#Variable : MontoInversion
winsorizada(BDa2censo$MontoInversion)
#Media winsorizada:   $813199.6

#Variable : CantidadInversiones
winsorizada(BDa2censo$CantidadInversiones)
#Media winsorizada:   11.00105
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
