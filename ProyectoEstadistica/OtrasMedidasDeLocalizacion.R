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
#Media Armónica:   $14267648

