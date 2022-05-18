
library(readxl)
BDa2censo<- read_excel("G:/Mi unidad/UCENTRAL/1ErSemestre/FUND_ ESTADÍSTICA_ANALÍTICA_DE_DATOS/ProyectoFundEstadistica/BD inversionistas depurada V6 anonimizada.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric"))

BDa2censo$Inv_Recurrente<-BDa2censo$CodEsInversionistaRecurrente

BDa2censo$Inv_Recurrente[BDa2censo$Inv_Recurrente=="Inversionista No Recurrente"]<-0
BDa2censo$Inv_Recurrente[BDa2censo$Inv_Recurrente=="Inversionista Recurrente"]<-1
BDa2censo$Inv_Recurrente<-as.numeric(BDa2censo$Inv_Recurrente)
BDa2censo$SectorCampaña<-as.factor(BDa2censo$SectorCampaña)
BDa2censo$Propósito<-as.factor(BDa2censo$Propósito)


library(fdth)
library(caTools)
library(ROCR)
library(pROC)
library(magrittr) 
library(InformationValue)


## De acuerdo a los objetivos planteados para el presente estudio existe un alto interés por parte de la compañía en conocer si existe alguna variable que nos permita predecir la fidelización de los clientes con el producto, entendiendo que la mejor variable que nos puede hablar de fidelización es la recurrencia de inversión. Por esto se analizarán las variables para identificar si alguna de ellas o un conjunto de ellas permiten predecir el comportamiento de inversión referente a la recurrencia.Siendo asi plantearemos un modelo de regresion logisitica que no permita estimar la probabilidad de recurrencia de un inversionista.

set.seed(88)
split <- sample.split(BDa2censo$Inv_Recurrente, SplitRatio = 0.75)
training <- subset(BDa2censo,split =="TRUE")
testing <- subset(BDa2censo,split=="FALSE")


logMultiple=glm(data=training,Inv_Recurrente~MontoInversion+Edad+IngresosMes+Patrimonio+SectorCampaña+Propósito, family = binomial)
summary(logMultiple)

#Desde el modelo de regresion logistica ajustado se encuentra que el sector económico de la campaña es una variable con un nivel de significancia en la variable respuesta de recurrencia del inversionista. De la misma forma en que muestra significancia respecto a los propósitos de inversión de diversificacion del portafolio y el motivador de hacer crecer el capital.

#De otro lado, las variables, monto inversión, edad, patrimonio e ingresos mes tambien muestran significancia respecto a la variable asociada a si el individuo es inversionista recurrente o no


## Probaremos con otros modelos descartando algunas variables con el animo de comparar el criterio de perdida de informacion AIC y escoger el mejor modelo
logMultiple1=glm(data=training,Inv_Recurrente~MontoInversion+Edad+IngresosMes+Patrimonio, family = binomial)
summary(logMultiple1)


logMultiple2=glm(data=training,Inv_Recurrente~MontoInversion+Edad+IngresosMes+Patrimonio, family = binomial)
summary(logMultiple2)


#De acuerdo a lo anterior encontramos que el modelo en donde menor pérdida de información es el designado con nombre: logMultiple , en donde el AIC es de 22826. Asociado a las variables MontoInversion +  Edad + IngresosMes + Patrimonio + SectorCampaña + Propósito  . No obstatne dentro de las variables cualitativas no todas sus categorías tienen un nivel de significancia relevante, por ejemplo para la variable  sector de campaña, las categorías de variables con mayor nivel de significancia son: Educación, Información y Comunicación, y Servicios Domésticos. Para el caso del propósito se destacan: Diversificar Portafolio y Pertenecer a la comunidad de a2censo  


#Resultando un modelo de la forma:

# Inv_Recurrente = e (-1,925-0,0000002243*MontoInversion+0,01733*Edad+0,000000009223*IngresosMes+0,000000002252*Patrimonio+0,2879*SectorCampañaAlcantarillado-0,02954*SectorCampañaAlojamiento Y Servicios De Comida+0,51*SectorCampañaAutomotriz+0,3723*SectorCampañaComercio al por mayor+0,04463*SectorCampañaComercio Al Por Menor+1,003*SectorCampañaEducación+0,5915*SectorCampañaEntretenimiento+0,6088*SectorCampañaIndustrías Creativas Y Culturales+0,7096*SectorCampañaInformación y Comunicaciones-0,006026*SectorCampañaInmobiliarias+0,1012*SectorCampañaInvestigación y Ciencias+0,4045*SectorCampañaManufactura+0,4026*SectorCampañaSalud-0,2092*SectorCampañaSaneamiento Ambiental-0,2422*SectorCampañaServicios Administrativos+0,8118*SectorCampañaServicios Domésticos+0,2204*SectorCampañaServicios Energéticos+0,1203*SectorCampañaTransporte y Almacenamiento+0,1616*PropósitoAprender de financiación+0,3406*PropósitoDiversificar mi portafolio de inversión-0,4473*PropósitoHacer parte de la comunidad a2censo-0,03824*PropósitoRentabilizar portafolio de inversión) / (1+e(-1,925-0,0000002243*MontoInversion+0,01733*Edad+0,000000009223*IngresosMes+0,000000002252*Patrimonio+0,2879*SectorCampañaAlcantarillado-0,02954*SectorCampañaAlojamiento Y Servicios De Comida+0,51*SectorCampañaAutomotriz+0,3723*SectorCampañaComercio al por mayor+0,04463*SectorCampañaComercio Al Por Menor+1,003*SectorCampañaEducación+0,5915*SectorCampañaEntretenimiento+0,6088*SectorCampañaIndustrías Creativas Y Culturales+0,7096*SectorCampañaInformación y Comunicaciones-0,006026*SectorCampañaInmobiliarias+0,1012*SectorCampañaInvestigación y Ciencias+0,4045*SectorCampañaManufactura+0,4026*SectorCampañaSalud-0,2092*SectorCampañaSaneamiento Ambiental-0,2422*SectorCampañaServicios Administrativos+0,8118*SectorCampañaServicios Domésticos+0,2204*SectorCampañaServicios Energéticos+0,1203*SectorCampañaTransporte y Almacenamiento+0,1616*PropósitoAprender de financiación+0,3406*PropósitoDiversificar mi portafolio de inversión-0,4473*PropósitoHacer parte de la comunidad a2censo-0,03824*PropósitoRentabilizar portafolio de inversión))






# Elaboramos las respectivas predicciones de acaurdo al modelo escogido para evalaur el desempeño del mismo
predict <- predict(logMultiple, type = 'response')
predict2 <- as.data.frame(predict(logMultiple, type = 'response'))

#Tambien definimos el valor del punto de corte o treshold para determinar la clasificaicon de la variable respuesta, con la funcion optCutOff, q nos permite determinar de manera automatica el punto de corte para la clasificacio de las observaciones del modelo propuesto.
optCutOff <- optimalCutoff(training$Inv_Recurrente, predict)[1]

optCutOff

# Por medio de la siguente matriz de confusion podemos evalaur el ajsute del modelo , tomado como criterio de corte el valor estimado 0.457 en el paso anterior. 
confusionMatrix(training$Inv_Recurrente, predict, threshold = optCutOff)
sensitivity(training$Inv_Recurrente, predict, threshold = optCutOff)
#Se concluye que el modelo acertó en el 27% de las observaciones de verdaderos positivos ( Grado de Sensibilidad), es decir solo identifica un 27% de los positivos reales
specificity(training$Inv_Recurrente,predict, threshold = optCutOff)
# La especificidad mide la tasa de verdadero negativos, es decir para este caso el modelo identifica el 89% de los negativos reales

negPredValue(training$Inv_Recurrente,predict, threshold = optCutOff)
#
#La curva característica operativa o curva ROC  permite identificar el rendimiento de un clasificador, en este caso el modelo de regresión logística. El área bajo la curva mide la relación entre la tasa de verdaderos positivos y la tasa de falsos positivos (1-Especificidad) en varios valores de umbral

plotROC(training$Inv_Recurrente, predict)

#El criterio de precision del modelo concluye que el 68% de las observaciones son acertadas. Y asu vez son evidencia de un modelos q no presta un nivel adecudo para la toma de decisiones.
#A pesar de haber escogido un modelos con el menor criterio de pérdida de informacion aún el nivel de precision no es suficiente para tomar decisiones fiables en el futuro.
