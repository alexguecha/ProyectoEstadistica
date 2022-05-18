library(readxl)
BDa2censo<- read_excel("G:/Mi unidad/UCENTRAL/1ErSemestre/FUND_ ESTADÍSTICA_ANALÍTICA_DE_DATOS/ProyectoFundEstadistica/BD inversionistas depurada V6 anonimizada.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric"))

install.packages("foreign")
install.packages("apaTables")
install.packages("PerformanceAnalytics")
install.packages("psych")
install.packages("corrr")
install.packages("igraph")
install.packages("car")

library(car)
library(foreign)
library(apaTables)
library(PerformanceAnalytics)
library(psych)
library(corrr)
library(igraph)
library(corrgram)
library(gclus)
library(GGally)
library(gvlma)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("forestmodel","see","jtools","olsrr","parameters","stats","ggplot2", "palmerpenguins", "plot3D" , "plot3Drgl","apaTables","gvlma","broomExtra","performance")
ipak(packages)

# Paso Cero:  Para facilidad de interpretación de las gráficas se  pretende manejar una escala lo más similar posible por lo tanto se transformarán las variables de: ingreso mes , monto de inversión y patrimonio a millones dividiendo en un millón. 


BDa2censo$IngresosMes <- BDa2censo$IngresosMes / 1000000
BDa2censo$Patrimonio <- BDa2censo$Patrimonio / 1000000
BDa2censo$MontoInversion <- BDa2censo$MontoInversion / 1000000

# Paso 1 : Realizamos un diagrama de las variables cuantitativas con sus distribuciones y correlaciones, para identificar magnitud y sentido de la relación entre ellas

pairs.panels(BDa2censo[,15:20],pch = 20,stars = TRUE, main="Correlaciones entre variables")


#Luego de analizar las diferentes correlaciones entre variables cuantitativas identificamos que la mayor correlación se presenta entre el patrimonio y la edad (0.44) le  sigue la correlación que se da entre la cantidad de inversiones con el patrimonio (0.29 ) y la siguiente es la correlación que se da entre monto de inversión y patrimonio con un 0.26
#Dentro de los objetivos que se han planteado en este proyecto se pretende encontrar un modelo que pueda predecir el valor del monto de inversión dado las variables dependientes con las que se cuenta. Viendo que el monto de inversión presenta una correlación alrededor del 0.26 (aunque es una correlación moderada), plantearemos preliminarmente un modelo de regresión lineal simple que relacione precisamente estas dos variables patrimonio y monto de inversión.

# Paso 2: Modelo de Regresion Lineal Simple

modeloSimple <- lm(MontoInversion ~ Patrimonio, data = BDa2censo)
summary(modeloSimple)
modeloSimple$coefficients

  
  
# El modelos obtenido tiene la forma:

# MontoInversion = 0.72 + Patrimonio*0.0015
# Evidenciamos que aunque tanto el coeficiente como el intercepto resultan ser significativos dentro del análisis, el R2 ajustado que nos arroja este modelo es muy bajo (6.6%). Es decir que solo el 6.6% de porcentaje de variación en la variable de respuesta ( monto inversion) es explicado por su relación con la variable predictora (Patrimonio)



#Dado a este bajo desempeño del modelo de regresión lineal simple, buscaremos alternativas con un modelo de regresión múltiple incluyendo las otras variables o información disponible dentro de nuestra data. Intentaremos encontrar o predecir el monto de la inversión incluyendo la relación que puede existir con la edad del inversionista, con los ingresos mensuales, con la tasa a la cual se pacta el retorno de su inversión y otras variables que están bajo el análisis. Esperamos que con esto el desempeño del modelo sea mucho mejor.

## paso 3: Modelo de Regresión Lienal Multiple

modeloMultiple1 <- lm(MontoInversion ~ Edad +Tasa+IngresosMes+Patrimonio+CantidadInversiones, data = BDa2censo )
summary(modeloMultiple1)
modeloMultiple1$coefficients

# El modelo de regresion multiple obtenido tiene la forma:

# MontoInversion = 0.086 + Edad*0.004 + Tasa*5.31  + IngresosMes *0.0058 + Patrimonio*0.0015 - CantidadInversiones*0.0104

#Luego del análisis, podemos evidenciar que tanto el intercepto como los coeficientes de las variables predictoras: edad, tasa.ingresos mes, patrimonio, cantidad de inversiones son significativos  ( a un nivel de significancia del 0.5%)  la medida de desempeño R2ajustado sigue siendo muy bajo. Para este caso del modelo de regresión múltiple nos dio un 8.6% aproximadamente ( Solo el 8.6% de la variabilidad en los datos se puede explicar por el modelo). Es una leve mejoría con el modelo presentado anteriormente, pero va en contra del principio de parsimonia ya que hemos agregado y cuatro variables adicionales y la mejora fue prácticamente insignificante.

# Propendiendo  por el principio de parsimonia aplicaremos el algoritmo step para encontrar quizás una mejor combinación de variables que nos permitan un mejor índice de pérdida de información o AIC para un modelo de regresión múltiple


#Luego de correr el algoritmo evidenciamos que el modelo que nos propone con menor pérdida de información AIC 1182.07 es el modelo que previamente hemos definido (modeloMultiple1), que incluye todas las variables predictoras cuantitativas
step(object = modeloMultiple1, direction = "both", trace = 1)

#Paso4 Validacion de Supuestos 
#Es muy importante al momento de realizar un modelo de regresión lineal validar el cumplimiento de los supuestos ya que podemos ya que sin esta validación podemos caer en la estimación de variables no confiables


## 1. Multicolinealidad
#### Los predictores deben ser independientes,no debe de haber colinialidad entre ellos , para validar esto calculamos FACTOR DE INFLACION DE LA VARIANZA(VIF)
vif(modelo1)
# Este resultado para todas las variables predciotres no esta dando ligeramente mayor a 1 , por lo tanto hay un indicio de una posible colinialidad. Es relvante seguir evaluando los demas supuestos, par atebner la certeza q no se cumple y q no se podria usar un modelo de regresion como herramienta predicora. 


#para evaluar la influencia que tienen los predictores sobre la variable dependdiente

## 2. Relacion Lineal entre predictores y variable respuesta

crPlots(modeloMultiple1)

###Luego de graficar los residuos para cada variable predictora podemos identificar comportamientos aleatorios sobre el valor cero pero de alguna manera desiguales en los sentidos positivo y negativos. La gran mayoría de los residuos para cada variable predictora se encuentran por encima cero, No se cumple que se distribuyan en igual  proporción alrededor  del cero esto indicaría que este supuesto no se cumpliría


##3. Distribuciónnormaldelosresiduos

## La media de los residuos nos da aproxidamente cero (-1.30e-16) , mas sin embargo la distribucion de los residuos estudentizados presentean una comportamiento anomalo ( gran parte de los residuos superan el limite de 2). Este comporamtiento lo podemos evidenciar en la grafica Normal QQplot donde es calro que los residus no se ajustan sobre la diagonal de los Quantiles teoricos , por lo tanto este supuesto tampoco se satisface.
mean(modeloMultiple1$residuals)


#Silarelacióneslineal,losresiduossedistribuyendeformaaleatoriaentornoacero


install.packages('ggfortify')
library(ggfortify)
autoplot(modeloMultiple1,2)  ## Qqplot











#4.Homocedasticidad Para valdiar este supuesto acudiremos  a realizar el test de Breusch-Pagan, el cual nos arrojo un  p-value de 2.2e-16 , lo cual nso permite rechazr la hipotesis nula q existe varibilidad constante de los residuos. Es decir q no se presenta homcedasticidad, imcumpliendo asi el supuesto . Adcionalmente en la grafica podemos observar un incremnto en valor absoluto de los residuos estudentizados, confirmando la heterocedasticidad de los residuos.


library(lmtest)
bptest(modeloMultiple1) #Breusch-Pagan Test For Homoscedasticity

par(mfrow=c(1,1))
spreadLevelPlot(modeloMultiple1)
autoplot(modeloMultiple1,1)


## 5.1INDEPENDENCIA  Para validar esto supuesto , realizamos prueba Durbin Watson y evidecmoa q el p-value nos da ceor por lo tanto se rechaza la hipotesis nula , tambien podemos ver graficamente la que si existe autocorrelacion 

#independencia ()
durbinWatsonTest(modeloMultiple1)  # no cumple test de independencia

plot(modeloMultiple1$resid) # existe un patron , significa q no pasa la prueba , no son independintes
acf(modelo1$residuals)  # Si usamos una gracica de autocorrelacion envidenciamos q los residuos no se establizand de manera alterna al interio del intervalo de confianza , por lo tanto se peude inferir que existe autocorrelacion 









#Con animo ilustrativo y para confirmar se realiza una representación 3D de la regresión con solo dos variables predictoras ( edad y patrimonio)
z<-BDa2censo$MontoInversion
y<-BDa2censo$Patrimonio
x<-BDa2censo$Edad

scatter3D(x, y, z, theta = 15, phi = 20)
scatter3D(x, y, z, phi = 0, bty ="g")
scatter3D(x, y, z, pch = 18,  theta = 20, phi = 20,
          main = "Modelo Monto de Inversión", xlab = "Edad",
          ylab ="Patrimonio", zlab = "Inversion")

scatter3D(x, y, z, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed",xlab = "Edad",
          ylab ="Patrimonio", zlab = "Inversion")




objr<-lm(z ~ x+y)
objr

grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(objr, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

# Marcamos las líneas de iteracción para que busquen la recta de regresión
fitpoints <- predict(objr)
#ploteamos la gráfica en 3d con recta de regresión
scatter3D(x, y, z, pch = 18, cex = 2, 
          theta = 20, phi = 20, ticktype = "detailed",
          xlab = "Edad", ylab = "Patrimonio", zlab = "Inversion",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints), main = "")

plotrgl() ### ojo interactivo





###############paquetes

###




