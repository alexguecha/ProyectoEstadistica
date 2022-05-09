#=========================================
# SEXTA SESIÓN
# Muestreo
#=========================================
# IDEA GRÁFICA DEL PROCESO DE SELECCIÓN
# Generando el Universo de Estudio - MARCO MUESTRAL:
popnx <- runif(100)
popny <- runif(100)

# Graficando la distribución especial de la población
plot(popnx,popny)
plot(popnx,popny,col=1, bg="blue", pch=21, cex=1.3, xlab="Longitid", ylab="Latitud", main="Simulación de Selección de una MAS")



# Seleccionando una muestra aleatoria simple, sin reemplazamiento, de 10 
# elementos de los 100 posibles de la población:
# set.seed(1643)  #  Semilla
MAS<- sample(1:100,22)
MAS

# Seleccionando los puntos de la MAS en la misma gráfica
points(popnx[MAS],popny[MAS])

# Resaltando la MAS a través de colores 
points(popnx[MAS],popny[MAS], pch=21, bg="green",cex=2)




# Representatividad de la MAS en la población elegida 
plot(popnx,popny,col=1, bg="blue", pch=21, cex=1.3, xlab="Longitid", ylab="Latitud", main="Simulación de Selección de una MAS")
points(popnx[MAS],popny[MAS], pch=21,bg="#FF0066",cex=12)
points(popnx[MAS],popny[MAS], pch=21, bg="green", cex=2)





# CASO DE ESTUDIO DE UNA MUESTRA ALEATORIA SIMPLE
# Kruuk y col. (1990) utilizaron un diseño de Muestreo Aleatorio 
# Estratificado para estimar el número de madrigueras o huecos de nutrias 
# (Lutra lutra) a lo largo de una costa de 1400 km de las Islas Shetland. 
# La costa se dividió en 237 secciones habitables de 5 km, con cada sección
# clasificada como una de cuatro tipos: 
# - acantilados de más de 10m (89 secciones)
# - agrícolas (61 secciones) 
# - turba (40 secciones)              # TURBA: Residuos de plantas que se acumulan en una zona pantanosa, de consistencia algo esponjosa, cuenta con una importante presencia de carbono y exhibe un tono oscuro. 
# - No turba (47 secciones) 

# La muestra presentada aquí se recopiló mediante Muestreo Aleatorio 
# Estratificado, pero para este ejemplo se tratará como si se hubiera 
# recopilado mediante un Muestreo Aleatorio Simple. 
# Los datos están disponibles en el paquete SDaA. 


# Cargando la librería específica:
library(SDaA)
attach(otters)

# El marco de datos se llama "otters" (nutrias):
head(otters)
  
# Muestra la dimensión del dataset
dim(otters)



# Cada observación incluye el número de sección, el tipo de hábitat 
# y el número de holts (guaridas ~ hogar). Aunque no es necesario para 
# el análisis, sería útil que los nombres de los estratos sean más 
# descriptivos (etiquetas en vez de códigos). 
# Esto se puede hacer cambiando cada categoría de hábitat en una etiqueta
# usando la función de factor y asignando etiquetas a los estratos, así:


otters$habitat <- factor(otters$habitat, labels = c("acantilados", "agrícola", 
                                                    "turba", "no turba"))
head(otters)





# Especificación del Diseño Muestral:
# Antes de especificar el diseño, es necesario incluir dentro 
# del marco de datos el tamaño de la población para la corrección 
# de poblaciones finita. Esto se puede hacer creando una variable llamada N.

otters$N <- 237
head(otters)



# Ahora se puede especificar el diseño muestral. 
# Primero cargue el paquete "survey".

library(survey)


# La función "svydesign" en el paquete "survey" se usa para crear un 
# Diseño Muestral que incluye información sobre el tipo de muestreo y 
# los datos. Un diseño MAS se puede especificar de la siguiente manera.


mydesign <- svydesign(id = ~1, data = otters, fpc = ~N)

mydesign


# Estimaciones
svymean(otters$holts,mydesign)
summary(mydesign)


# El argumento ids = ~ 1 tiene que ver con si los elementos se agrupan 
# dentro de las unidades de muestreo o cómo, por ejemplo, en el muestreo 
# por conglomerados. Pero para un MAS, simplemente 
# especifique ids = ~ 1 para indicar un elemento por unidad de muestreo. 
# El argumento data = otters indica el tamaño del marco de muestreo. 
# El argumento fpc = ~ N indica la variable del tamaño de la población de 
# la que se tomó la muestra.










#========================================================================
#  UNA MANERA DE REALIZAR UN MAS, EN CARTOGRAFÍA:
#========================================================================
# “Generando un escenario de selección de muestras aleatorias simples”
#========================================================================


# UN CASO DE ESTUDIO:
# DESCRIPCIÓN DE LA BASE:
# El "aí, cúcula" o perezoso bayo (Bradypus variegatus) es una especie de 
# perezoso tridáctilo de la familia Bradipodidae, perteneciente al orden 
# Pilosa, propia de Centroamérica y Sudamérica.
# Es la especie más distribuida y común del grupo, hallándose en muchos y 
# diferentes ambientes, incluyendo selvas densas, bosques secos y 
# áreas naturales. Es solitario, con hábitos diurnos y nocturnos,
# alimentándose de hojas de muchas especies de árboles.
# La hembra emite un lamento fuerte, estridente, durante la temporada de 
# apareamiento para atraer machos. Es un alarido que suena 
# como un "ay ay" y por tal motivo ha recibido este 
# nombre de parte de varias etnias que viven cerca de su hábitat. 



# carga la libreria
library(dismo)

# Seleccionando el dataset de la librería y asignando
file <- paste0(system.file(package="dismo"), "/ex/bradypus.csv")
# este es el archivo que usaremos: file



# Ahora lectura e inspección del dataset:
bradypus <- read.table(file,  header=TRUE,  sep=",")
# Otra forma:  bradypus <- read.csv(file)


# Visualizar el dataset donde encontraremo la especie y 
# las coordenadas de geolocalización

head(bradypus)


# Extraer lsa coordenadas
bradypus <- bradypus[,2:3]
head(bradypus)




# Desde aquí puede carga toda la base completa del estudio:
acaule <- gbif("solanum", "acaule*", geo=FALSE)

# cargar los datos guardados de S. acaule
data(acaule)

# Muestra la dimensión del dataset
dim(acaule)




# Seleccione los registros que tienen datos de longitud y latitud
colnames(acaule)

acgeo <- subset(acaule, !is.na(lon) & !is.na(lat))
dim(acgeo)

# muestra algunos valores
acgeo[1:4, c(1:5,7:10)]




# A continuación se muestra una manera simple de hacer un mapa de las 
# localidades de ocurrencia de Solanum acaule. Es importante hacer tales 
# mapas para asegurar que los puntos estén, al menos aproximadamente, en 
# la ubicación correcta.

library(maptools)
## Comprobando la disponibilidad de rgeos: VERDADERO
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-80,70), ylim=c(-60,10), axes=TRUE, col="light yellow")


# restaurar el cuadro alrededor del mapa
box()

# agrega los puntos
points(acgeo$lon, acgeo$lat, col='blue', pch=20, cex=1.4)
# trazar puntos nuevamente para agregar un borde, para una mejor visibilidad
points(acgeo$lon, acgeo$lat, col='red', cex=1.5)




# CALIDAD DE LA DATA:




#---------------------------------------------------------------
# Limpieza de datos
#---------------------------------------------------------------

# La "limpieza" de datos es particularmente importante para los datos 
# procedentes de bodegas de datos de distribución de especies como 
# la anterior. En muchas ocasiones la recopilación de datos no implica 
# la calidad de los mismos, por lo tanto es nuestro deber comprenderlos 
# y limpiarlos adecuadamente para su aplicación. 

# Aquí continuamos con nuestro ejemplo:  
# ¿Ves algún error en el mapa anterior?


# Solanum acaule es una especie de "Perezoso Bayo" que se encuentra en las 
# partes más altas de las montañas de los Andes del sur de Perú, Bolivia y
# el norte de Argentina. 

# ¿Ves algún error en el mapa?

# Hay algunos registros que se mapearon en el océano al sur de Pakistán. 

# ¿Alguna idea de por qué puede haber sucedido? 
# Los errores más comunes son: faltan signos menos en algunas coordenadas. 
# Las coordenadas son alrededor de (65.4, 23.4) pero deberían estar en el
# norte de Argentina alrededor de(-65.4, -23.4). 

# Tamnbién hay dos registros (filas 303 y 885) que se asignan al mismo 
# lugar en la Antártida (-76.3, -76.3). La descripción de la localidad 
# dice que debería estar en Huarochiri, cerca de Lima, Perú. Por lo tanto,
# la longitud es probablemente correcta y la latitud transcrita erróneamente.
# Curiosamente, el registro ocurre dos veces. 


# Por otro lado, los puntos en Brasil (todos los acaule [98,]) debería 
# estar en el sur de Bolivia, por lo que esto probablemente se deba a un 
# error tipográfico en la longitud. 
# Del mismo modo, también hay tres registros que tienen 
# latitudes plausibles, pero longitudes que están claramente equivocadas, 
# como lo están en el Océano Atlántico, al sur de África occidental. Parece
# que tienen una longitud que es cero. En muchas bases de datos, encontrará
# valores que son "cero" donde se pretendía "sin datos". La función gbif 
# (cuando se usan los argumentos predeterminados) establece coordenadas 
# que son (0, 0) a NA, pero no si una de las coordenadas es cero. Veamos 
# si los encontramos buscando registros con longitudes de cero.

# Echemos un vistazo a estos registros:
lonzero = subset(acgeo, lon==0)

# mostrar los registros de las primeras 13 columnas
lonzero[, 1:13]



# ---------------------------------------------------
# Registros duplicados
# ---------------------------------------------------
# Curiosamente, otro problema de calidad de datos se señala a continuación: 
# ¿Qué registros son duplicados?
# (solo para las primeras 10 columnas)
dups <- duplicated(lonzero[, 1:10])
dups

# eliminar duplicados
lonzero  <-  lonzero[dups, ]
lonzero[,1:13]

dups <- duplicated(lonzero[, 1:10])
dups




# Para poder supervisar en detalle la base con la que se está graficando, 
#es necesario tener la posiblidad de maniobrar la data del shapefile.
# Esta línea convierte el Shapefile en Dataframe
library("raster")
acgeoEXCEL=as(acgeo, "data.frame")
head(acgeoEXCEL)
fix(acgeoEXCEL)



# Y estas líenas exporta a EXCEL la base anerior
library(readxl)
library(writexl)

write_xlsx(acgeoEXCEL,"C:/Users/Matematicas/Desktop/acgeo_excel.xlsx")





# Actividad:
#========================================================================
#  MUESTREO ALEATORIO SIMPLE  - MAS
#========================================================================
# “Generando un escenario de selección de muestras aleatorias simples”
#========================================================================

# Proporcione una muestra aleatoria simple de 100 observaciones 
# e identifíquelas en la cartografía.
