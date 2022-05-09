#============================================================
# SÉPTIMA SESIÓN
# Muestreo  -  Ejemplo Muestreo Siniestralidad_Vial - Bogotá
#============================================================


# Lo primero que se debe hacer es explora la Data con la que se 
# trabajará.

# Importa la Base de Trabajo: MARCO DE ESTUDIO.
# Desde RStudio importar y nombrar como MARCO


MARCO=X2015_2019_siniestralidad_vial_CAS_EJEMPLO_MUESTREO_R

# Visualizar el dataset donde encontraremo las variables de interés
head(MARCO)




# Muestra la dimensión del dataset
dim(MARCO)



# Explorando las variables del Marco de Muestreo
colnames(MARCO)






# Ahora exploremos el Shapefile para saber que es lo que vamos 
# a graficar, o donde vamos a consignar nuestras estimaciones.

# SHAPEFILE:
# Un shapefile se compone principalmente de 3 archivos, de extensión
# .shp .shx y .dbf

#1.  .shp es el archivo que almacena las entidades geométricas de los 
# objetos.

# 2. .shx es el archivo que almacena el índice de las entidades 
# geométricas.

#3.  .dbf es la base de datos, en formato dBASE, donde se almacena la 
# información de los atributos de los objetos.


# Opcionalmente se pueden utilizar otros archivos con extensión
# .cpg .prj y .qpj para especificar el encoding o incluir información
# sobre la proyección cartográfica.


# CARGANDO EL SHAPEFILE
# Librería para leer Shapefile desde R:
library(rgdal)

# Indicandole a R donde encontrar nuestra carpeta de Shapefile, y el 
# nombre del archivo Shapefile.
sp_df <- readOGR(dsn = "C:/Users/cesar/Downloads/Shapefile_Localidades_SHP_Bogota", layer = "Loca")



head(sp_df)
fix(sp_df)
# as.data.frame(sp_df)


#===================================================================
# Convertir Shapefile en Excel:
# Para poder supervisar en detalle la base con la que se está graficando, 
#es necesario tener la posiblidad de maniobrar la data del shapefile.
# Esta línea convierte el Shapefile en Dataframe
library("raster")
LocalidadEXCEL=as(sp_df, "data.frame")
head(LocalidadEXCEL)
fix(LocalidadEXCEL)

# Y estas líneas exporta a EXCEL la base anterior
library(readxl)
library(writexl)

write_xlsx(LocalidadEXCEL,"C:/Users/Matematicas/Desktop/LocalidadEXCEL.xlsx")

#===================================================================




# Y ahora sí podemos disponer de todas las herramientas de R para hacer 
# gráficos elegantes.


# UNA PRUEBA GRAFICA DESDE EL SHAPEFILE: MAPAS TEMÁTICOS A PARTIR DE UNA 
# VARIABLE CUANTITATIVA HIPOTÉTICA, EN ESTE CASO, LA VARIABLE "LocArea"
# DEL ARCHIVO SHAPEFILE "sp_df"


library(leaflet)
library(dplyr)
library(sf)
library(tmap)
library(tigris)




pal <- colorNumeric( palette = "RdYlBu", domain=sp_df$LocArea)  #palette = "YlGnBu"   "RdBu"  "RdYlBu"  "Spectral"  "Paired"  "PuRd"  "RdYlGn"

leaflet(sp_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = ~pal(sp_df$LocArea),
              stroke = FALSE, fillOpacity = 1.0)  # Este parámetro proporciona opacidad en el mapa (Valores esntre 0 y 1)








# Asignando el Shapefile
LocaSH="C:/Users/cesar/Downloads/Shapefile_Localidades_SHP_Bogota/Loca.shp"

Bogota_Localidades_Shapefile <- st_read(LocaSH)



# Buscando en la base EXCEL de trabajo (MARCO), y usando una variable o columna de 
# de resumen (en este caso: "LOCALIDAD")

Siniestros_Bogota <- MARCO %>%
  group_by(LOCALIDAD) %>%
  summarise(n = n(), Promedio_M = mean(TOTAL_MUERTOS), Mediana_M = median(TOTAL_MUERTOS), Total_M = sum(TOTAL_MUERTOS))

# Para visualizar la base MARCO resumida
 Resumen=as.data.frame(Siniestros_Bogota)
 Resumen



# NECESARIA PARA VINCULAR LAS DOS BASES:
 NroLOCALIDAD=c("15", "12", "07", "02", "19", "10", "09", "08", "17", "14", "16", "18", "04", "03", "11", "20", "13", "06", "01", "05")
 NroLOCALIDAD= as.data.frame(NroLOCALIDAD)


Resumen2=cbind(Resumen, NroLOCALIDAD )
Resumen2



library(tidyr)
Etiquetas=unite(Resumen2, Etiqueta,c(1,5), sep = ": ", remove = TRUE)
Etiquetas=Etiquetas[,1]
Etiquetas



Resumen3=cbind(Resumen2, Etiquetas )
Resumen3



# Vinculando el Shapefile con el Dataset
# sp_df: El Shapefile     y Siniestros_Bogota: El dataset
LOCALIDADES_JOIN <- geo_join(Bogota_Localidades_Shapefile, Resumen3,"LocCodigo", "NroLOCALIDAD")



# Creando una paleta de colores basada en el rango de números en la 
# columna Total_M
pal <- colorNumeric("RdYlBu", domain=LOCALIDADES_JOIN$Total_M)


# Deshacerse de filas con valores NA
# Usando el método Base R para filtrar subconjunto () porque estamos 
#tratando con un SpatialPolygonsDataFrame y no con un marco de datos 
# normal, por lo tanto, filter () no funcionaría

LOCALIDADES_JOIN <-subset(LOCALIDADES_JOIN, !is.na(Total_M))

# Configuración del texto emergente
popup_sb <- paste0("Total: ", as.character(LOCALIDADES_JOIN$Total_M))




# Mapeándolo con los nuevos mosaicos CartoDB.Positron
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  #setView(-98.483330, 38.712046, zoom = 4) %>% 
    addPolygons(data = LOCALIDADES_JOIN , 
              fillColor = ~pal(LOCALIDADES_JOIN$Total_M),
              opacity = 1,
              color = "black",
              dashArray = "3",fillOpacity = 0.9,
              highlight = highlightOptions(
                weight = 1,
                color = "#666",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = TRUE),
              label = LOCALIDADES_JOIN$Etiqueta,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  addLegend(pal = pal, values =LOCALIDADES_JOIN$Total_M, opacity = 0.7, title = NULL,
            position = "bottomright")

              
              

#  Muestreo:

# Seleccionando una muestra aleatoria (sin reemplazamiento), de 
# elementos de los 100 posibles sujetos de la población que fue generada
# anteriormente:

# set.seed(1643)  #  Semilla
# n=500.  *


N=dim(MARCO)
n=50

MAS<- sample(1:N,n)
MAS





# Seleccionando los puntos de la MAS en la misma gráfica
DATASET_M=MARCO[MAS,]
fix(DATASET_M)

# Y desde aquí se hace la estimación de parámetros.




  