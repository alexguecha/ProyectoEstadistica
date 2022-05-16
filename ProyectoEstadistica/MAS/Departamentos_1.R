MARCO_INV=BdInversionistas

# Visualizar el dataset donde encontraremo las variables de interés
head(MARCO_INV$MontoInversion)




# Muestra la dimensión del dataset
dim(MARCO_INV)



# Explorando las variables del Marco de Muestreo
colnames(MARCO_INV)



N=dim(MARCO_INV)
n=500

MAS<- sample(1:N,n)
MAS
fix(MAS)
DATASET_M=MARCO_INV[MAS,]
fix(DATASET_M)

library(rgdal)

# Indicandole a R donde encontrar nuestra carpeta de Shapefile, y el 
# nombre del archivo Shapefile.
sp_df <- readOGR(dsn = "~/GitHub/ProyectoEstadistica/ProyectoEstadistica/MAS/Departamentos202204_shp", layer = "departamento")

head(sp_df)
fix(sp_df)

library("raster")
DepartamentoEXCEL=as(sp_df, "data.frame")
head(DepartamentoEXCEL)
fix(DepartamentoEXCEL)

library(readxl)
library(writexl)

write_xlsx(DepartamentoEXCEL,"~/GitHub/ProyectoEstadistica/ProyectoEstadistica/MAS/DepartamentoEXCEL.xlsx")

library(leaflet)
library(dplyr)
library(sf)
library(tmap)
library(tigris)

pal <- colorNumeric( palette = "RdYlBu", domain=sp_df$AREA)  #palette = "YlGnBu"   "RdBu"  "RdYlBu"  "Spectral"  "Paired"  "PuRd"  "RdYlGn"

leaflet(sp_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = ~pal(sp_df$AREA),
              stroke = FALSE, fillOpacity = 1.0)  # Este parámetro proporciona opacidad en el mapa (Valores esntre 0 y 1)






DeptoSH="~/GitHub/ProyectoEstadistica/ProyectoEstadistica/MAS/depto/depto.shp"

Departamentos_Shapefile <- st_read(DeptoSH)

Departamentos_inversion <- MARCO_INV %>%
  group_by(Departamento) %>%
  summarise(n = n(), Promedio_M = mean(MontoInversion), Mediana_M = median(MontoInversion), Total_M = sum(MontoInversion))

# Para visualizar la base MARCO resumida
Resumen=as.data.frame(Departamentos_inversion)
Resumen

write_xlsx(Resumen,"~/GitHub/ProyectoEstadistica/ProyectoEstadistica/MAS/ResumenEXCEL.xlsx")


NroDepto=DepartamentoEXCEL$DPTO
NroDepto= as.data.frame(NroDepto)


Resumen2=cbind(Resumen, NroDepto )
Resumen2 =ResumenEXCEL

library(tidyr)
Etiquetas=unite(Resumen2, Etiqueta,c(1,5), sep = ": ", remove = TRUE)
Etiquetas=Etiquetas[,1]
Etiquetas



Resumen3=cbind(Resumen2, Etiquetas )
Resumen3



# Vinculando el Shapefile con el Dataset
# sp_df: El Shapefile     y Siniestros_Bogota: El dataset
DEPARTAMENTOS_JOIN <- geo_join(Departamentos_Shapefile, Resumen3,"DPTO", "DPTO")



# Creando una paleta de colores basada en el rango de números en la 
# columna Total_M
pal <- colorNumeric("RdYlBu", domain=DEPARTAMENTOS_JOIN$Total_M)


# Deshacerse de filas con valores NA
# Usando el método Base R para filtrar subconjunto () porque estamos 
#tratando con un SpatialPolygonsDataFrame y no con un marco de datos 
# normal, por lo tanto, filter () no funcionaría

DEPARTAMENTOS_JOIN <-subset(DEPARTAMENTOS_JOIN, !is.na(Total_M))

# Configuración del texto emergente
popup_sb <- paste0("Total: ", as.character(DEPARTAMENTOS_JOIN$Total_M))

fix(DEPARTAMENTOS_JOIN)
head(sp_df)
fix(DEPARTAMENTOS_JOIN)

# Mapeándolo con los nuevos mosaicos CartoDB.Positron
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  #setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = DEPARTAMENTOS_JOIN , 
              fillColor = ~pal(DEPARTAMENTOS_JOIN$Total_M),
              opacity = 1,
              color = "black",
              dashArray = "3",fillOpacity = 0.9,
              highlight = highlightOptions(
                weight = 1,
                color = "#666",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = TRUE),
              label = DEPARTAMENTOS_JOIN$Etiqueta,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  addLegend(pal = pal, values =DEPARTAMENTOS_JOIN$Total_M, opacity = 0.7, title = NULL,
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




