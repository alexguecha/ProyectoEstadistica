#==========================

head(Incendios)

## En el proceso de limpieza se quitaros 2 registros , un dato que corresponde a peru y un dato vacio


dim(Incendios)

N = nrow(Incendios)

## N corresponde a la población total del estudio que son el total de registros

z = qnorm(0.035, mean= 0, sd = 1, lower.tail = TRUE)

## 
d = 0.03

n = z*z*0.5*0.5/(d*d+(z*z*0.5*0.5)/N)


n = ceiling(n)

n

set.seed(3564)
muestra_inc <- Incendios [ sample (N, size = n ),]
muestra_inc

MediaH=mean(muestra_inc$HERIDOS)
MediaH

MediaF=mean(muestra_inc$FALLECIDOS)
MediaF

muestra_inc$TotalN <- N
head(muestra_inc)
muestra_inc


library(survey)



mydesign <- svydesign(id = ~1, data = muestra_inc, fpc = ~TotalN)

summary(mydesign)


# Estimaciones
svymean(muestra_inc$HERIDOS,mydesign)
summary(mydesign)
#=========================


VarH=var(muestra_inc$HERIDOS)
VarH


MediaH=mean(muestra_inc$HERIDOS)
MediaH


VarEstim=(1-757/4435)*VarH/757
VarEstim


EE=sqrt(VarEstim)
EE

CV=(EE/MediaH)*100
CV

valort=qt(c(0.025),df=(757-1),lower.tail = FALSE)# probabilidad de cola 0.025 equivle a nivel de confianza del 95%, ya q la dist t es simetrica

Lsup=MediaH+(valort*EE) 
Linf=MediaH-(valort*EE)


resumenMediaH1 <- data.frame(n ,MediaH,VarEstim,EE,Linf,Lsup,CV)
resumenMediaH1

#Con un nivel de confianza del 95% el promedio de los heridos de la población va a ser de 0.39


EstimTot=N*MediaF
EstimTot


VarEstimTOT=(N^2)*VarEstim
VarEstimTOT

#El Error Estándar (EE) de la estimación, es:
EETot=sqrt(VarEstimTOT)

EETot

## Calculo de intervalo de confianza al 95%

valortF=qt(c(0.025),df=(757-1),lower.tail = FALSE)# probabilidad de cola 0.025 equivle a nivel de confianza del 95%, ya q la dist t es simetrica

LsupTotF=EstimTot+(valortF*EETot) 
LinfTotF=EstimTot-(valortF*EETot)

# El coeficiente de variación de la estimación del Total, es:
CVTot=(EETot/EstimTot)*100
CVTot

resumenTotF <- data.frame(n ,EstimTot,VarEstimTOT,EETot,LinfTotF,LsupTotF,CVTot)
resumenTotF


## Cartografía

library(rgdal)

sp_df <- readOGR(dsn = "C:/Users/Sergio/Downloads/MGN2021_DPTO_POLITICO", layer = "MGN_DPTO_POLITICO")

head(sp_df)
#fix(sp_df)
as.data.frame(sp_df)

DPTO_SH="C:/Users/Sergio/Downloads/MGN2021_DPTO_POLITICO/MGN_DPTO_POLITICO.shp"

DPTO_SH2 <- st_read(DPTO_SH)

library("raster")
LocalidadEXCEL=as(sp_df, "data.frame")
head(LocalidadEXCEL)
fix(LocalidadEXCEL)

library(readxl)
library(writexl)

write_xlsx(LocalidadEXCEL,"C:/Users/Sergio/Downloads/LocalidadEXCEL.xlsx")



### 
library(leaflet)
library(dplyr)
library(sf)
library(tmap)
library(tigris)

library(dplyr)
HeridosDepto <- muestra_inc %>%
group_by(DEPARTAMENTO) %>%
  summarise(promedioH = mean(HERIDOS),COD_DANE)




# Para visualizar la base resumida
ResumenH=as.data.frame(HeridosDepto)
ResumenH


library(tidyr)
Etiquetas=unite(ResumenH, Etiqueta,c(1,2), sep = ": ", remove = TRUE)
Etiquetas=Etiquetas[,1]
Etiquetas



Resumen3=cbind(ResumenH, Etiquetas )
Resumen3


DPTO_SH2

DPTO_JOIN <- geo_join(DPTO_SH2, Resumen3,"DPTO_CCDGO", "COD_DANE")
#DPTO_JOIN = as.data.frame(DPTO_JOIN)
DPTO_JOIN


pal <- colorNumeric( palette = "RdYlBu", domain=DPTO_JOIN$promedioH)  #palette = "YlGnBu"   "RdBu"  "RdYlBu"  "Spectral"  "Paired"  "PuRd"  "RdYlGn"

leaflet(sp_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = ~pal(DPTO_JOIN$promedioH),
              stroke = FALSE, fillOpacity = 1.0)  # Este parámetro proporciona opacidad en el mapa (Valores esntre 0 y 1)

popup_sb <- paste0("Promedio de Heridos: ", as.character(DPTO_JOIN$promedioH))


leaflet(sp_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  #setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = DPTO_JOIN , 
              fillColor = ~pal(DPTO_JOIN$promedioH),
              opacity = 1,
              color = "black",
              dashArray = "3",fillOpacity = 0.9,
              highlight = highlightOptions(
                weight = 1,
                color = "#666",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = TRUE),
              label = DPTO_JOIN$Etiquetas,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  addLegend(pal = pal, values =DPTO_JOIN$promedioH, opacity = 0.7, title = NULL,
            position = "bottomright")



#=================




