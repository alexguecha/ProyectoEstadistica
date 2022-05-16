library(readxl)
BDa2censo<- read_excel("G:/Mi unidad/UCENTRAL/1ErSemestre/FUND_ ESTADÍSTICA_ANALÍTICA_DE_DATOS/ProyectoFundEstadistica/BD inversionistas depurada V6 anonimizada.xlsx", 
                                                        col_types = c("text", "text", "text", 
                                                                      "text", "text", "text", "text", "text", 
                                                                      "text", "text", "text", "text", "text", 
                                                                      "text", "numeric", "numeric", "numeric", 
                                                                      "numeric", "numeric", "numeric"))



N=nrow(BDa2censo)
n=500
set.seed(123)
masBD <- BDa2censo [ sample (N, size = n ),]


MediaEdad=round(mean(masBD$Edad),2)
MediaEdad

VarEdad=var(masBD$Edad)
VarEdad


VarEstim=round(((1-n/N)*VarEdad/n),2)
VarEstim


EE=round(sqrt(VarEstim),2)
EE

CV=round((EE/MediaEdad)*100,2)
CV

valort=qt(c(0.025),df=(n-1),lower.tail = FALSE)# probabilidad de cola 0.025 equivle a nivel de confianza del 95%, ya q la dist t es simetrica

Lsup=round(MediaEdad+(valort*EE),2) 
Linf=round(MediaEdad-(valort*EE),2)


round(c(VarEdad,Linf),digit=2)
resumenMediaEdad <- data.frame(n ,MediaEdad,VarEstim,EE,Linf,Lsup,CV)
resumenMediaEdad
mean(BDa2censo$Edad)
 ######

MediaInversion=mean(masBD$MontoInversion)
VarMuestraInv =var(masBD$MontoInversion)
VarEstimadaMediaInv=((N-n)/N)*(VarMuestraInv/n)



VarEstimInv=round(((1-n/N)*VarEstimadaMediaInv/n),2)
VarEstimInv


EEmediaInv=round(sqrt(VarEstimInv),2)
EEmediaInv

CVmediaInv=round((EEmediaInv/MediaInversion)*100,2)
CVmediaInv

valort=qt(c(0.025),df=(n-1),lower.tail = FALSE)# probabilidad de cola 0.025 equivle a nivel de confianza del 95%, ya q la dist t es simetrica

LsupMediaInv=round(MediaInversion+(valort*EEmediaInv),2) 
LinfMediaInv=round(MediaInversion-(valort*EEmediaInv),2)



resumenMediaInversion <- data.frame(n ,MediaInversion,VarEstimInv,EEmediaInv,LinfMediaInv,LsupMediaInv,CVmediaInv)
resumenMediaInversion
mean(BDa2censo$MontoInversion)


###

MediaInversion=mean(masBD$MontoInversion)
TotalInversion=N*MediaInversion
TotalInversion

VarMuestra =var(masBD$MontoInversion)
VarEstimadaMedia=((N-n)/N)*(VarMuestra/n)

VarEstimadaTotal=N*N*VarEstimadaMedia
EEtotal=sqrt(VarEstimadaTotal)


CVtotal=round((EEtotal/TotalInversion)*100,2)
CVtotal

LsupTotal=round(TotalInversion+(valort*EEtotal),2) 
LinfTotal=round(TotalInversion-(valort*EEtotal),2)

sum(BDa2censo$MontoInversion)

resumenTotalInversion <- data.frame(n ,TotalInversion,VarEstimadaTotal,EEtotal,LinfTotal,LsupTotal,CVtotal)
resumenTotalInversion
####

library(rgdal)

sp_df <- readOGR(dsn = "~/GitHub/ProyectoEstadistica/ProyectoEstadistica/Sergio/MGN2021_DPTO_POLITICO", layer = "MGN_DPTO_POLITICO")

head(sp_df)
#fix(sp_df)
as.data.frame(sp_df)

DPTO_SH="~/GitHub/ProyectoEstadistica/ProyectoEstadistica/Sergio/MGN2021_DPTO_POLITICO/MGN_DPTO_POLITICO.shp"

DPTO_SH2 <- st_read(DPTO_SH)

library("raster")
library(leaflet)
library(dplyr)
library(sf)
library(tmap)
library(tigris)
library(dplyr)
InverDepto <- masBD %>%
  group_by(Departamento) %>%
  summarise(promedioEdad = mean(Edad),promedioInver = mean(MontoInversion),Cod_Dpto)




# Para visualizar la base resumida
ResumenInv=as.data.frame(InverDepto)
ResumenInv


library(tidyr)
Etiquetas=unite(ResumenInv, Etiqueta,c(1,2,3), sep = ": ", remove = TRUE)
Etiquetas=Etiquetas[,1]
Etiquetas



Resumen3=cbind(ResumenInv, Etiquetas )
Resumen3


DPTO_SH2

DPTO_JOIN <- geo_join(DPTO_SH2, Resumen3,"DPTO_CCDGO", "Cod_Dpto")
#DPTO_JOIN = as.data.frame(DPTO_JOIN)
DPTO_JOIN


pal <- colorNumeric( palette = "RdYlBu", domain=DPTO_JOIN$promedioInver)  #palette = "YlGnBu"   "RdBu"  "RdYlBu"  "Spectral"  "Paired"  "PuRd"  "RdYlGn"

leaflet(sp_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = ~pal(DPTO_JOIN$promedioInver),
              stroke = FALSE, fillOpacity = 1.0)  # Este parámetro proporciona opacidad en el mapa (Valores esntre 0 y 1)

popup_sb <- paste0("Promedio de Monto de Inversión: ", as.character(DPTO_JOIN$promedioInver))


leaflet(sp_df) %>%
  addProviderTiles("CartoDB.Positron") %>%
  #setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = DPTO_JOIN , 
              fillColor = ~pal(DPTO_JOIN$promedioInver),
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
  addLegend(pal = pal, values =DPTO_JOIN$promedioInver, opacity = 0.7, title = NULL,
            position = "bottomright")








#### Diferentes Subset
Recurrentes <- subset(BDa2censo, CodEsInversionistaRecurrente== "Inversionista Recurrente")
Mayores35 <- subset(BDa2censo, Edad > 35)
Mayores35Recurrentes <- subset(Mayores35, CodEsInversionistaRecurrente== "Inversionista Recurrente")

InvMayor_1MM <- subset(BDa2censo, MontoInversion>=1000000)

InvMayor_2MM <- subset(BDa2censo, MontoInversion>=2000000)

### Boxplots

boxplot(BDa2censo$MontoInversion~BDa2censo$Edad,col="gold",xlab="Edad",ylab="Inversión",
        main="Inversión por Edad")


boxplot(BDa2censo$MontoInversion~BDa2censo$GrupoEdad,col="gold",xlab="Grupo Edad",ylab="Inversión",
        main="Inversión por GrupoEdad")




boxplot(BDa2censo$MontoInversion~BDa2censo$GrupoIngresos,col="gold",xlab="Edad",ylab="Inversión",
        main="Inversión por GrupoEdad")
##

boxplot(Recurrentes$MontoInversion~Recurrentes$GrupoEdad,col="gold",xlab="Grupo Edad",ylab="Inversión",
        main="Recurrentes Inversión por GrupoEdad")


boxplot(Recurrentes$MontoInversion~Recurrentes$SectorCampaña,col="gold",xlab="Sector Campaña",ylab="Inversión",
        main="Recurrentes Inversión por Sector Campaña")


boxplot(Recurrentes$MontoInversion~Recurrentes$Tasa,col="gold",xlab="Tasa",ylab="Inversión",
        main="Recurrentes Inversión por Tasa")



boxplot(Recurrentes$MontoInversion~Recurrentes$Propósito,col="gold",xlab="Propósito",ylab="Inversión",
        main="Recurrentes Inversión por Propósito", horizontal = TRUE)

### 

boxplot(Mayores35$MontoInversion~Mayores35$Edad,col="gold",xlab="Edad",ylab="Inversión",
        main="Inversión por Edad")

boxplot(Mayores35$MontoInversion~Mayores35$GrupoEdad,col="gold",xlab="Edad",ylab="Inversión",
        main="Inversión por Edad")

boxplot(data=Mayores35Recurrentes,MontoInversion~Patrimonio,col="gold",xlab="Tasa",ylab="Inversión",
        main="Inversión por Tasa")

plot(Mayores35$Edad,Mayores35$MontoInversion)

plot(Mayores35$IngresosMes,Mayores35$MontoInversion)


library(corrgram)
library(gclus)
corrgram(Mayores35Recurrentes, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Matriz de Correlaciones Mayor de 35 Años Recurrentes")

modeloMayores35 <- lm(MontoInversion ~ Tasa+CantidadInversiones + Edad + IngresosMes + Patrimonio, data = Mayores35 )

summary(modeloMayores35)

modeloMayores35 <- lm(MontoInversion ~ Patrimonio, data = Mayores35 )

summary(modeloMayores35)


modeloPatromonioMayores35Recurrentes <- lm(Patrimonio ~ CantidadInversiones + Edad + IngresosMes +MontoInversion, data = Mayores35Recurrentes )

summary(modeloPatromonioMayores35Recurrentes)


library(corrgram)
library(gclus)
corrgram(InvMayor_1MM, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Matriz de Correlaciones Inv Mayor 1MM")

boxplot(data=InvMayor_1MM,MontoInversion~Edad,col="gold",xlab="Edad",ylab="Inversión",
        main="Inversión por Patrominio")


library(corrgram)
library(gclus)
corrgram(InvMayor_2MM, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Matriz de Correlaciones Inv Mayor 2MM")

boxplot(data=InvMayor_2MM,MontoInversion~GrupoIngresos,col="gold",xlab="Grupo Ingresos",ylab="Inversión",
        main="Inversión por Grupo Edad")


modeloInv2MM <- lm(MontoInversion ~  IngresosMes + Patrimonio, data = InvMayor_2MM )

summary(modeloInv2MM)






### Modelo de Regression Multiple

modelotest <- lm(MontoInversion ~ Tasa+CantidadInversiones + Edad + IngresosMes + Patrimonio, data = BDa2censo )

summary(modelotest)

Predicciones1<-modelotest$fitted.values

library(ggplot2)
ggplot(data = BDa2censo, aes(x = predict(modelotest), 
                             y = abs(rstudent(modelotest))))+
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed")+
  geom_point(aes(color = ifelse(abs(rstudent(modelotest)) > 2, "red", "black")))+
  scale_color_identity()+
  labs(title = "Distribucion de los residuos estudentizados", 
       x = "Prediccion modelo", 
       y = "Residuos estudentizados")+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


###
modelotest1 <- lm(MontoInversion ~ IngresosMes , data = BDa2censo )

summary(modelotest1)

# ----
Recurrentes <- subset(BDa2censo, CodEsInversionistaRecurrente== "Inversionista Recurrente")

modelotest2 <- lm(MontoInversion ~ Edad +Patrimonio+IngresosMes+Tasa+CantidadInversiones , data = Recurrentes )
summary(modelotest2) ## mejor 11%

NoRecurrentes <- subset(BDa2censo, CodEsInversionistaRecurrente== "Inversionista No Recurrente")

modelotest2 <- lm(MontoInversion ~ Edad +Patrimonio+IngresosMes+Tasa+CantidadInversiones , data = NoRecurrentes )
summary(modelotest2)
 ### ---

Mayores37 <- subset(BDa2censo, Edad > 37)
modelotest2 <- lm(MontoInversion ~ Edad + IngresosMes, data = Mayores37 )
summary(modelotest2)


#### ---

mean(BDa2censo$IngresosMes)
Ingresos30MM <- subset(BDa2censo, IngresosMes > 30000000)
modelotest2 <- lm(MontoInversion ~ Edad + IngresosMes+Patrimonio, data = Ingresos30MM )
summary(modelotest2)


modelotest3 <- lm(IngresosMes ~ Edad +Patrimonio+Tasa+MontoInversion, data = Ingresos30MM )
summary(modelotest3)


##### ----

masde12inv <- subset(BDa2censo, CantidadInversiones > 15)
modelotest4 <- lm(MontoInversion ~ Patrimonio ,data = masde12inv)
summary(modelotest4)
  ####

step(object = modelotest2, direction = "both", trace = 1)
  ####

library(corrgram)
library(gclus)
corrgram(BDa2censo, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Matriz de Correlaciones")


####
library(GGally)
ggpairs(BDa2censo[,15:20], lower = list(continuous = "smooth"),diag = list(continuous = "bar"), axisLabels = "none")



### Reg siimple

simpleModel <- lm(MontoInversion~ Patrimonio,data=BDa2censo)
summary(simpleModel)

plot(BDa2censo$Patrimonio,BDa2censo$MontoInversion)
abline(simpleModel)

###
simpleModel2 <- lm(Patrimonio ~ Edad,data=BDa2censo)
summary(simpleModel2)

plot(BDa2censo$Edad,BDa2censo$Patrominio)
abline(simpleModel2)
