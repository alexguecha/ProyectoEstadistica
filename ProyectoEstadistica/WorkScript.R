BdCrowfunding<-read.table(file.choose(), header = T, sep = ";")
fix(BdCrowfunding)

knitr::opts_chunk$set(echo = TRUE)
SectorCampaña_= BdCrowfunding$SectorCampaña
SCtabla=data.frame(table(SectorCampaña_))
porcentaje=prop.table(SCtabla[,2])
SCtabla2= cbind(SCtabla, porcentaje)
cum_frequencia=cumsum(SCtabla2[,2])
SCtabla3= cbind(SCtabla2, cum_frequencia)
cum_porcentaje=cumsum(SCtabla3[,3])
SCtabla4= cbind(SCtabla3, cum_porcentaje)
SCtabla4
knitr::kable(
  (SCtabla4)
)


pie(SCtabla4[,3],labels=SCtabla4[,1], clockwise=TRUE,radius=1,border="black",main="Diagrama de Sectores")


Campaña= BdCrowfunding$Campaña
Ctabla=data.frame(table(Campaña))
porcentaje=prop.table(Ctabla[,2])
Ctabla2= cbind(Ctabla, porcentaje)
cum_frequencia=cumsum(Ctabla2[,2])
Ctabla3= cbind(Ctabla2, cum_frequencia)
cum_porcentaje=cumsum(Ctabla3[,3])
Ctabla4= cbind(Ctabla3, cum_porcentaje)
Ctabla4
knitr::kable(
  (Ctabla4)
)

colors <- c(2, 3, 4, 5, 6, 7,8)
BPCampaña <- barplot(prop.table(table(BdCrowfunding$Camp)),
                           main="Frecuencias relativas por sector de campaña",ylab ="Frecuencias Relativas",las=2 )


BPCampaña <- barplot(prop.table(table(BdCrowfunding$Camp)),
                     main="Frecuencias relativas por sector de campaña",ylim=c(0,0.07),ylab ="Frecuencias Relativas",las=2 )

pie(Ctabla4[,3],labels=Ctabla4[,1], clockwise=TRUE,radius=1,border="black",main="Diagrama de Sectores")




CalidadTributaria= BdCrowfunding$CalidadTributaria
CTtabla=data.frame(table(CalidadTributaria))
porcentaje=prop.table(CTtabla[,2])
CTtabla2= cbind(CTtabla, porcentaje)
cum_frequencia=cumsum(CTtabla2[,2])
CTtabla3= cbind(CTtabla2, cum_frequencia)
cum_porcentaje=cumsum(CTtabla3[,3])
CTtabla4= cbind(CTtabla3, cum_porcentaje)
CTtabla4
knitr::kable(
  (CTtabla4)
)

colors <- c(2, 3, 4, 5, 6)
BPCalidadTributaria <- barplot(prop.table(table(BdCrowfunding$CalidadTributaria)), col= colors,main="Frecuencias relativas por Calidad Tributaria",ylim=c(0,0.55),ylab ="Frecuencias Relativas",las=1 )

barplot(table(BdCrowfunding$CalidadTributaria), col = "blue", main="Diagrama de barras",ylim=c(0,13000),las=1, cex.names = 0.5, lwd = 1.5, cex.axis=0.9, xlab="Calidad Tributaria", ylab = "Conteos")

pie(CTtabla4[,3],labels=CTtabla4[,1], clockwise=TRUE,radius=1,border="black",main="Diagrama de Sectores")

Pais= BdCrowfunding$Pais
Ptabla=data.frame(table(Pais))
porcentaje=prop.table(Ptabla[,2])
Ptabla2= cbind(Ptabla, porcentaje)
cum_frequencia=cumsum(Ptabla2[,2])
Ptabla3= cbind(Ptabla2, cum_frequencia)
cum_porcentaje=cumsum(Ptabla3[,3])
Ptabla4= cbind(Ptabla3, cum_porcentaje)
Ptabla4
knitr::kable(
  (Ptabla4)
)

colors <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
BPPais <- barplot(prop.table(table(BdCrowfunding$Pais)), col= colors,main="Frecuencias relativas por País",ylim=c(0,1.5),ylab ="Frecuencias Relativas",las=2 )

pie(Ptabla4[,3],labels=Ptabla4[,1], clockwise=TRUE,radius=1,border="black",main="Diagrama de Sectores")


#Ciudad
Ciudad= BdCrowfunding$Ciudad
Cdtabla=data.frame(table(Ciudad))
porcentaje=prop.table(Cdtabla[,2])
Cdtabla2= cbind(Cdtabla, porcentaje)
cum_frequencia=cumsum(Cdtabla2[,2])
Cdtabla3= cbind(Cdtabla2, cum_frequencia)
cum_porcentaje=cumsum(Cdtabla3[,3])
Cdtabla4= cbind(Cdtabla3, cum_porcentaje)
Cdtabla4
knitr::kable(
  (Cdtabla4)
)

barplot(table(BdCrowfunding$Ciudad), col = "blue", main="Diagrama de barras",ylim=c(0,13000),las=2, cex.names = 0.5, lwd = 1.5, cex.axis=0.9, xlab="Ciudad", ylab = "Conteos")


pie(Cdtabla4[,3],labels=Cdtabla4[,1], clockwise=TRUE,radius=1,border="black",main="Diagrama de Sectores")


#Departamento
Departamento= BdCrowfunding$Departamento
Dotabla=data.frame(table(Departamento))
porcentaje=prop.table(Dotabla[,2])
Dotabla2= cbind(Dotabla, porcentaje)
cum_frequencia=cumsum(Dotabla2[,2])
Dotabla3= cbind(Dotabla2, cum_frequencia)
cum_porcentaje=cumsum(Dotabla3[,3])
Dotabla4= cbind(Dotabla3, cum_porcentaje)
Dotabla4
knitr::kable(
  (Dotabla4)
)

barplot(table(BdCrowfunding$Departamento), col = "blue", main="Diagrama de barras",ylim=c(0,13000),las=2, cex.names = 0.5, lwd = 1.5, cex.axis=0.9, xlab="Departamento", ylab = "Conteos")
pie(Dotabla4[,3],labels=Dotabla4[,1], clockwise=TRUE,radius=1,border="black",main="Diagrama de Sectores")

library(plotly)

plot_ly(Dotabla4, labels = Dotabla4[,1], values = Dotabla4[,3], type = "pie") %>%
  layout(title = "Diagrama de sectores por Departamentos")

#SECTOR ECONOMICO


SectorEconomico= BdCrowfunding$SectorEconomico
SEtabla=data.frame(table(SectorEconomico))
porcentaje=prop.table(SEtabla[,2])
SEtabla2= cbind(SEtabla, porcentaje)
cum_frequencia=cumsum(SEtabla2[,2])
SEtabla3= cbind(SEtabla2, cum_frequencia)
cum_porcentaje=cumsum(SEtabla3[,3])
SEtabla4= cbind(SEtabla3, cum_porcentaje)
SEtabla4
knitr::kable(
  (SEtabla4)
)

barplot(table(BdCrowfunding$SectorEconomico), col = "blue", main="Diagrama de barras",ylim=c(0,10000),las=2, cex.names = 0.5, lwd = 1.5, cex.axis=0.9, xlab="Sector Economico", ylab = "Conteos")

plot_ly(SEtabla4, labels = SEtabla4[,1], values = SEtabla4[,3], type = "pie") %>%
  layout(title = "Diagrama de sectores por Sector Económico")

colors <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,21,22,23,24,25,26,27,28,29)
BPSectorEconomico <- barplot(prop.table(table(BdCrowfunding$SectorEconomico)), col= colors,main="Frecuencias relativas por Sector Económico",ylim=c(0,0.5),ylab ="Frecuencias Relativas",las=2 )

pie(SEtabla4[,3],labels=SEtabla4[,1], clockwise=TRUE,radius=1,border="black",main="Diagrama de Sectores")

TipoInversionista= BdCrowfunding$TipoInversionista
TItabla=data.frame(table(TipoInversionista))
porcentaje=prop.table(TItabla[,2])
TItabla2= cbind(TItabla, porcentaje)
cum_frequencia=cumsum(TItabla2[,2])
TItabla3= cbind(TItabla2, cum_frequencia)
cum_porcentaje=cumsum(TItabla3[,3])
TItabla4= cbind(TItabla3, cum_porcentaje)
TItabla4
knitr::kable(
  (TItabla4)
)

barplot(table(BdCrowfunding$TipoInversionista), col = "blue", main="Diagrama de barras",ylim=c(0,21000),las=2, cex.names = 0.5, lwd = 1.5, cex.axis=0.9, xlab="Tipo Inversionista", ylab = "Conteos")

plot_ly(TItabla4, labels = TItabla4[,1], values = TItabla4[,3], type = "pie") %>%
  layout(title = "Diagrama de sectores por Tipo Inversionista")

colors <- c(2, 3)
BPTipoInversionista <- barplot(prop.table(table(BdCrowfunding$TipoInversionista)), col= colors,main="Frecuencias relativas por Tipo de Inversionista",ylim=c(0,1),ylab ="Frecuencias Relativas",las=2 )


Proposito = BdCrowfunding$Propósito
PTtabla=data.frame(table(Proposito))
porcentaje=prop.table(PTtabla[,2])
PTtabla2= cbind(PTtabla, porcentaje)
cum_frequencia=cumsum(PTtabla2[,2])
PTtabla3= cbind(PTtabla2, cum_frequencia)
cum_porcentaje=cumsum(PTtabla3[,3])
PTtabla4= cbind(PTtabla3, cum_porcentaje)
PTtabla4
knitr::kable(
  (PTtabla4)
)
summary(BdCrowfunding$IngresosMes)

library(fdth)
is.na(x = BdCrowfunding$IngresosMes)
sum(is.na(x = BdCrowfunding$IngresosMes))
IMtabla1 <- fdt(sort(BdCrowfunding$IngresosMes)[1000000:10000000], k = 5, na.rm = T)
IMtabla <- fdt(BdCrowfunding$IngresosMes, start=0, end=11000000000, h=10)
IMtabla <- fdt(BdCrowfunding$IngresosMes,breaks="Sturges",right=F)
IMtabla <- fdt(sort(BdCrowfunding$IngresosMes)[1000000:10000000],breaks="Sturges")
IMtabla
IMtabla1
knitr::kable(
  (IMtabla)
)
IngresosMes = BdCrowfunding$IngresosMes
IMtabla=data.frame(table(IngresosMes))
porcentaje=prop.table(IMtabla[,2])
IMtabla2= cbind(IMtabla, porcentaje)
cum_frequencia=cumsum(IMtabla2[,2])
IMtabla3= cbind(IMtabla2, cum_frequencia)
cum_porcentaje=cumsum(IMtabla3[,3])
IMtabla4= cbind(IMtabla3, cum_porcentaje)
IMtabla4
knitr::kable(
  (IMtabla4)
)

POtabla <- fdt(BdCrowfunding$Patrimonio,breaks="Sturges",right=F)

knitr::kable(
  (POtabla)
)

MItabla <- fdt(BdCrowfunding$MontoInversion,breaks="Sturges",right=F)

MItabla <- fdt(BdCrowfunding$MontoInversion,breaks="Sturges",right=F)

MItabla <- fdt(BdCrowfunding$MontoInversion, start=49500, end=156752000, h=31340500)

knitr::kable(
  (MItabla)
)

Edtabla <- fdt(BdCrowfunding$Edad,breaks="Sturges",right=F)

knitr::kable(
  (Edtabla)
)

MItabla <- fdt(BdCrowfunding$Edad, start=2, end=90, h=17.6)

summary(BdCrowfunding$Edad)

CItabla <- fdt(BdCrowfunding$CantidadInversiones, start=1, end=85, h=16.8)

knitr::kable(
  (CItabla)
)

summary(BdCrowfunding$CantidadInversiones)

CItabla <- fdt(BdCrowfunding$CantidadInversiones,breaks="Sturges",right=F)

CItabla <- fdt(BdCrowfunding$CantidadInversiones,breaks="Sturges")

CItabla <- fdt(BdCrowfunding$CantidadInversiones, k=5)
Edtabla <- fdt(BdCrowfunding$Edad,k=5)

IMtabla <- fdt(BdCrowfunding$IngresosMes,k=5)
knitr::kable(
  (IMtabla)
)

MItabla <- fdt(BdCrowfunding$MontoInversion, start=50000, end=156752000, h=31030000)

knitr::kable(
  (MItabla)
)


CItabla <- fdt(BdCrowfunding$CantidadInversiones, start=1, end=85, h=14)
knitr::kable(
  (CItabla)
)


IMtabla <- fdt(BdCrowfunding$IngresosMes,K=5)
IMtabla <- fdt(BdCrowfunding$IngresosMes,k=5)

knitr::kable(
  (IMtabla)
)

plot(IMtabla,type='fh',main="Histograma de frecuencias absolutas",
     xlab="Ingresos Mes",ylab="Conteos",col="red") 

plot(IMtabla,type='fp',main="Poligono de frecuencias absolutas",
     xlab="Ingresos Mes",ylab="Conteos",col="black",lwd=2) 

plot(IMtabla,type='rfpp',main="Poligono de frecuencias relativas (%)",
     xlab="Ingresos",ylab="Porcentaje",col="black",lwd=2)

plot(IMtabla,type='cfp',main="Poligono de frecuencias absolutas acumuladas",
     xlab="Ingresos",ylab="Conteos",col="black") 


library(agricolae)
par(mfrow=c(1,2),mar=c(4,4,0,1),cex=1)
h1<-graph.freq(BdCrowfunding$IngresosMes, density=6, col="blue",border="red",ylim=c(0,1), frequency=2,xlab="population")
h2<-graph.freq(BdCrowfunding$IngresosMes, border=0,ylim=c(0,1), frequency=2,xlab="population")
polygon.freq(h2,col="blue", frequency=2)