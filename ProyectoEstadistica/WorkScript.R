BdCrowfunding<-read.table(file.choose(), header = T, sep = ";")
fix(BdCrowfunding)

knitr::opts_chunk$set(echo = TRUE)
SectorCampaña_= BdCrowfunding$SectorCamp
SCtabla=data.frame(table(SectorCampaña_))
porcentaje=prop.table(SCtabla[,2])
SCtabla2= cbind(SCtabla, porcentaje)
cum_frequencia=cumsum(SCtabla2[,2])
SCtabla3= cbind(SCtabla2, cum_frequencia)
cum_porcentaje=cumsum(SCtabla3[,3])
SCtabla4= cbind(SCtabla3, cum_porcentaje)
SCtabla4
knitr::kable(
  head(SCtabla4)
)

knitr::kable(
  (SCtabla4)
)

colors <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

BPSectorCampaña <- barplot(prop.table(table(BdCrowfunding$SectorCamp)),col= colors,main="Frecuencias relativas por sector de campaña",ylab ="Frecuencias Relativas",las=2 )


BPSectorCampaña <- barplot(prop.table(table(BdCrowfunding$SectorCamp)),col= colors,
                           main="Frecuencias relativas por sector de campaña",ylim=c(0,0.25),ylab ="Frecuencias Relativas",las=2 )


pie(SCtabla4[,3],labels=SCtabla4[,1], clockwise=TRUE,radius=1,border="black",main="Diagrama de Sectores")


Campaña= BdCrowfunding$Camp
Ctabla=data.frame(table(Campaña))
porcentaje=prop.table(Ctabla[,2])
Ctabla2= cbind(Ctabla, porcentaje)
cum_frequencia=cumsum(Ctabla2[,2])
Ctabla3= cbind(Ctabla2, cum_frequencia)
cum_porcentaje=cumsum(Ctabla3[,3])
Ctabla4= cbind(Ctabla3, cum_porcentaje)
Ctabla4
knitr::kable(
  head(Ctabla4)
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
colors <- c(2, 3)
BPTipoInversionista <- barplot(prop.table(table(BdCrowfunding$TipoInversionista)), col= colors,main="Frecuencias relativas por Tipo de Inversionista",ylim=c(0,1),ylab ="Frecuencias Relativas",las=2 )


Proposito = BdCrowfunding$Proposito
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




















