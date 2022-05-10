

#====================================================================
# 1. Diagrama de Dispersión Codificado por Color (Diagrama de Color):
#====================================================================
library(sp, rgl)
library(raster)
library(scatterplot3d)


# Cargar el archivo de área de trabajo (geog495) el cual contiene
# la base specmap y muchas otras.


# Algo de información al respecto
# https://specmap.apps.pgc.umn.edu/
attach(specmap)    
fix(specmap)  # 783 casos


plot(O18 ~ Insol, pch=16, cex=0.6)
cor(O18, Insol)

# detach(specmap)



#Realizando un mejor gráfico:
#----------------------------
library(RColorBrewer)
library(classInt) # recodificación de los intervalo de clase
plotvar <- Insol
nclr <- 5
plotclr <- brewer.pal(nclr,"PuOr")
plotclr <- plotclr[nclr:1] # reordenando los colores
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)

plot(O18 ~ Age, ylim=c(2.5,-2.5), type="l")
points(O18 ~ Age, pch=16, col=colcode, cex=1.5)





#=================================================================
# 2. Colores y Símbolos:
#=================================================================
attach(sumcr)
fix(sumcr)
head(sumcr)

plot(WidthWS ~ CumLen, pch=as.integer(Reach), col=as.integer(HU))
legend(25, 2, c("Reach A", "Reach B", "Reach C"), pch=c(1,2,3), col=1)
legend(650, 2, c("Glide", "Pool", "Riffle"), pch=1, col=c(1,2,3))


detach(sumcr)


#=================================================================
# 3. Colores y Símbolos: Bubble Chart
#=================================================================

# Un gráfico básico de X versus Y
attach(orstationc)
head(orstationc)

plot(pjan ~ tann)



# Un gráfico básico de X versus Y, con bubbles
plot(orstationc$tann, orstationc$pjan, type="n")
symbols(tann, pjan, circles=elev, inches=0.1, add=T)


# Ahora un gráfico de burbulas, mejorado aunque simple
# Cargando librerias necesarias
library(ggplot2)
library(dplyr)

# El bubble chart
ggplot(orstationc, aes(x=tann, y=pjan, size = elev)) +
  geom_point(alpha=0.7)





# Otro Ejemplo de Bubble Chart, algo más elaborado:
#---------------------------------------
library(ggplot2)
theme_set(theme_bw() + 
    theme(legend.position = "top")
)

# Cargando los datos
data("mtcars")
head(mtcars)
df <- mtcars

# Convirtiendo cyl como una variable de agrupación
df$cyl <- as.factor(df$cyl)

# Inspeccionando solo las variables de trabajo
head(df[, c("wt", "mpg", "cyl", "qsec")], 4)


# Graficando el bubble con ggplot:
ggplot(df, aes(x = wt, y = mpg)) + 
  geom_point(aes(color = cyl, size = qsec), alpha = 0.5) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_size(range = c(0.5, 12))  # Ajustando el tamaño de los puntos








#============================================================================
# 4. Diagramas de dispersión 3D: Gráfico de nubes de puntos tridimensionales
#============================================================================
library(lattice)

attach(orstationc)
cloud(tann ~ pjan * elev, data = orstationc)


cloud(elev ~ lon*lat, data = orstationc)





# Otro Cloud Chart, algo más elaborado:
#---------------------------------------

x <- seq(-pi, pi, len = 20)
y <- seq(-pi, pi, len = 20)
g <- expand.grid(x = x, y = y)
g$z <- sin(sqrt(g$x^2 + g$y^2))
wireframe(z ~ x * y, g, drape = TRUE, col.regions = "white",
          aspect = c(3,1), colorkey = FALSE)
data(iris)
cloud(Sepal.Length ~ Petal.Length * Petal.Width, data = iris,
      groups = Species, screen = list(z = 20, x = -70),
      subpanel = panel.superpose,
      key = list(title = "Iris Data", x = .15, y=.85, corner = c(0,1),
                 border = TRUE, 
                 points = Rows(trellis.par.get("superpose.symbol"), 1:3),
                 text = list(levels(iris$Species))))
print(cloud(Sepal.Length ~ Petal.Length * Petal.Width, 
            data = iris, cex = .8, perspective = FALSE,
            groups = Species, 
            subpanel = panel.superpose,
            main = "Stereo",
            screen = list(z = 20, x = -70, y = 3)),
      split = c(1,1,2,1), more = TRUE)
print(cloud(Sepal.Length ~ Petal.Length * Petal.Width,
            data = iris, cex = .8, perspective = FALSE,
            groups = Species,
            subpanel = panel.superpose,
            main = "Stereo",
            screen = list(z = 20, x = -70, y = 0)),
      split = c(2,1,2,1))





# Otro Cloud Chart, mucho más elaborado que el anterior:
#---------------------------------------

library(scatterplot3d)
library(RColorBrewer)

library(maptools)
library(maps)
library(RColorBrewer)

# Obtener colores para etiquetar los puntos
plotvar <- pann # Crear una variable para graficar
nclr <- 8 # numero de colores
plotclr <- brewer.pal(nclr,"PuBu") # obtener los colores
colornum <- cut(rank(tjul), nclr, labels=FALSE)
colcode <- plotclr[colornum] # asignar colores


# Gráfco de dispersión
plot.angle <- 45
scatterplot3d(tann, pjan, tjul, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=2,col.axis="gray", col.grid="gray")





#===================================================
library(RColorBrewer)
library(FactoClass)
library(classInt) # class-interval recoding library
plotvar <- Insol

# 3D scatter plot 
s3d <- scatterplot3d(iris[, 1:3], pch ="2", grid=FALSE, box=FALSE)

# Adicionando grilla
addgrids3d(iris[, 1:3], grid = c("xy", "xz", "yz"))

# Modificando la forma de los puntos
s3d$points3d(iris[, 1:3], pch = 16)
legend(s3d$xyz.convert(7.5, 3, 4.5), legend = levels(iris$Species), pch = 16)


# Add regression plane and supplementary points
data(trees)
head(trees)



# 3D scatter plot con modelo de regresión:
  
#3D scatter plot
s3d <- scatterplot3d(trees, type = "h", color = "blue", angle=55, pch = 16)

# Ajustando el modelo de regresion
my.lm <- lm(trees$Volume ~ trees$Girth + trees$Height)
s3d$plane3d(my.lm)

# Agregando el modelo de regresion al scaret plot
s3d$points3d(seq(10, 20, 2), seq(85, 60, -5), seq(60, 10, -10),
             col = "red", type = "h", pch = 8)








#===================================================

library(scatterplot3d)
library(RColorBrewer)

# get colors for labeling the points
plotvar <- pann # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"PuBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color

# scatter plot
plot.angle <- 45
scatterplot3d(lon, lat, plotvar, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=2, 
              col.axis="gray", col.grid="gray")







# Relacionando Cartografía a un Cloud Chart:
#-------------------------------------------
library(maps)

# get points that define Oregon county outlines
or.map <- map("county", "oregon", xlim=c(-125,-114), ylim=c(42,47), plot=FALSE)

# get colors for labeling the points
plotvar <- pann # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"PuBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color

# scatterplot and map
plot.angle <- 45
s3d <- scatterplot3d(lon, lat, plotvar, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=2, col.axis="gray", col.grid="gray")
s3d$points3d(or.map$x,or.map$y,rep(0,length(or.map$x)), type="l")











#============================================================================
# 5. Gráficos de Superficie y Puntos OpenGL
#============================================================================

library(rgl)
example(rgl.surface)

rgl.clear()

example(rgl.spheres)





#============================================================================
# 6. Gráficos Trellis  (Glifos)/ Lattice - Mapa
#============================================================================

library(lattice)
attach(scanvote)
head(scanvote)

coplot(Yes ~ log10(Pop) | Country, columns=3,
       panel=function(x,y,...) {
         panel.smooth(x,y,span=.8,iter=5,...)
         abline(lm(y ~ x), col="blue") } )


library(sp)
attach(yellpratio)
head(yellpratio)


# Mapa Simple
# Gráfico de Yellowstone
plot(ynpstate.shp)
plot(ynplk.shp, add=T)
plot(ynprivers.shp, add=T)
points(Lon, Lat)

# Gráfico de estrellas para la tasa de precipitación
col.red <- rep("red",length(orstationc[,1]))
stars(yellpratio[,4:15], locations=as.matrix(cbind(Lon, Lat)),
      col.stars=col.red, len=0.2, key.loc=c(-111.5,42.5), labels=NULL, add=T)






#============================================================================
# 7.scatter plot matrix
#============================================================================
library(GGally)
library(ggplot2)
ggpairs(iris[,-5])+ theme_bw()



# Otro Scatter Plot Matrix, mucho más elaborado:
#-----------------------------------------------


p <- ggpairs(iris, aes(color = Species))+ theme_bw()
# Cambiando el color manualmente.
# Recorre cada gráfico cambiando escalas relevantes
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")) +
      scale_color_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))  
  }
}
p






# Otro Scatter Plot Matrix, con inferencia:
#-----------------------------------------------

# Instalando el paquete ("PerformanceAnalytics")
library("PerformanceAnalytics")
my_data <- mtcars[, c(1,3,4,5,6,7)]
chart.Correlation(my_data, histogram=TRUE, pch=19)




library("ggcorrplot")
# Calculando la matriz de correlaciones
my_data <- mtcars[, c(1,3,4,5,6,7)]
corr <- round(cor(my_data), 1)

# Visualizando
ggcorrplot(corr, p.mat = cor_pmat(my_data),
           hc.order = TRUE, type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE)




# ================================================================
# ================================================================"
#  Reducción de Dimensión:  ACP
#=================================================================

library( rgl )
library(cluster)
library(FactoMineR)
library(ggplot2)
library(factoextra)
library(corrplot)
library( psych)
library(colourpicker)
library(shiny)
library( rgl )
library(cluster)





# La base de trabajo:
df <- read.table("http://factominer.free.fr/book/temperature.csv", header= T, sep = ";", row.names = 1)
head(df)


# Realizar PCA
df$Area<-factor(df$Area)
df_pca <- PCA( df, graph = FALSE, scale.unit = TRUE, quanti.sup = 13:16, quali.sup = 17 )
print(df_pca)

# Para visualizar los resultados
# df_pca$eig



#Dibujar variables e individuos – juntos o en dos gráficas 
fviz_pca_biplot( df_pca, habillage = as.factor(df$Area))


#Interpretar los resultados, argumentando la interpretación.
df_pca$eig
df_pca$var$cor




#Comp 1 tiene 86,87% de varianza. 
# Con los primeros dos componentes se puede expliacar casi toda la 
# varianza de datos (98,29%).
fviz_screeplot( df_pca, ncp = 12 )





#Clasificación jerárquica sobre los datos originales.
distdf <- dist( df[,1:12], method = "euclidean" )
hclu <- hclust( distdf, method = "ward.D" )
dendro <- as.dendrogram( hclu ) 
plot( dendro , horiz = FALSE, type = "r" )
rect.hclust( hclu, k = 3 )



cluster <- as.factor( cutree( hclu, k = 3 ) )
#Clasificación jerárquica sobre los datos obtenidos al aplicar componentes principales
datos_pca_hcpc <- HCPC( df_pca, nb.clust = -1 )











#============================================================================
# 8. Cluster Analysis
#============================================================================



library(factoextra)
USArrests %>%
  scale() %>%                                        # Reescalando los datos
  dist() %>%                                         # Calcula la matriz de distancias
  hclust(method = "ward.D2") %>%                     # Construcciónde los clusters jarárquicos
  fviz_dend(cex = 0.5, k = 4, palette = "jco")       # Visualizando y cortando 4 grupos





# Cortar el árbol para generar los clusters
#------------------------------------------
library(factoextra)
datos <- USArrests
datos <- scale(datos)
set.seed(101)

hc_euclidea_completo <- hclust(d = dist(x = datos, method = "euclidean"),
                               method = "complete")

fviz_dend(x = hc_euclidea_completo, k = 5, cex = 0.6) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=4")





# Otra forma:
set.seed(5665)
fviz_dend(x = hc_euclidea_completo,
          k = 4,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,
          cex = 0.5,
          type = "circular")




# Cluster post ACP
#------------------------------------------
fviz_cluster(object = list(data=datos, cluster=cutree(hc_euclidea_completo, k=3)),
             ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE,
             labelsize = 8)  +
  labs(title = "Hierarchical clustering + Proyección PCA",
       subtitle = "Distancia euclídea, Lincage complete, K=3") +
  theme_bw() +
  theme(legend.position = "bottom")





# Cluster post ACP con forma de elipsoide
#------------------------------------------
library(cluster)
fuzzy_cluster <- fanny(x = datos, diss = FALSE, k = 3, metric = "euclidean",
                       stand = FALSE)
head(fuzzy_cluster$membership)

library(factoextra)
fviz_cluster(object = fuzzy_cluster, repel = TRUE, ellipse.type = "norm",
             pallete = "jco") + theme_bw() + labs(title = "Fuzzy Cluster plot")









#============================================================================
# 8.2. Heatmaps
#============================================================================
datos <- mtcars
# Para que las variables sean comparables bajo un mismo esquema de colores se
# estandarizan.
datos <- scale(datos)
heatmap(x = datos, scale = "none", distfun = function(x){dist(x, method = "euclidean")},
        hclustfun = function(x){hclust(x, method = "average")},
        cexRow = 0.7)












#============================================================================
# 9. Análisis de Correspondecias Múltiples
#============================================================================

# Cargando los paquetes
require(FactoMineR)
require(ggplot2)

# Cargando la base de trabajo
data(tea)
# Encabezado de la base
head(tea)


# Seleccionando las variables de trabajo
newtea = tea[, c("Tea", "How", "how", "sugar", "where", "always")]

# Encabezado de la base
head(newtea)

# Número de Categorías por Variables
cats = apply(newtea, 2, function(x) nlevels(as.factor(x)))

cats



# Aplicando MCA
mca1 = MCA(newtea, graph = FALSE)

# lISTA DE POSIBLES RESULTADOS DEL MCA
mca1


# Tabla de eigenvalues
mca1$eig

# Base de datos con las variables coordenadas
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), cats))

# Base de datos con la observaciones coordenadas
mca1_obs_df = data.frame(mca1$ind$coord)

# Gráfico de las categorías de las variables (MAPA PERCEPTUAL)
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")





# Para tener una representación más interesante, podríamos superponer una 
# visualización gráfica de las observaciones y las categorías. Además, 
# dado que algunos individuos se superpondrán, podemos agregar algunas 
# curvas de densidad

# Gráfico de observaciones y categorías de MCA
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca1_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca1_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable")





