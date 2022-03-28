library(dplyr)
datos <- as.data.frame(state.x77)

datos

datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, muerte = Murder, graduados = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)

datos <- mutate(.data = datos, densidad_pobl = habitantes * 1000 / area)
datos
round(cor(x = datos, method = "pearson"), 3)
