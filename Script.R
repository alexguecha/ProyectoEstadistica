library(readxl)
Base_de_datos_inversionistas_depurada_v2 <- read_excel("Base de datos inversionistas depurada v2.xlsx", 
                                                       col_types = c("skip", "text", "text", 
                                                                     "skip", "skip", "skip", "skip", "text", 
                                                                     "text", "skip", "skip", "skip", "text", 
                                                                     "text", "numeric", "numeric", "text", 
                                                                     "skip", "numeric", "skip", "skip", 
                                                                     "numeric", "skip", "numeric", "skip", 
                                                                     "skip", "skip", "numeric", "skip"))
View(Base_de_datos_inversionistas_depurada_v2)