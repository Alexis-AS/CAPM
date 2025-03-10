#Instalación de paquetes para la importación
#install.packages("quantmod")
#install.packages("imf.data")
#install.packages("dplyr")
#install.packages("zoo")
#install.packages("tseries")
#install.packages("e1071")
#install.packages("forecast")
#install.packages("stringr")
#install.packages("lmtest")
#install.packages("tibble")
#install.packages("nortest")
#install.packages("tidyr")
#install.packages("purrr")
#install.packages("tidyverse")
#install.packages("moments")
#install.packages("car")
#install.packages("plm")
#install.packages("openxlsx")
#install.packages("stabledist")
#install.packages("statmod")
#install.packages("finity")
#install.packages("stabreg")
#install.packages("parallel")
#install.packages("ggplot2")
#install.packages("here")

#Posibles ubicaciones
#setwd("C:/Users/alexi/OneDrive - Universidad Técnica Federico Santa María/2025/Paper CAPM/Bases de Datos/Control Variables")
setwd("C:/Users/usuario/OneDrive - Universidad Técnica Federico Santa María/2025/Paper CAPM/Bases de Datos/Control Variables")

#Librerías
library(dplyr)
library(imf.data)
library(zoo)
library(tseries)
library(e1071)
library(forecast)
library(lmtest)
library(quantmod)
library(stringr)
library(tibble)
library(nortest)
library(tidyr)
library(tidyverse)
library(purrr)
library(openxlsx)
library(stabledist)
library(statmod)
library(finity)
library(parallel)
library(ggplot2)
library(here)
#library(stabreg)

# Horizonte de trabajo
start_date <- "2017-10-01"
end_date <- "2024-07-01"
fecha_inicial <- as.Date(start_date)
fecha_final <- as.Date(end_date)
fechas_completas <- data.frame(Date = seq(fecha_inicial, fecha_final, by = "day"))

# Cargar el archivo CSV
database <- read.csv("Database.csv", stringsAsFactors = FALSE)

# Reemplazar "." con interpolación lineal
procesar_columna <- function(col) {
  # Reemplazar "." con NA
  col[col == "."] <- NA
  col <- as.numeric(col) # Convertir a numérico si no lo es
  # Aplicar interpolación lineal para valores faltantes
  col <- na.approx(col, na.rm = FALSE, rule = 2) 
  return(col)
}

# Aplicar la función a todas las columnas excepto la columna de fecha
database[, -1] <- lapply(database[, -1], procesar_columna)

# Calcular el logaritmo natural para cada columna numérica
calcular_ln <- function(col) {
  # Reemplazar valores <= 0 con NA para evitar problemas con log
  col <- ifelse(col > 0, log(col), NA)
  return(col)
}

# Crear un nuevo dataframe con logaritmos naturales
database_ln <- database
database_ln[, -1] <- lapply(database_ln[, -1], calcular_ln)

# Guardar los resultados en un archivo CSV
write.csv(database_ln, "Database_CV.csv", row.names = FALSE)

# Mostrar las primeras filas del dataframe procesado
head(database_ln)