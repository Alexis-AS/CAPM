# Instalar librerías si no están instaladas
if (!require("readxl")) install.packages("readxl")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyr")) install.packages("tidyr") # Para pivot_longer

# Cargar librerías
library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

# Configuración de carpetas
input_folder <- "C:/Users/usuario/OneDrive - Universidad Técnica Federico Santa María/2025/Paper CAPM/Simulaciones"
output_folder <- file.path(dirname(input_folder), "Graficos_Simulaciones")  # Subcarpeta única para gráficos

# Crear el nuevo directorio si no existe
if (!dir.exists(nueva_ruta)) {
  dir.create(nueva_ruta)
}

# Obtener lista de archivos que empiezan con "Resultados_Alpha"
archivos <- list.files(ruta_directorio, pattern = "^Resultados_Alpha.*\\.xlsx$", full.names = TRUE)

# Procesar cada archivo
for (archivo in archivos) {
  # Cargar el archivo Excel
  datos <- read_excel(archivo)
  
  # Nombre del archivo
  nombre_archivo <- basename(archivo)
  
  # Extraer y transformar el valor de Beta
  beta_string <- sub(".*_Beta_", "", nombre_archivo)
  beta_string <- sub("_.*", "", beta_string)
  beta_modificado <- gsub("Neg", "-", beta_string)
  beta_modificado <- gsub("coma", ".", beta_modificado)
  
  # Convertir a numérico
  beta_inicial <- as.numeric(beta_modificado)
  
  # Verificar y llenar las entradas vacías de Beta_Inicial
  if ("Beta_Inicial" %in% colnames(datos)) {
    # Reemplazar únicamente las celdas vacías con el valor de beta_inicial
    datos$Beta_Inicial[is.na(datos$Beta_Inicial) | datos$Beta_Inicial == ""] <- beta_inicial
  } else {
    # Crear la columna Beta_Inicial y llenarla con el valor
    datos$Beta_Inicial <- beta_inicial
  }
  
  # Crear el nombre del nuevo archivo
  nuevo_nombre <- file.path(nueva_ruta, paste0("Actualizado_", nombre_archivo))
  
  # Guardar los cambios en el nuevo archivo Excel
  write.xlsx(datos, nuevo_nombre)
}

cat("Todos los archivos han sido procesados y guardados en el nuevo directorio.\n")