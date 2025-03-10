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
#install.packages("sn")
#setwd("C:/Users/alexi/OneDrive - Universidad Técnica Federico Santa María/2025/Paper CAPM/Bases de Datos")
setwd("C:/Users/usuario/OneDrive - Universidad Técnica Federico Santa María/2025/Paper CAPM/Bases de Datos")
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
library(pbapply)
library(sn)
#library(stabreg)
start_date <- "2017-10-01"
end_date <- "2020-01-01"
fecha_inicial <- as.Date(start_date)
fecha_final <- as.Date(end_date)
fechas_completas <- data.frame(Date = seq(fecha_inicial, fecha_final, by = "day"))

# Importar el archivo final_df.csv desde una carpeta específica
final_df <- read.csv("final_df.csv")

# Asegurarte de que la columna esté en tipo Date con el formato original
final_df$Date <- as.Date(final_df$Date, format = "%m/%d/%Y")

# Transformar al formato "yyyy-mm-dd"
final_df$Date <- format(final_df$Date, format = "%Y-%m-%d")
# Identificar el nombre de la última columna
Rm_Rf <- names(final_df)[ncol(final_df)]

# Calcular el cuadrado y el cubo de los valores de la última columna
final_df$Rm_Rf2 <- final_df[[Rm_Rf]]^2
final_df$Rm_Rf3 <- final_df[[Rm_Rf]]^3
# Definir el directorio de trabajo donde se encuentra el archivo Database_CV.csv
#setwd("C:/Users/alexi/OneDrive - Universidad Técnica Federico Santa María/2025/Paper CAPM/Bases de Datos/Control Variables")
setwd("C:/Users/usuario/OneDrive - Universidad Técnica Federico Santa María/2025/Paper CAPM/Bases de Datos/Control Variables")

# Importar el archivo Database_CV.csv
database_cv <- read.csv("Database_CV.csv", stringsAsFactors = FALSE)

# Asegurarte de que la columna Date esté en formato Date en ambos dataframes
database_cv$Date <- as.Date(database_cv$Date, format = "%Y-%m-%d")
final_df$Date <- as.Date(final_df$Date)

# Realizar el join entre final_df y database_cv utilizando la columna Date como clave
final_df_filtrado <- final_df %>%
  left_join(database_cv, by = "Date")

# Función para ajustar la serie eliminando NAs y verificando datos
ajustar_serie2 <- function(retornos_empresa) {
  # Verificar si todos los valores son NA
  if (all(is.na(retornos_empresa$Retorno))) {
    return(data.frame(Date = retornos_empresa$Date, Retorno = NA))
  }
  
  # Buscar el índice de la primera fila donde el retorno no es NA
  start_index <- which(!is.na(retornos_empresa$Retorno))[1]
  
  # Si no hay valores no NA, retornar un dataframe vacío
  if (is.na(start_index)) {
    return(data.frame(Date = retornos_empresa$Date, Retorno = NA))
  }
  
  # Filtrar los datos desde la primera fila sin NA
  retornos_empresa <- retornos_empresa[start_index:nrow(retornos_empresa), ]
  return(retornos_empresa)
}

# Función principal para procesar una empresa con modelo skew-t
procesar_empresa_skew_t <- function(empresa_col) {
  empresa_name <- gsub("_Return$", "", empresa_col)
  
  # Extraer los retornos de la empresa con la columna de fecha
  retornos_empresa <- final_df_filtrado[, c("Date", empresa_col)]
  colnames(retornos_empresa) <- c("Date", "Retorno")
  
  # Ajustar la serie eliminando los NAs de la columna de retorno
  retornos_empresa <- ajustar_serie2(retornos_empresa)
  
  # Verificar si quedan suficientes datos
  if (nrow(retornos_empresa) < 150 || all(is.na(retornos_empresa$Retorno))) {
    cat("Insuficientes datos para la empresa:", empresa_name, "\n")
    return(NULL)  # Saltar esta empresa si no tiene suficientes datos
  }
  
  # Extraer las variables de mercado y las últimas seis variables
  exp_vars <- final_df_filtrado[, c("Date", "Rm_Rf", "Rm_Rf2", "Rm_Rf3", "Exc_Rate", "Brent", "Covid", "Soy", "Passangers", "VIX")]
  
  # Realizar el merge entre los datos de la empresa y las variables explicativas
  empresa_data <- merge(exp_vars, retornos_empresa, by = "Date", all.x = TRUE)
  empresa_data <- na.omit(empresa_data)
  regress1 <- tryCatch(selm(Retorno ~ Rm_Rf, data = empresa_data, family = "ST"),
                       error = function(e) NULL)
  regress2 <- tryCatch(selm(Retorno ~ Rm_Rf + Rm_Rf2 + Rm_Rf3, data = empresa_data, family = "ST"),
                       error = function(e) NULL)
  regress3 <- tryCatch(selm(Retorno ~ Rm_Rf + Rm_Rf2 + Rm_Rf3 + Exc_Rate + 
                              Brent +  Soy + Passangers + VIX, data = empresa_data, family = "ST"),
                       error = function(e) NULL)

  # Verificar si los modelos son nulos
  if (is.null(regress1) || is.null(regress2) || is.null(regress3)) {
    cat("Error en uno o más modelos para la empresa:", empresa_name, "\n")
    return(NULL)
  }
  
  # Resumen y métricas para cada modelo
  sumreg1 <- summary(regress1)
  sumreg2 <- summary(regress2)
  sumreg3 <- summary(regress3)
  
  aic_1 <- AIC(regress1)
  aic_2 <- AIC(regress2)
  aic_3 <- AIC(regress3)
  
  bic_1 <- BIC(regress1)
  bic_2 <- BIC(regress2)
  bic_3 <- BIC(regress3)
  
  mse_1 <- mean(resid(regress1)^2, na.rm = TRUE)
  mse_2 <- mean(resid(regress2)^2, na.rm = TRUE)
  mse_3 <- mean(resid(regress3)^2, na.rm = TRUE)
  
  n_observaciones <- nrow(empresa_data)
  
  # Almacenar resultados
  list(
    coeficientes = list(
      modelo1 = coef(regress1),
      modelo2 = coef(regress2),
      modelo3 = coef(regress3)
    ),
    aic = list(
      AIC_1 = aic_1,
      AIC_2 = aic_2,
      AIC_3 = aic_3
    ),
    bic = list(
      BIC_1 = bic_1,
      BIC_2 = bic_2,
      BIC_3 = bic_3
    ),
    mse = list(
      MSE_1 = mse_1,
      MSE_2 = mse_2,
      MSE_3 = mse_3
    ),
    n_obs = n_observaciones
  )
}

# Ciclo for para iterar sobre las empresas
resultados_empresas <- list()  # Lista para almacenar resultados

empresa_cols <- grep("_Return$", colnames(final_df_filtrado), value = TRUE)

for (empresa in empresa_cols) {
  cat("Procesando empresa:", empresa, "\n")
  resultado <- procesar_empresa_skew_t(empresa)
  
  if (!is.null(resultado)) {
    resultados_empresas[[empresa]] <- resultado
  }
}


exportar_a_excel_skewt <- function(resultados_empresas, archivo_salida) {
  
  # Crear un data frame inicial vacío
  datos_a_exportar <- data.frame(
    Empresa = character(),
    Alpha_Modelo1 = numeric(),
    Beta_Modelo1 = numeric(),
    AIC_Modelo1 = numeric(),
    BIC_Modelo1 = numeric(),
    MSE_Modelo1 = numeric(),
    Alpha_Modelo2 = numeric(),
    Beta_Modelo2 = numeric(),
    AIC_Modelo2 = numeric(),
    BIC_Modelo2 = numeric(),
    MSE_Modelo2 = numeric(),
    Alpha_Modelo3 = numeric(),
    Beta_Modelo3 = numeric(),
    AIC_Modelo3 = numeric(),
    BIC_Modelo3 = numeric(),
    MSE_Modelo3 = numeric(),
    N_Obs = numeric(),
    stringsAsFactors = FALSE
  )
  # Obtener los nombres de las empresas para asignar a cada resultado
  nombres_empresas <- colnames(final_df)[grepl("_Return$", colnames(final_df))]
  # Iterar sobre cada índice en los resultados
  for (i in seq_along(resultados_empresas)) {
    resultado <- resultados_empresas[[i]]
    
    # Verificar si 'resultado' es válido
    if (!is.null(resultado) && !any(is.na(resultado))) {
      coefs_modelo1 <- resultado$coeficientes$modelo1
      coefs_modelo2 <- if (!is.null(resultado$coeficientes$modelo2)) resultado$coeficientes$modelo2 else NA
      coefs_modelo3 <- resultado$coeficientes$modelo3
      aic <- resultado$aic
      bic <- resultado$bic
      mse <- resultado$mse
      # Crear una fila para la empresa
      fila_empresa <- data.frame(
        Empresa = nombres_empresas[i],
        
        # Modelo 1
        Alpha_Modelo1 = if (!is.null(coefs_modelo1) && "(Intercept.CP)" %in% names(coefs_modelo1)) coefs_modelo1["(Intercept.CP)"] else NA,
        Beta_Modelo1 = if (!is.null(coefs_modelo1) && "Rm_Rf" %in% names(coefs_modelo1)) coefs_modelo1["Rm_Rf"] else NA,
        AIC_Modelo1 = if (!is.null(aic) && length(aic) >= 1) aic[1] else NA,
        BIC_Modelo1 = if (!is.null(bic) && length(bic) >= 1) bic[1] else NA,
        MSE_Modelo1 = if (!is.null(mse) && length(mse) >= 1) mse[1] else NA,
        
        # Modelo 2
        Alpha_Modelo2 = if (!is.null(coefs_modelo2) && "(Intercept.CP)" %in% names(coefs_modelo2)) coefs_modelo2["(Intercept.CP)"] else NA,
        Beta_Modelo2 = if (!is.null(coefs_modelo2) && "Rm_Rf" %in% names(coefs_modelo2)) coefs_modelo2["Rm_Rf"] else NA,
        Gamma_Modelo2 = if (!is.null(coefs_modelo2) && "Rm_Rf2" %in% names(coefs_modelo2)) coefs_modelo2["Rm_Rf2"] else NA,
        Delta_Modelo2 = if (!is.null(coefs_modelo2) && "Rm_Rf3" %in% names(coefs_modelo2)) coefs_modelo2["Rm_Rf3"] else NA,
        AIC_Modelo2 = if (!is.null(aic) && length(aic) >= 2) aic[2] else NA,
        BIC_Modelo2 = if (!is.null(bic) && length(bic) >= 2) bic[2] else NA,
        MSE_Modelo2 = if (!is.null(mse) && length(mse) >= 2) mse[2] else NA,
        
        # Modelo 3
        Alpha_Modelo3 = if (!is.null(coefs_modelo3) && "(Intercept.CP)" %in% names(coefs_modelo3)) coefs_modelo3["(Intercept.CP)"] else NA,
        Beta_Modelo3 = if (!is.null(coefs_modelo3) && "Rm_Rf" %in% names(coefs_modelo3)) coefs_modelo3["Rm_Rf"] else NA,
        Gamma_Modelo3 = if (!is.null(coefs_modelo3) && "Rm_Rf2" %in% names(coefs_modelo3)) coefs_modelo3["Rm_Rf2"] else NA,
        Delta_Modelo3 = if (!is.null(coefs_modelo3) && "Rm_Rf3" %in% names(coefs_modelo3)) coefs_modelo3["Rm_Rf3"] else NA,
        Exc_Rate = if (!is.null(coefs_modelo3) && "Exc_Rate" %in% names(coefs_modelo3)) coefs_modelo3["Exc_Rate"] else NA,
        Brent = if (!is.null(coefs_modelo3) && "Brent" %in% names(coefs_modelo3)) coefs_modelo3["Brent"] else NA,
        #Covid = if (!is.null(coefs_modelo3) && "Covid" %in% names(coefs_modelo3)) coefs_modelo3["Covid"] else NA,
        Soy = if (!is.null(coefs_modelo3) && "Soy" %in% names(coefs_modelo3)) coefs_modelo3["Soy"] else NA,
        Passangers = if (!is.null(coefs_modelo3) && "Passangers" %in% names(coefs_modelo3)) coefs_modelo3["Passangers"] else NA,
        VIX = if (!is.null(coefs_modelo3) && "VIX" %in% names(coefs_modelo3)) coefs_modelo3["VIX"] else NA,
        AIC_Modelo3 = if (!is.null(aic) && length(aic) >= 3) aic[3] else NA,
        BIC_Modelo3 = if (!is.null(bic) && length(bic) >= 3) bic[3] else NA,
        MSE_Modelo3 = if (!is.null(mse) && length(mse) >= 3) mse[3] else NA,
        
        # Número de observaciones
        N_Obs = if (!is.null(resultado$n_obs)) resultado$n_obs else NA,
        
        stringsAsFactors = FALSE
      )
      
      # Agregar la fila al data frame de exportación
      datos_a_exportar <- rbind(datos_a_exportar, fila_empresa)
    }
  }
  
  # Crear un workbook de openxlsx y agregar una hoja
  wb <- createWorkbook()
  addWorksheet(wb, "Resultados Skew-t")
  
  # Escribir los datos en la hoja
  writeData(wb, "Resultados Skew-t", datos_a_exportar)
  
  # Guardar el archivo Excel
  saveWorkbook(wb, archivo_salida, overwrite = TRUE)
  
  cat("Archivo exportado con éxito:", archivo_salida, "\n")
}

# Extraer el año de las fechas de inicio y término
start_year <- format(as.Date(start_date), "%Y")
end_year <- format(as.Date(end_date), "%Y")

# Crear el nombre del archivo con las fechas incluidas
archivo_ST <- paste0("results_ST_cv_", start_year, "_to_", end_year, ".xlsx")

# Llamada a la función para exportar a Excel
exportar_a_excel_skewt(resultados_empresas, archivo_ST)

