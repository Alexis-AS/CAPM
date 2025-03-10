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
#install.packages("pbapply")
#install.packages("here")
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
final_df <- final_df %>%
  left_join(database_cv, by = "Date")
# Filtrar final_df para que incluya solo las filas dentro del rango de fechas
final_df_filtrado <- final_df %>%
  filter(Date >= fecha_inicial & Date <= fecha_final)
# Función para calcular S_hat
calculate_S_hat <- function(data, n) {
  m <- length(data)
  S_hat <- 0
  for (i in 1:(n-1)) {
    S_hat <- S_hat + abs(sum(data[(floor(m*i/n)+1):floor(m*(i+1)/n)] - mean(data))) *
      abs(sum(data[(floor(m*(i-1)/n)+1):floor(m*i/n)] - mean(data)))
  }
  S_hat <- S_hat / sum(sapply(1:n, function(i) abs(sum(data[(floor(m*(i-1)/n)+1):floor(m*i/n)] - mean(data)))^2))
  return(S_hat)
}

# Función para realizar el test con un valor de n dado
perform_test <- function(data, n, q = 0.05) {
  S_hat_m_n <- calculate_S_hat(data, n)
  z_q <- qnorm(1 - q/2)
  sigma2_pi <- 1 + 4/pi - 20/(pi^2)
  C_n_m <- abs(S_hat_m_n - 2/pi) > z_q * sqrt(sigma2_pi / n)
  return(C_n_m)
}

ajustar_serie2 <- function(retornos_empresa) {
  # Buscar el índice de la primera fila donde el retorno no es NA
  start_index <- which(!is.na(retornos_empresa$Retorno))[1]
  
  # Filtrar los datos desde la primera fila sin NA
  retornos_empresa <- retornos_empresa[start_index:nrow(retornos_empresa), ]
  
  return(retornos_empresa)
}

# Función para procesar cada empresa con manejo de errores
procesar_empresa_ols <- function(empresa_col) {
  tryCatch({
    empresa_name <- gsub("_Return$", "", empresa_col)
    
    # Extraer los retornos de la empresa con la columna de fecha
    retornos_empresa <- final_df_filtrado[, c("Date", empresa_col)]
    colnames(retornos_empresa) <- c("Date", "Retorno")
    
    # Ajustar la serie eliminando los NAs de la columna de retorno
    retornos_empresa <- ajustar_serie2(retornos_empresa)
    
    # Verificar si quedan suficientes datos
    if (nrow(retornos_empresa) < 150 || all(is.na(retornos_empresa$Retorno))) {
      message("Datos insuficientes para la empresa: ", empresa_name)
      return(NULL)  # Saltar esta empresa
    }
    
    values <- tryCatch(floor(sqrt(nrow(retornos_empresa))), 
                       error = function(e) {
                         message("Error al calcular 'values' para la empresa: ", empresa_name, ": ", e$message)
                         return(NULL)  # Saltar esta empresa si no se puede calcular
                       })
    if (is.null(values)) return(NULL)  # Saltar si hubo error en 'values'
    
    # Extraer las variables de mercado y las últimas seis variables
    exp_vars <- final_df_filtrado[, c("Date", "Rm_Rf", "Rm_Rf2", "Rm_Rf3", "Exc_Rate", "Brent", "Soy", "Passangers", "VIX")]
    
    # Realizar el merge entre los datos de la empresa y las variables explicativas
    empresa_data <- merge(exp_vars, retornos_empresa, by = "Date", all.x = TRUE)
    
    regress1 <- tryCatch(lm(Retorno ~ Rm_Rf, data = empresa_data), error = function(e) NULL)
    regress2 <- tryCatch(lm(Retorno ~ Rm_Rf + Rm_Rf2 + Rm_Rf3, data = empresa_data), error = function(e) NULL)
    regress3 <- tryCatch(lm(Retorno ~ Rm_Rf + Rm_Rf2 + Rm_Rf3 + Exc_Rate +
                              Brent + Soy + Passangers + VIX, 
                            data = empresa_data), error = function(e) NULL)
    
    # Verificar si los modelos son nulos
    if (is.null(regress1) || is.null(regress2) || is.null(regress3)) {
      message("Error en modelo para la empresa: ", empresa_name)
      return(NULL)
    }
    
    # Calcular métricas
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
    
    # Prueba de colas pesadas
    heavy_tails_res1 <- sapply(values, function(n) perform_test(resid(regress1), n))
    heavy_tails_res2 <- sapply(values, function(n) perform_test(resid(regress2), n))
    heavy_tails_res3 <- sapply(values, function(n) perform_test(resid(regress3), n))
    
    # **Pruebas de normalidad en los residuos**
    normality_tests <- function(residuals) {
      list(
        jarque_bera = tryCatch(jarque.bera.test(residuals)$p.value, error = function(e) NA),
        shapiro_wilk = tryCatch(shapiro.test(residuals)$p.value, error = function(e) NA),
        shapiro_francia = tryCatch(sf.test(residuals)$p.value, error = function(e) NA)
      )
    }
    
    normality_res1 <- normality_tests(resid(regress1))
    normality_res2 <- normality_tests(resid(regress2))
    normality_res3 <- normality_tests(resid(regress3))
    
    # Almacenar resultados
    list(
      coeficientes = list(
        modelo1 = sumreg1$coefficients,
        modelo2 = sumreg2$coefficients,
        modelo3 = sumreg3$coefficients
      ),
      r_squared = list(
        modelo1 = sumreg1$r.squared,
        modelo2 = sumreg2$r.squared,
        modelo3 = sumreg3$r.squared,
        AIC_1 = aic_1,
        AIC_2 = aic_2,
        AIC_3 = aic_3,
        BIC_1 = bic_1,
        BIC_2 = bic_2,
        BIC_3 = bic_3,
        MSE_1 = mse_1,
        MSE_2 = mse_2,
        MSE_3 = mse_3,
        n_obs = n_observaciones
      ),
      heavy_tails_tests = list(
        heavy_tails_res1 = heavy_tails_res1,
        heavy_tails_res2 = heavy_tails_res2,
        heavy_tails_res3 = heavy_tails_res3
      ),
      normality_tests = list(
        modelo1 = normality_res1,
        modelo2 = normality_res2,
        modelo3 = normality_res3
      ),
      resids = list(
        modelo1 = resid(regress1),
        modelo2 = resid(regress2),
        modelo3 = resid(regress3)
      )
    )
  }, error = function(e) {
    message("Error procesando la empresa: ", empresa_col, " - ", e$message)
    return(NULL)
  })
}

# Paralelización
num_cores <- detectCores() - 1  # Usa todos los núcleos menos uno
cl <- makeCluster(num_cores)
empresa_cols <- grep("_Return$", colnames(final_df_filtrado), value = TRUE)

clusterExport(cl, list("final_df_filtrado", "ajustar_serie2", "procesar_empresa_ols", "perform_test", 
                       "calculate_S_hat", "jarque.bera.test", "shapiro.test", "sf.test"))
pboptions(type = "timer")

resultados_empresas <- pblapply(empresa_cols, procesar_empresa_ols, cl = cl)
stopCluster(cl)


# Exportar a Excel

exportar_a_excel_openxlsx <- function(resultados_empresas, archivo_salida) {
  
  # Crear un data frame inicial vacío
  datos_a_exportar <- data.frame(
    Empresa = character(),
    Modelo1 = numeric(),
    R_Squared_Modelo1 = numeric(),
    AIC_Modelo1 = numeric(),
    BIC_Modelo1 = numeric(),
    MSE_Modelo1 = numeric(),
    Modelo2 = numeric(),
    R_Squared_Modelo2 = numeric(),
    AIC_Modelo2 = numeric(),
    BIC_Modelo2 = numeric(),
    MSE_Modelo2 = numeric(),
    Modelo3 = numeric(),
    R_Squared_Modelo3 = numeric(),
    AIC_Modelo3 = numeric(),
    BIC_Modelo3 = numeric(),
    MSE_Modelo3 = numeric(),
    Heavy_Tails_Res1 = logical(),
    Heavy_Tails_Res2 = logical(),
    Heavy_Tails_Res3 = logical(),
    JB = logical(),
    SW = logical(),
    SF = logical(),
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
      r2 <- resultado$r_squared
      heavy_tails_tests <- resultado$heavy_tails_tests
      normality_tests <- resultado$normality_tests
      
      # Crear una fila para la empresa
      fila_empresa <- data.frame(
        Empresa = nombres_empresas[i],
        Alpha_Modelo1 = coefs_modelo1["(Intercept)", "Estimate"],
        pvalue_Alpha_M1 = coefs_modelo1["(Intercept)", "Pr(>|t|)"],
        Beta_Modelo1 = coefs_modelo1["Rm_Rf", "Estimate"],
        pvalue_Beta_M1 = coefs_modelo1["Rm_Rf", "Pr(>|t|)"],
        R_Squared_Modelo1 = r2[1],
        AIC_Modelo1 = r2[4],
        BIC_Modelo1 = r2[7],
        MSE_Modelo1 = r2[10],
        Alpha_Modelo2 = if (!is.null(coefs_modelo2)) coefs_modelo2["(Intercept)", "Estimate"] else NA,
        pvalue_Alpha_M2 = if (!is.null(coefs_modelo2)) coefs_modelo2["(Intercept)", "Pr(>|t|)"] else NA,
        Beta_Modelo2 = if (!is.null(coefs_modelo2)) coefs_modelo2["Rm_Rf", "Estimate"] else NA,
        pvalue_Beta_M2 = if (!is.null(coefs_modelo2)) coefs_modelo2["Rm_Rf", "Pr(>|t|)"] else NA,
        Gamma_Modelo2 = if (!is.null(coefs_modelo2)) coefs_modelo2["Rm_Rf2", "Estimate"] else NA,
        pvalue_Gamma_M2 = if (!is.null(coefs_modelo2)) coefs_modelo2["Rm_Rf2", "Pr(>|t|)"] else NA,
        Delta_Modelo2 = if (!is.null(coefs_modelo2)) coefs_modelo2["Rm_Rf3", "Estimate"] else NA,
        pvalue_Delta_M2 = if (!is.null(coefs_modelo2)) coefs_modelo2["Rm_Rf3", "Pr(>|t|)"] else NA,
        R_Squared_Modelo2 = r2[2],
        AIC_Modelo2 = r2[5],
        BIC_Modelo2 = r2[8],
        MSE_Modelo2 = r2[11],
        Alpha_Modelo3 = if ("(Intercept)" %in% rownames(coefs_modelo3)) coefs_modelo3["(Intercept)", "Estimate"] else NA,
        pvalue_Alpha_M3 = if ("(Intercept)" %in% rownames(coefs_modelo3)) coefs_modelo3["(Intercept)", "Pr(>|t|)"] else NA,
        Beta_Modelo3 = if ("Rm_Rf" %in% rownames(coefs_modelo3)) coefs_modelo3["Rm_Rf", "Estimate"] else NA,
        pvalue_Beta_M3 = if ("Rm_Rf" %in% rownames(coefs_modelo3)) coefs_modelo3["Rm_Rf",  "Pr(>|t|)"] else NA,
        Gamma_Modelo3 = if ("Rm_Rf2" %in% rownames(coefs_modelo3)) coefs_modelo3["Rm_Rf2", "Estimate"] else NA,
        pvalue_Gamma_M3 = if ("Rm_Rf2" %in% rownames(coefs_modelo3)) coefs_modelo3["Rm_Rf2",  "Pr(>|t|)"] else NA,
        Delta_Modelo3 = if ("Rm_Rf3" %in% rownames(coefs_modelo3)) coefs_modelo3["Rm_Rf3", "Estimate"] else NA,
        pvalue_Delta_M3 = if ("Rm_Rf3" %in% rownames(coefs_modelo3)) coefs_modelo3["Rm_Rf3",  "Pr(>|t|)"] else NA,
        ExcRate_Modelo3 = if ("Exc_Rate" %in% rownames(coefs_modelo3)) coefs_modelo3["Exc_Rate", "Estimate"] else NA,
        pvalue_ExcRate_M3 = if ("Exc_Rate" %in% rownames(coefs_modelo3)) coefs_modelo3["Exc_Rate",  "Pr(>|t|)"] else NA,
        #Covid_Modelo3 = if ("Covid" %in% rownames(coefs_modelo3)) coefs_modelo3["Covid", "Estimate"] else NA,
        pvalue_Covid_M3 = if ("Covid" %in% rownames(coefs_modelo3)) coefs_modelo3["Covid", "Pr(>|t|)"] else NA,
        Brent_Modelo3 = if ("Brent" %in% rownames(coefs_modelo3)) coefs_modelo3["Brent", "Estimate"] else NA,
        pvalue_Brent_M3 = if ("Brent" %in% rownames(coefs_modelo3)) coefs_modelo3["Brent",  "Pr(>|t|)"] else NA,
        Soy_Modelo3 = if ("Soy" %in% rownames(coefs_modelo3)) coefs_modelo3["Soy", "Estimate"] else NA,
        pvalue_Soy_M3 = if ("Soy" %in% rownames(coefs_modelo3)) coefs_modelo3["Soy",  "Pr(>|t|)"] else NA,
        Passangers_Modelo3 = if ("Passangers" %in% rownames(coefs_modelo3)) coefs_modelo3["Passangers", "Estimate"] else NA,
        pvalue_Passangers_M3 = if ("Passangers" %in% rownames(coefs_modelo3)) coefs_modelo3["Passangers",  "Pr(>|t|)"] else NA,
        VIX_Modelo3 = if ("VIX" %in% rownames(coefs_modelo3)) coefs_modelo3["VIX", "Estimate"] else NA,
        pvalue_VIX_M3 = if ("VIX" %in% rownames(coefs_modelo3)) coefs_modelo3["VIX",  "Pr(>|t|)"] else NA,
        R_Squared_Modelo3 = r2[3],
        AIC_Modelo3 = r2[6],
        BIC_Modelo3 = r2[9],
        MSE_Modelo3 = r2[12],
        Heavy_Tails_Res1 = heavy_tails_tests[1],
        Heavy_Tails_Res2 = heavy_tails_tests[2],
        Heavy_Tails_Res3 = heavy_tails_tests[3],
        JB = normality_tests[1],
        SW = normality_tests[2],
        SF = normality_tests[3],
        N_Obs = r2[13],
        stringsAsFactors = FALSE
      )
      
      # Agregar la fila al data frame de exportación
      datos_a_exportar <- rbind(datos_a_exportar, fila_empresa)
    }
  }
  
  # Crear un workbook de openxlsx y agregar una hoja
  wb <- createWorkbook()
  addWorksheet(wb, "Resultados OLS")
  
  # Escribir los datos en la hoja
  writeData(wb, "Resultados OLS", datos_a_exportar)
  
  # Guardar el archivo Excel
  saveWorkbook(wb, archivo_salida, overwrite = TRUE)
  
  cat("Archivo exportado con éxito:", archivo_salida, "\n")
}
# Extraer el año de las fechas de inicio y término
start_year <- format(as.Date(start_date), "%Y")
end_year <- format(as.Date(end_date), "%Y")

# Crear el nombre del archivo con las fechas incluidas
archivo_OLS <- paste0("results_OLS_cv_", start_year, "_to_", end_year, ".xlsx")

# Llamada a la función para exportar a Excel, con el nuevo nombre
exportar_a_excel_openxlsx(resultados_empresas, archivo_OLS)

# Procesamiento solo del primer test de colas pesadas
heavy_tails_results <- list()

# Obtención de las columnas de retornos de empresas
empresa_cols <- grep("_Return$", names(final_df_filtrado), value = TRUE)
empresa_cols <- empresa_cols[!empresa_cols %in% c("Rm_Rf", "Risk_Free_Rate", "Rm_Rf2", "Rm_Rf3", "Exc_Rate", "Covid", "Brent", "Soy", "Passangers", "VIX")]

for (empresa_col in empresa_cols) {
  empresa_name <- gsub("_Return$", "", empresa_col)
  empresa_start_date <- min(final_df_filtrado$Date[!is.na(final_df_filtrado[[empresa_col]])])
  empresa_data <- final_df_filtrado %>% filter(Date >= empresa_start_date)
  
  # Filtrar datos desde el primer valor no NA de la empresa
  empresa_data <- final_df_filtrado %>% filter(!is.na(.[[empresa_col]]) & !is.na(.[["Rm_Rf"]]))
  
  if (nrow(empresa_data) < 2 || all(is.na(empresa_data[[empresa_col]]))) {
    next  # Saltar esta empresa si no tiene suficientes datos
  }
  values <- floor(4/5*nrow(empresa_data))
  # Test de colas pesadas (solo el test original)
  heavy_tails <- sapply(values, function(n) perform_test(empresa_data[[empresa_col]], n))
  heavy_tails_results[[empresa_name]] <- heavy_tails
}

# Crear un Excel con solo los resultados del primer test de colas pesadas
wb_heavy_tails <- createWorkbook()
list_to_dataframe <- function(input_list) {
  df <- do.call(rbind, lapply(input_list, function(x) data.frame(t(x), stringsAsFactors = FALSE)))
  df$Empresa <- rownames(df)
  rownames(df) <- NULL
  return(df)
}
# Convertir la lista a dataframe
heavy_tails_df <- list_to_dataframe(heavy_tails_results)

# Añadir los resultados al workbook
addWorksheet(wb_heavy_tails, "Heavy Tails")
writeData(wb_heavy_tails, "Heavy Tails", heavy_tails_df, na.string = "")

# Extraer el año de las fechas de inicio y término
start_year <- format(as.Date(start_date), "%Y")
end_year <- format(as.Date(end_date), "%Y")

# Crear el nombre del archivo de exportación con solo los años
archivo_OLS <- paste0("results_OLS_ht_cv", start_year, "_to_", end_year, ".xlsx")

# Guardar el workbook con el nuevo nombre
saveWorkbook(wb_heavy_tails, archivo_OLS, overwrite = TRUE)

generar_histogramas_residuos <- function(resultados_empresas, carpeta_histogramas) {
  
  for (empresa in seq_along(resultados_empresas)) {
    resultado <- resultados_empresas[[empresa]]
    
    # Extraer el vector de residuos
    resids <- resultado$resids[[1]]  # Acceder directamente al vector numérico de residuos
    
    # Crear el dataframe con los residuos
    resids_df <- data.frame(Residuos = resids)
    
    # Calcular la media y desviación estándar de los residuos
    media <- mean(resids, na.rm = TRUE)
    sd_resid <- sd(resids, na.rm = TRUE)
    
    # Crear la secuencia de valores para la curva de densidad normal
    x_seq <- seq(min(resids), max(resids), length.out = 100)
    densidad_normal <- dnorm(x_seq, mean = media, sd = sd_resid)
    
    # Crear el histograma y añadir la curva de densidad normal
    plot_hist <- ggplot(resids_df, aes(x = Residuos)) +
      geom_histogram(bins = 100, fill = "blue", color = "white", alpha = 1) +
      geom_line(data = data.frame(x_seq, densidad_normal), 
                aes(x = x_seq, y = densidad_normal * length(resids) * diff(range(resids)) / 100), 
                color = "green", linewidth = 1) +
      labs(title = paste("Histograma de Residuos para Empresa", empresa),
           x = "Residuos", y = "Frecuencia") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white", color = "black"))
    
    # Guardar el histograma como imagen PNG
    ruta_guardado <- file.path(carpeta_histogramas, paste0("histograma_residuos_", empresa, ".png"))
    ggsave(ruta_guardado, plot = plot_hist, width = 8, height = 6)
  }
  cat("¡Histogramas guardados correctamente en la carpeta!\n")
}
# Crear el nombre de la carpeta de forma dinámica
nombre_carpeta <- paste0("carpeta_histogramas_OLS_cv_", start_year, "_", end_year)

# Llamada a la función con el nombre de la carpeta generado dinámicamente
generar_histogramas_residuos(resultados_empresas, nombre_carpeta)