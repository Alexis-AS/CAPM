# Instalación de paquetes para la importación
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
#setwd("C:/Users/alexi/OneDrive - Universidad Técnica Federico Santa María/2025/Paper CAPM/Simulaciones")
setwd("C:/Users/usuario/OneDrive - Universidad Técnica Federico Santa María/2025/Paper CAPM/Simulaciones")
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
simulate_returns_to_csv <- function(n_obs, start_date, alpha_values, beta_values, num_reps) {
  # Validación de inputs
  if (any(alpha_values <= 0 | alpha_values > 2)) stop("Alpha values must be in the range (0, 2].")
  if (any(beta_values < -1 | beta_values > 1)) stop("Beta values must be in the range [-1, 1].")
  if (num_reps <= 0) stop("Number of repetitions must be greater than 0.")
  
  # Crear el marco de datos para fechas
  dates <- seq.Date(from = as.Date(start_date), length.out = n_obs, by = "day")
  
  # Inicializar data.frame para compilar todas las combinaciones de parámetros
  parameters_list <- data.frame(Alpha = numeric(),
                                Beta = numeric(),
                                Gamma = numeric(),
                                Delta = numeric(),
                                stringsAsFactors = FALSE)
  
  # Iterar sobre valores de alpha y beta
  for (alpha in alpha_values) {
    for (beta in beta_values) {
      # Inicializar data.frame para esta combinación
      returns_df <- data.frame(Date = dates)
      
      # Generar todas las repeticiones
      for (rep in 1:num_reps) {
        # Generar un gamma aleatorio para cada repetición
        gamma <- 1
        delta <- 0  # Delta fijo en 0
        
        # Generar datos simulados de una distribución estable
        simulated_returns <- rstable(n_obs, alpha, beta, gamma, delta)
        
        # Agregar los retornos simulados como una columna nueva
        col_name <- paste0("Rep_", rep)
        returns_df[[col_name]] <- simulated_returns
        
        # Solo agregar una combinación única de parámetros a la lista
        if (rep == 1) { 
          parameters_list <- rbind(parameters_list, 
                                   data.frame(Alpha = alpha, 
                                              Beta = beta, 
                                              Gamma = gamma, 
                                              Delta = delta))
        }
      }
      
      # Generar nombre de archivo para esta combinación
      alpha_str <- gsub("\\.", "coma", as.character(alpha))  # Reemplazar "." con "coma"
      beta_str <- gsub("-", "Neg", gsub("\\.", "coma", as.character(beta)))  # Reemplazar "-" con "Neg" y "." con "coma"
      file_name <- paste0("Alpha_", alpha_str, "_Beta_", beta_str, ".csv")
      
      # Guardar el archivo CSV para esta combinación
      write.csv(returns_df, file = file_name, row.names = FALSE)
    }
  }
  
  # Guardar el archivo compilado de parámetros únicos
  write.csv(parameters_list, file = "Simulation_Parameters_Compiled.csv", row.names = FALSE)
}

# Valores predefinidos de alpha, beta y número de repeticiones
alpha_values <- c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.75, 1.8, 1.85, 1.9, 1.95, 2)
beta_values <- c(-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
num_reps <- 50  # Número de repeticiones por combinación

# Simular y guardar los datos en archivos CSV
simulate_returns_to_csv(n_obs = 1500, 
                        start_date = "2017-10-01", 
                        alpha_values = alpha_values, 
                        beta_values = beta_values, 
                        num_reps = num_reps)
