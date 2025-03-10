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
#setwd("C:/Users/alexi/OneDrive - Universidad Técnica Federico Santa María/2025/Paper CAPM/Bases de Datos")
setwd("C:/Users/usuario/OneDrive - Universidad Técnica Federico Santa María/2025/Paper CAPM/Bases de Datos")

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

#Horizonte de trabajo
start_date <- "2017-10-01"
end_date <- "2024-07-01"
fecha_inicial <- as.Date(start_date)
fecha_final <- as.Date(end_date)
fechas_completas <- data.frame(Date = seq(fecha_inicial, fecha_final, by = "day"))

#Importar la tasa libre de riesgo (Money Market Rate) desde un archivo CSV
mmr_data <- read.csv("MMR.csv")
mmr_data$Date <- as.Date(mmr_data$Date, format = "%Y-%m-%d")
colnames(mmr_data)[which(names(mmr_data) == "Rate")] <- "Value"

#Filtrar y procesar la tasa libre de riesgo
risk_free_rate <- function(tasa, fecha_inicial, fecha_final) {
  tasa <- tasa %>%
    filter(Date >= fecha_inicial & Date <= fecha_final)
  tasa_diaria <- fechas_completas
  tasa_diaria <- tasa_diaria %>%
    left_join(tasa, by = "Date") %>%
    select(Date, Value)
  tasa_diaria <- tasa_diaria %>%
    mutate(Value = zoo::na.locf(Value, na.rm = FALSE))
  tasa_diaria$Value <- as.numeric(as.character(tasa_diaria$Value))
  tasa_diaria$Dias_Del_Mes <- lubridate::days_in_month(tasa_diaria$Date)
  tasa_diaria <- tasa_diaria %>%
    mutate(Tasa_Diaria = (1 + (Value / 100))^(1 / Dias_Del_Mes) - 1)
  tasa_diaria <- tasa_diaria %>% select(-Dias_Del_Mes, -Value)
  return(tasa_diaria)
}
tasa_diaria <- risk_free_rate(mmr_data, fecha_inicial, fecha_final)

#Importar datos de empresas desde Yahoo Finance, Tratamiento
#Tickers
tickers_info <- read.csv("B3C.csv")
tickers <- tickers_info[[8]] %>% as.character() %>% str_split(pattern = ",") %>% unlist()

import_yahoo_data <- function(tickers, start_date, end_date) {
  price_data_list <- list()
  dividend_data_list <- list()
  for (ticker in tickers) {
    tryCatch({
      getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
      data <- get(ticker)
      data <- Cl(data)
      var_name <- ticker
      assign(var_name, data, envir = .GlobalEnv)
      price_data_list[[var_name]] <- data
      dividends <- getDividends(ticker, from = start_date, to = end_date)
      div_name <- paste0(var_name, ".div")
      assign(div_name, dividends, envir = .GlobalEnv)
      dividend_data_list[[var_name]] <- dividends
    }, error = function(e) {
      message(paste("No se pudo importar", ticker, ":", e$message))
      NULL
    })
  }
  return(list(prices = price_data_list, dividends = dividend_data_list))
}

yahoo_data <- import_yahoo_data(tickers, start_date, end_date)
precios <- yahoo_data$prices
dividendos <- yahoo_data$dividends
convert_to_df <- function(data_list) {
  data_df_list <- list()
  for (ticker in names(data_list)) {
    data <- data_list[[ticker]]
    data_df <- data.frame(Date = index(data), coredata(data))
    data_df_list[[ticker]] <- data_df
  }
  return(data_df_list)
}
precios_df <- convert_to_df(precios)
dividendos_df <- convert_to_df(dividendos)

#Combinación de precios y dividendos

combine_data <- function(precios, dividendos, fechas_completas) {
  combined_df <- fechas_completas
  
  for (ticker in names(precios_df)) {
    price_data <- precios_df[[ticker]]
    combined_df <- combined_df %>%
      left_join(price_data, by = "Date")
  }
  
  for (ticker in names(dividendos_df)) {
    dividend_data <- dividendos_df[[ticker]]
    combined_df <- combined_df %>%
      left_join(dividend_data, by = "Date")
  }
  
  combined_df <- combined_df %>%
    mutate_all(~ replace_na(., 0))
  combined_df <- combined_df %>%
    filter(rowSums(select(., -Date) != 0) > 0)
  return(combined_df)
}

data_combined <- combine_data(precios_df, dividendos_df, fechas_completas)
variables_a_eliminar <- c(tickers, paste0(tickers, ".div"))
rm(list = variables_a_eliminar)
gc()
replace_zeros_data_combined <- function(data_combined) {
  data_combined[-1] <- lapply(data_combined[-1], function(col) {
    #Reemplazar ceros por NA
    col[col == 0] <- NA
    #Rellenar NA con el último valor válido anterior
    col <- zoo::na.locf(col, na.rm = FALSE)
    return(col)
  })
  return(data_combined)
}

#Aplicar la función a data_combined
data_combined <- replace_zeros_data_combined(data_combined)
#Tratamiento de datos de empresas
calculate_returns <- function(data_combined) {
  tickers <- unique(sub("\\.Close$", "", grep("\\.Close$", names(data_combined), value = TRUE)))
  
  for (ticker in tickers) {
    price_col <- paste0(ticker, ".Close")
    div_col <- paste0(ticker, ".div")
    
    if (div_col %in% names(data_combined)) {
      data_combined <- data_combined %>%
        mutate(!!paste0(ticker, "_Return") := ifelse(is.na(get(div_col)),
                                                     log((get(price_col) / lag(get(price_col)))),
                                                     log((get(price_col) + get(div_col)) / lag(get(price_col)))))
    } else {
      data_combined <- data_combined %>%
        mutate(!!paste0(ticker, "_Return") := log((get(price_col) / lag(get(price_col)))))
    }
  }
  
  data_combined <- data_combined %>%
    mutate(across(ends_with("_Return"), ~ ifelse(is.infinite(.) | is.nan(.), NA, .)))
  
  return(data_combined)
}
retornos <- calculate_returns(data_combined)

filter_returns <- function(data_combined) {
  return_cols <- grep("_Return$", names(data_combined), value = TRUE)
  return(data_combined %>%
           select(Date, all_of(return_cols)))
}
returns_df <- filter_returns(retornos)
returns_df <- returns_df[-1,]
group_returns_by_sector <- function(returns_df, tickers_info) {
  sector_list <- list()
  
  sectors <- unique(tickers_info[[3]])
  
  for (sector in sectors) {
    tickers_in_sector <- tickers_info %>%
      filter(NSector == sector) %>%
      pull(Code)
    
    return_cols <- paste0(tickers_in_sector, "_Return")
    return_cols <- return_cols[return_cols %in% names(returns_df)]
    
    sector_returns <- returns_df %>%
      select(Date, all_of(return_cols))
    
    sector_list[[sector]] <- sector_returns
  }
  
  return(sector_list)
}

sector_returns_list <- group_returns_by_sector(returns_df, tickers_info)
#Importar datos del mercado
import_market_data <- function(mtickers, start_date, end_date) {
  market_data_list <- list()
  for (ticker in mtickers) {
    data <- getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    var_name <- gsub("\\^", "caret_", ticker)
    data <- Cl(data) #Solo utilizar precios de cierre
    data <- data.frame(Date = index(data), coredata(data))
    names(data)[2] <- paste0(var_name, ".Close")
    data <- data %>%
      mutate_at(vars(ends_with(".Close")), ~ zoo::na.locf(., na.rm = FALSE)) #Conservar el último valor de cierre registrado
    market_data_list[[var_name]] <- data
  }
  return(market_data_list)
}

mtickers <- c("^BVSP")
market_data <- import_market_data(mtickers, start_date, end_date)
#Tratamiento de los datos del mercado
combine_index_with_dates <- function(index_data) {
  combined_index <- index_data %>%
    mutate_all(~ zoo::na.locf(., na.rm = FALSE)) #Reemplazar NA con el valor anterior registrado
  
  return(combined_index)
}

combined_market_data <- lapply(market_data, function(index_data) {
  combine_index_with_dates(index_data)
})
#Cálculo de retornos del mercado
calculate_returns <- function(data) {
  ticker_cols <- grep("\\.Close$", names(data), value = TRUE)
  for (col in ticker_cols) {
    return_col <- gsub("\\.Close$", "_Return", col)
    data <- data %>%
      mutate(!!return_col := log(get(col) / lag(get(col)))) %>%
      mutate(!!return_col := ifelse(is.infinite(get(return_col)) | is.nan(get(return_col)), NA, get(return_col))) %>%
      select(-all_of(col))
  }
  return(data)
}
returns_market_data <- lapply(combined_market_data, calculate_returns)
#Dataframes finales
combined_rf_market <- function(rf_data, market_data) {
  combined_data <- market_data %>%
    left_join(rf_data, by = "Date") %>%
    rename(Risk_Free_Rate = Tasa_Diaria) %>%
    mutate(Rm_Rf = caret_BVSP_Return - Risk_Free_Rate) %>%
    select(-caret_BVSP_Return, -Risk_Free_Rate)
  return(combined_data)
}

#Aplicar la función a cada dataset en returns_market_data
combined_rf_market_data <- lapply(returns_market_data, function(market_data) {
  combined_rf_market(tasa_diaria, market_data)
})
combined_rf_market_data <- as.data.frame(combined_rf_market_data)
colnames(combined_rf_market_data) <- gsub("caret_BVSP\\.", "", colnames(combined_rf_market_data))
colnames(combined_rf_market_data)
combined_rf_market_data <- combined_rf_market_data[-1,]
#Dataframe de datos de empresas
combined_returns_df <- returns_df %>%
  left_join(tasa_diaria, by = "Date")
adjusted_returns_df <- combined_returns_df %>%
  mutate(across(ends_with("_Return"), ~ . - Tasa_Diaria)) %>%
  select(-Tasa_Diaria)
#Combinación con los datos
final_df <- adjusted_returns_df %>%
  left_join(combined_rf_market_data, by = "Date")
# Identificar las columnas de empresas que terminan en "_Return"
empresas_cols <- final_df %>%
  select(ends_with("_Return"))

# Filtrar las columnas con más de 150 observaciones no NA
empresas_filtradas <- empresas_cols %>%
  select(where(~ sum(!is.na(.)) > 150))

# Crear un nuevo data frame final que conserve solo las columnas seleccionadas y la columna "Date"
final_df <- final_df %>%
  select(Date, all_of(names(empresas_filtradas)), Rm_Rf)

final_df <- final_df %>%
  mutate(
    Rm_Rf2 = (Rm_Rf)^2,  # Columna con (Rm - Rf)^2
    Rm_Rf3 = (Rm_Rf)^3     # Columna con (Rm - Rf)^3
  )
export_df <- final_df %>%
  select(Date, all_of(names(empresas_filtradas)), Rm_Rf)

# Crear un workbook y una hoja de trabajo
wb <- createWorkbook()
addWorksheet(wb, "Fechas_y_Datos")  # Agregar una hoja con un nombre adecuado

# Escribir los datos en la hoja de trabajo
writeData(wb, "Fechas_y_Datos", export_df)

# Guardar el archivo Excel
output_file <- "final_df.csv"  # Cambia el nombre del archivo si lo deseas
saveWorkbook(wb, output_file, overwrite = TRUE)
cat("Archivo exportado correctamente:", output_file, "\n")
