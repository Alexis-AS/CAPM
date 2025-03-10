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
#install.packages("pbapply")
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
start_date <- "2022-04-01"
end_date <- "2024-07-01"
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
final_df_filtrado <- final_df %>%
  filter(Date >= fecha_inicial & Date <= fecha_final)

trapani_finite_moment_test <- function(data, k, u = 1, psi = 1, verbose = FALSE) {
  # Test de Trapani desde el paquete finity con manejo de errores
  result <- tryCatch(
    finite_moment_test(
      obs = data, 
      k = k, 
      u = u, 
      psi = psi, 
      ignore_errors = TRUE,  # Maneja errores devolviendo NA y p-value = 1
      verbose = verbose
    ),
    error = function(e) {
      cat("Error en la prueba de Trapani:", conditionMessage(e), "\n")
      return(c(NA, NA))  # Retorna NA en caso de error
    }
  )
  
  return(result)
}

# Función para automatizar la prueba de Trapani para todas las empresas
automated_trapani_for_all <- function(data, k_start = 2, k_step = 0.05, u = 1, psi = 1, verbose = FALSE) {
  empresa_cols <- grep("_Return$", names(data), value = TRUE)
  parametros_iniciales <- data.frame(Empresa = character(), alpha = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
  
  for (empresa in empresa_cols) {
    current_k <- k_start
    rejected <- FALSE
    cat("Probando empresa:", empresa, "\n")
    
    while (!rejected && current_k > 0) {
      # Realizar la prueba de Trapani para el valor actual de k
      result <- trapani_finite_moment_test(data[[empresa]], k = current_k, u = u, psi = psi, verbose = verbose)
      theta_r <- result[1]
      p_value <- result[2]
      
      # Si el p-value indica que el momento es finito (rechazamos H0)
      if (!is.na(p_value) && p_value < 0.05) {
        rejected <- TRUE
        cat("Rechazo de H0 en k =", current_k, "para", empresa, "\n")
        parametros_iniciales <- rbind(parametros_iniciales, data.frame(Empresa = empresa, alpha = current_k, p_value = p_value))
      } else {
        # Disminuir k
        current_k <- current_k - k_step
      }
    }
    
    # Si no se rechazó H0 para ningún valor de k, asignar 0.1 como valor por defecto
    if (!rejected) {
      cat("No se rechazó H0 para ningún valor de k en la empresa:", empresa, "\n")
      parametros_iniciales <- rbind(parametros_iniciales, data.frame(Empresa = empresa, alpha = 0.1, p_value = 1))
    }
  }
  
  return(parametros_iniciales)
}

# Aplicar la prueba a todos los retornos de empresas
parametros_iniciales <- automated_trapani_for_all(final_df, k_start = 2, k_step = 0.05, u = 1, psi = 0.3)

# Función para calcular la mediana, los excesos negativos y positivos para cada empresa
calcular_excesos <- function(final_df) {
  empresas <- grep("_Return$", names(final_df), value = TRUE)  # Extraer los nombres de las empresas
  resultados <- list()  # Lista para almacenar los resultados
  
  for (empresa in empresas) {
    # Extraer los retornos de la empresa actual
    x <- final_df[[empresa]]
    
    # Paso 1: Calcular la mediana de los retornos de la empresa
    mediana_x <- median(x, na.rm = TRUE)
    # Paso 2: Inicializar los vectores de exceso negativo y positivo
    exceso_negativo <- rep(0, length(x))  # Vector de ceros del tamaño de x
    exceso_positivo <- rep(0, length(x))  # Vector de ceros del tamaño de x
    
    # Paso 3: Iterar sobre los valores de x
    for (i in 1:length(x)) {
      pre_DAM <- abs(x - mediana_x)
      if (!is.na(x[i])) {  # Asegurarse de que no sea NA
        if (x[i] < mediana_x) {
          exceso_negativo[i] <- mediana_x - x[i]  # Calcular el exceso negativo
        } else if (x[i] > mediana_x) {
          exceso_positivo[i] <- x[i] - mediana_x  # Calcular el exceso positivo
        }
      }
    }
    DAM <- median(pre_DAM, na.rm= TRUE)
    media <- mean(x,na.rm=TRUE) 
    # Paso 4: Filtrar los excesos (quitar ceros)
    exceso_negativo <- exceso_negativo[exceso_negativo != 0]
    exceso_positivo <- exceso_positivo[exceso_positivo != 0]
    
    # Guardar los resultados para la empresa actual
    resultados[[empresa]] <- list(
      mediana = mediana_x,
      exceso_negativo = exceso_negativo,
      exceso_positivo = exceso_positivo,
      DAM = DAM,
      media = media
    )
  }
  
  return(resultados)
}

# Aplicar la función a los retornos de las empresas
resultados_excesos <- calcular_excesos(final_df)

calcular_beta_wilcoxon <- function(exceso_negativo, exceso_positivo) {
  
  # Verificar que ambos vectores tengan suficientes datos
  if (length(exceso_negativo) < 2 || length(exceso_positivo) < 2) {
    return(0)  # No se puede calcular β si no hay suficientes datos
  }
  
  # a) Prueba bilateral Wilcoxon entre excesos negativos y positivos
  wilcox_test_b0 <- wilcox.test(exceso_negativo, exceso_positivo, exact = FALSE)  # Prueba bilateral
  testb0 <- wilcox_test_b0$p.value
  
  # b) Prueba Wilcoxon unidireccional "no es menor que" (greater)
  wilcox_test_bnegativo <- wilcox.test(exceso_negativo, exceso_positivo, alternative = "greater", exact = FALSE)
  testbnegativo <- wilcox_test_bnegativo$p.value
  
  # c) Prueba Wilcoxon unidireccional "no es mayor que" (less)
  wilcox_test_bpositivo <- wilcox.test(exceso_negativo, exceso_positivo, alternative = "less", exact = FALSE)
  testbpositivo <- wilcox_test_bpositivo$p.value
  
  # Paso 2: Determinación del valor de β según los resultados
  if (testb0 > 0.05) {
    beta <- 0
  } else if (testbnegativo > 0.05) {
    beta <- -0.5
  } else if (testbpositivo > 0.05) {
    beta <- 0.5
  } else {
    beta <- 0  # Caso en que no se cumpla ninguna de las condiciones
  }
  
  return(beta)
}

# Modificación de la función para incorporar los valores de beta a los parámetros iniciales
incorporar_beta_a_parametros_wilcoxon <- function(parametros_iniciales, resultados_excesos) {
  for (empresa in parametros_iniciales$Empresa) {
    # Obtener los excesos de la empresa
    excesos <- resultados_excesos[[empresa]]
    exceso_negativo <- excesos$exceso_negativo
    exceso_positivo <- excesos$exceso_positivo
    
    # Calcular el valor de β usando Wilcoxon para la empresa
    beta <- calcular_beta_wilcoxon(exceso_negativo, exceso_positivo)
    
    # Obtener DAM y media
    DAM <- excesos$DAM
    media <- excesos$media
    
    # Actualizar el dataframe "parámetros iniciales" con los valores
    parametros_iniciales$beta[parametros_iniciales$Empresa == empresa] <- beta
    parametros_iniciales$gamma[parametros_iniciales$Empresa == empresa] <- DAM
    parametros_iniciales$delta[parametros_iniciales$Empresa == empresa] <- media
  }
  
  return(parametros_iniciales)
}

# Aplicar la función a los parámetros iniciales y los excesos de las empresas usando Wilcoxon
parametros_iniciales <- incorporar_beta_a_parametros_wilcoxon(parametros_iniciales, resultados_excesos)

# Función stablepdf para evaluar la pdf de distribuciones estables
stablepdf <- function(x, alpha, beta) {
  if (length(alpha) > 1 || length(beta) > 1 || alpha < 0.1 || alpha > 2 || abs(beta) > 1) {
    stop("Error: Parámetros inválidos")
  }
  
  # Paso 2: Calcular la pdf según las condiciones de alpha y beta
  else if (alpha == 2 && beta == 0) {
    # Caso 1: Distribución Normal (α = 2, β = 0)
    pdf <- dnorm(x, mean = 0, sd = 1)
  }
  else if (alpha == 1) {
    pdf <- dcauchy(x, location = 0, scale = 1)
  }
    else if (alpha <= 0.5){
      pdf <- dstable(x, alpha, beta, 1, 0, pm = 0)
  } else if (alpha > 0.5 && beta == 0) {
    # Caso 2: Método de Ament y O'Neill con β = 0 (Distribución simétrica)
    pdf <- ament_oneill_pdf(x, alpha, beta)
  } else if (alpha > 0.9 && alpha < 1.1 && beta != 0) {
    # Caso 3: Método de Nolan para α cercano a 1
    pdf <- dstable(x, alpha, beta, 1, 0, pm = 0)
  }  else {
    # Caso 4: Método de Ament y O'Neill para otros valores de α y β (Distribución asimétrica)
    pdf <- ament_oneill_pdf(x, alpha, beta)
  }
  
  return(pdf)
}

ament_oneill_pdf <- function(x, alpha, beta) {
  
  # Parámetro zeta
  z <- -beta * tan(pi * alpha / 2)
  eps <- 1e-16
  
  if (beta == 0) {
    n <- 42
    min_inf_x <- (alpha / (pi * eps) * gamma(alpha * n) / gamma(n))^(1 / (alpha * n - 1))
    pdf <- rep(0, length(x))
    
    neg_inf_cond <- x < -min_inf_x
    if (any(neg_inf_cond)) {
      x[neg_inf_cond] <- -x[neg_inf_cond]
    }
    
    inf_cond <- x > min_inf_x
    if (any(inf_cond)) {
      pdf[inf_cond] <- stable_pdf_series_infinity(x[inf_cond], alpha, 0, n)
    }
    
    fourier_cond <- !inf_cond
    if (any(fourier_cond)) {
      pdf[fourier_cond] <- stable_sym_pdf_fourier_integral(x[fourier_cond], alpha)
    }
    
    return(pdf)
  }
  
  else if (beta != 0) {
    if (alpha >= 0.5 && alpha <= 0.9) {
      n <- 90
    } else if (alpha >= 1.1 && alpha <= 2) {
      n <- 80
    }
    
    pdf <- rep(NA_real_, length(x))
    xlz_cond <- x < z
    if (any(xlz_cond)) {
      pdf[xlz_cond] <- ament_oneill_pdf(-x[xlz_cond], alpha, -beta)
    }
    
    if (any(!xlz_cond)) {
      min_inf_x <- ((1 + z^2)^(n / 2) * alpha / (pi * eps) * gamma(alpha * n) / gamma(n))^(1 / (alpha * n - 1))
      min_inf_x <- min_inf_x + z
      
      inf_cond <- min_inf_x < x
      if (any(inf_cond)) {
        
        pdf[inf_cond] <- stable_pdf_series_infinity(x[inf_cond], alpha, beta, n)
      }
      
      fourier_cond <- (z < x) & !inf_cond
      if (any(fourier_cond)) {
        pdf[fourier_cond] <- stable_pdf_fourier_integral(x[fourier_cond], alpha, beta)
      }
    }
    
    return(pdf)
  }
}
stable_pdf_series_infinity <- function(x, alpha, beta, max_coef) {
  zeta <- -beta * tan((pi * alpha) / 2)
  k_index <- seq(0, max_coef)
  sign <- -1
  gamma_part <- gamma(alpha * (k_index + 1)) / factorial(k_index)
  sqrt_1_plus_zeta <- sqrt(1 + zeta^2)
  geometric_part <- 1
  sin_part <- sin((pi / 2 * alpha - atan(zeta)) * (k_index + 1))
  x_to_minus_a <- (x - zeta)^(-alpha)
  x_part <- 1 / (x - zeta)
  val <- rep(0, length(x))
  for (k in 0:max_coef) {
    sign <- -sign
    geometric_part <- geometric_part * sqrt_1_plus_zeta
    x_part <- x_part * x_to_minus_a
    term <- sign * gamma_part[k + 1] * geometric_part * sin_part[k + 1] * x_part
    val <- val + term
  }
  val <- val * alpha / pi
  return(val)
}

stable_pdf_fourier_integral <- function(x, alpha, beta) {
  eps <- 1e-16   # Máquina de precisión
  
  if (alpha >= 1.1) {
    # Parámetros para cuadratura gaussiana
    num_quad <- 86
    
    # Nodos y pesos de cuadratura para alpha >= 1.1
    gx <- c(7.0370416932351169e-06, 8.9457728587460458e-05, 4.5195717164507891e-04, 1.4387800013186535e-03, 3.4238229401029920e-03,
            6.6679664869140516e-03, 1.1244527018077863e-02, 1.7062429312798121e-02, 2.3941528921398801e-02, 3.1681730492178345e-02,
            4.0101739779817722e-02, 4.9052345082266816e-02, 5.8416313367213923e-02, 6.8103426617886029e-02, 7.8044751687152913e-02,
            8.8187621674808730e-02, 9.8491651958493867e-02, 1.0892570482430752e-01, 1.1946561554032360e-01, 1.3009249854990793e-01,
            1.4079148624023027e-01, 1.5155078763117422e-01, 1.6236098315732694e-01, 1.7321449381516493e-01, 1.8410517939724058e-01,
            1.9502803262698309e-01, 2.0597894484676815e-01, 2.1695452535829091e-01, 2.2795196121330644e-01, 2.3896890767984083e-01,
            2.5000340211253524e-01, 2.6105379579367261e-01, 2.7211869966031971e-01, 2.8319694083099400e-01, 2.9428752758178356e-01,
            3.0538962098220246e-01, 3.1650251181003752e-01, 3.2762560167565286e-01, 3.3875838752038662e-01, 3.4990044884671112e-01,
            3.6105143718373622e-01, 3.7221106735260195e-01, 3.8337911022287513e-01, 3.9455538670580470e-01, 4.0573976283971641e-01,
            4.1693214574735116e-01, 4.2813248020546890e-01, 4.3934074598515077e-01, 4.5055695570150323e-01, 4.6178115355311344e-01,
            4.7301341376196310e-01, 4.8425383952216777e-01, 4.9550256288736721e-01, 5.0675974549192959e-01, 5.1802557928302240e-01,
            5.2930028492144732e-01, 5.4058411161129039e-01, 5.5187734378208209e-01, 5.6318030393397900e-01, 5.7449335812231850e-01,
            5.8581688888130956e-01, 5.9715132998513110e-01, 6.0849716286556388e-01, 6.1985498063016387e-01, 6.3122540832012675e-01,
            6.4260904482606040e-01, 6.5400647463066774e-01, 6.6541875634974845e-01, 6.7684685622122265e-01, 6.8829256028106156e-01,
            6.9975558376055846e-01, 7.1123699066990020e-01, 7.2273896702242169e-01, 7.3426487948155317e-01, 7.4582269874591844e-01,
            7.5740470496696566e-01, 7.6901033051191892e-01, 7.8063124635092274e-01, 7.9230651366491101e-01, 8.0410492102734399e-01,
            8.1612625247941939e-01, 8.2818135034865736e-01, 8.4120303825748510e-01, 8.5677958040655922e-01, 8.7803721055848216e-01,
            9.1078416473987345e-01)
    
    gw <- c(2.4123585761838668e-05, 1.7469393088180406e-04, 6.0975563988952377e-04, 1.4288650277602072e-03, 2.5860145358909906e-03, 
            3.9139638862970855e-03, 5.2225620441390059e-03, 6.3820155329709829e-03, 7.3422109186047540e-03, 8.1077876105023049e-03, 
            8.7073841240531413e-03, 9.1743792256825593e-03, 9.5386194524040412e-03, 9.8242023344950434e-03, 1.0049736076644946e-02, 
            1.0229323237713391e-02, 1.0373585169887569e-02, 1.0490520985968452e-02, 1.0586173608364573e-02, 1.0665129967020341e-02, 
            1.0730891859179584e-02, 1.0786149736593508e-02, 1.0832984766811224e-02, 1.0873018221674781e-02, 1.0907522259639847e-02, 
            1.0937502403629278e-02, 1.0963759233467768e-02, 1.0986934773375319e-02, 1.1007547570747180e-02, 1.1026019381147249e-02, 
            1.1042695595265807e-02, 1.1057860975532409e-02, 1.1071751857045479e-02, 1.1084565666505860e-02, 1.1096468395525276e-02, 
            1.1107600514184439e-02, 1.1118081666841355e-02, 1.1128014432939991e-02, 1.1137487348001939e-02, 1.1146577371101231e-02, 
            1.1155351893346113e-02, 1.1163870355731593e-02, 1.1172185618068415e-02, 1.1180345086850615e-02, 1.1188391761261667e-02, 
            1.1196364889593627e-02, 1.1204300700566747e-02, 1.1212232963606782e-02, 1.1220193879270582e-02, 1.1228214214753063e-02, 
            1.1236323377118574e-02, 1.1244550178188855e-02, 1.1252923441962841e-02, 1.1261473748637778e-02, 1.1270230850846127e-02, 
            1.1279221450164715e-02, 1.1288479658221117e-02, 1.1298037896021292e-02, 1.1307944646122644e-02, 1.1318225812893915e-02, 
            1.1328910209404287e-02, 1.1340046911577153e-02, 1.1351721380555410e-02, 1.1364027498287044e-02, 1.1376911984350185e-02, 
            1.1390449917960382e-02, 1.1404631513115529e-02, 1.1419998637800782e-02, 1.1436706611044661e-02, 1.1454320558660740e-02, 
            1.1472107181507585e-02, 1.1491546656562140e-02, 1.1511214906719663e-02, 1.1544621281952909e-02, 1.1568033041001595e-02, 
            1.1596005189903223e-02, 1.1611886374448196e-02, 1.1642999393803366e-02, 1.1703605655774101e-02, 1.1944617349004275e-02, 
            1.1998375088894278e-02, 1.2329022152040849e-02, 1.3931620772305212e-02, 1.7679054162172823e-02, 2.5992451059429083e-02, 
            3.9945131495550699e-02)
    
  } else if (alpha > 0.9) {
    stop("Valores de alpha en el rango (0.9, 1.1) no soportados")
    
  } else if (alpha >= 0.5) {
    
    num_quad <- 94
    
    # Nodos y pesos de cuadratura para alpha entre 0.5 y 0.9
    gx <- c(3.7609390003375964e-08, 
            1.3685854979526549e-06, 
            1.4889428111233842e-05, 
            8.4909234018965159e-05, 
            3.1873821783035098e-04, 
            8.9204133360438380e-04, 
            2.0127465688419980e-03, 
            3.8618683453436136e-03, 
            6.5470889986536940e-03, 
            1.0090726664739097e-02, 
            1.4447552604036681e-02, 
            1.9533895674843968e-02, 
            2.5252790259180181e-02, 
            3.1509724331768026e-02, 
            3.8220152765976879e-02, 
            4.5311835199784489e-02, 
            5.2724547956529318e-02, 
            6.0408722152233675e-02, 
            6.8323799499387480e-02, 
            7.6436655143339735e-02, 
            8.4720214965027643e-02, 
            9.3152294763300211e-02, 
            1.0171464825117857e-01, 
            1.1039219699792262e-01, 
            1.1917241285088062e-01, 
            1.2804482522689517e-01, 
            1.3700062911969721e-01, 
            1.4603237349945128e-01, 
            1.5513371343540050e-01, 
            1.6429921251385121e-01, 
            1.7352418487154128e-01, 
            1.8280456842410028e-01, 
            1.9213682268853544e-01, 
            2.0151784603996090e-01, 
            2.1094490837374680e-01, 
            2.2041559602735405e-01, 
            2.2992776650187988e-01, 
            2.3947951105149096e-01, 
            2.4906912362070266e-01, 
            2.5869507493505961e-01, 
            2.6835599079492339e-01, 
            2.7805063380973916e-01, 
            2.8777788796725268e-01, 
            2.9753674557007820e-01, 
            3.0732629613371543e-01, 
            3.1714571693036520e-01, 
            3.2699426489914540e-01, 
            3.3687126977933890e-01, 
            3.4677612827216525e-01, 
            3.5670829904465617e-01, 
            3.6666729838885320e-01, 
            3.7665269672921287e-01, 
            3.8666411580352911e-01, 
            3.9670122617260967e-01, 
            4.0676374501492912e-01, 
            4.1685143449747197e-01, 
            4.2696410186536610e-01, 
            4.3710159821272931e-01, 
            4.4726381749484123e-01, 
            4.5745069603649885e-01, 
            4.6766221598520491e-01, 
            4.7789840652298787e-01, 
            4.8815934536220495e-01, 
            4.9844515052405652e-01, 
            5.0875598897275465e-01, 
            5.1909210453586785e-01, 
            5.2945380107118389e-01, 
            5.3984142933037105e-01, 
            5.5025535726907182e-01, 
            5.6069612221610865e-01, 
            5.7116437727195657e-01, 
            5.8166086578398579e-01, 
            5.9218626968330357e-01, 
            6.0274130153474181e-01, 
            6.1332726467412169e-01, 
            6.2394583575131990e-01, 
            6.3459902800886914e-01, 
            6.4528672842975432e-01, 
            6.5601051151983347e-01, 
            6.6677339218565990e-01, 
            6.7758442124720375e-01, 
            6.8844699690792643e-01, 
            6.9935285580563211e-01, 
            7.1030369804167903e-01, 
            7.2128236992493600e-01, 
            7.3240671216090558e-01, 
            7.4367772949733946e-01, 
            7.5519374839089404e-01, 
            7.6736343736510493e-01, 
            7.7970740763740065e-01, 
            7.9219118119668186e-01, 
            8.0570157617752125e-01, 
            8.2101055899513764e-01, 
            8.4330359349538586e-01)
    gw <- c(1.7500611765012179e-07, 
            3.8882557435783513e-06, 
            2.9954641912492096e-05, 
            1.2782804549555320e-04, 
            3.6976304598021047e-04, 
            8.1258486789405663e-04, 
            1.4598247640076655e-03, 
            2.2565489131548054e-03, 
            3.1172626255617855e-03, 
            3.9619509977401879e-03, 
            4.7373256373690322e-03, 
            5.4189419764267398e-03, 
            6.0029781163538484e-03, 
            6.4967663749125951e-03, 
            6.9120587945015596e-03, 
            7.2612932414371596e-03, 
            7.5558989407095746e-03, 
            7.8057162802363175e-03, 
            8.0189406936974315e-03, 
            8.2022775474879586e-03, 
            8.3611605279137139e-03, 
            8.4999707834225811e-03, 
            8.6222333472394299e-03, 
            8.7307842750017125e-03, 
            8.8279088186299960e-03, 
            8.9154535861816365e-03, 
            8.9949164606365779e-03, 
            9.0675180782155505e-03, 
            9.1342583376412181e-03, 
            9.1959609393384989e-03, 
            9.2533084527800825e-03, 
            9.3068699414011486e-03, 
            9.3571227648605662e-03, 
            9.4044698327488199e-03, 
            9.4492533078482065e-03, 
            9.4917655360856907e-03, 
            9.5322578036120403e-03, 
            9.5709473825848993e-03, 
            9.6080232366267462e-03, 
            9.6436506619579036e-03, 
            9.6779750821076437e-03, 
            9.7111251615898172e-03, 
            9.7432153882503012e-03, 
            9.7743482267823173e-03, 
            9.8046159138632880e-03, 
            9.8341019486747616e-03, 
            9.8628823752800656e-03, 
            9.8910269212001307e-03, 
            9.9185999146112931e-03, 
            9.9456610759173381e-03, 
            9.9722660729993397e-03, 
            9.9984675891846859e-03, 
            1.0024315689563553e-02, 
            1.0049857967087024e-02, 
            1.0075140023966186e-02, 
            1.0100206959935651e-02, 
            1.0125102938360671e-02, 
            1.0149871950774473e-02, 
            1.0174556113780179e-02, 
            1.0199197618018014e-02, 
            1.0223846640284091e-02, 
            1.0248548038151891e-02, 
            1.0273348419043692e-02, 
            1.0298288470141991e-02, 
            1.0323432854489152e-02, 
            1.0348849701458745e-02, 
            1.0374601974019832e-02, 
            1.0400712381956034e-02, 
            1.0427243781987490e-02, 
            1.0454389862197593e-02, 
            1.0482244702565721e-02, 
            1.0510876149932735e-02, 
            1.0540013946875334e-02, 
            1.0570289997603391e-02, 
            1.0601804398556677e-02, 
            1.0635579476156305e-02, 
            1.0670744865279679e-02, 
            1.0705103070143742e-02, 
            1.0742678704510395e-02, 
            1.0784424144377271e-02, 
            1.0838880134468544e-02, 
            1.0885073551248455e-02, 
            1.0926729010027615e-02, 
            1.0963337430818136e-02, 
            1.1008609571599130e-02, 
            1.1284106092130687e-02, 
            1.1237297680636530e-02, 
            1.1867898115707079e-02, 
            1.2386002478015653e-02, 
            1.2225388580440253e-02, 
            1.2967800176569934e-02, 
            1.3961994785569806e-02, 
            1.6951270842679722e-02, 
            3.1804594306850377e-02)
  }
  
  # Inicialización de pdf
  pdf <- rep(0, length(x))
  
  # Escalar gx y gw
  rank_scaling <- (-log(eps))^(1 / alpha)
  gx <- rank_scaling * gx
  gw <- rank_scaling / pi * gw
  
  gx_to_a <- gx^alpha
  exp_gx_to_a <- exp(-gx_to_a)
  
  z <- -beta * tan(alpha * pi / 2)
  
  # Cálculo de la integral para cada j
  for (j in seq_along(gx)) {
    h <- (x - z) * gx[j] + z * gx_to_a[j]
    pdf <- pdf + gw[j] * cos(h) * exp_gx_to_a[j]
  }
  
  return(pdf)
}

stable_sym_pdf_fourier_integral <- function(x, alpha) {
  num_quad <- 46
  eps <- 1e-16
  gx <- c(2.7148704107693849e-08, 
          1.1539065574946093e-06, 
          1.4068944360213799e-05, 
          8.8528868370045850e-05, 
          3.6522895950416606e-04, 
          1.1242856520043885e-03, 
          2.7940849388937931e-03, 
          5.8998374370067466e-03, 
          1.0954815404797378e-02, 
          1.8308965706909420e-02, 
          2.7904977612154668e-02, 
          3.8402581477255261e-02, 
          4.7620451072357205e-02, 
          6.0322361829278297e-02, 
          7.7212543014189644e-02, 
          9.6567947626676073e-02, 
          1.1770638898039337e-01, 
          1.4023188371095557e-01, 
          1.6381653230311635e-01, 
          1.8807833684292100e-01, 
          2.1220569852597571e-01, 
          2.3337594286746213e-01, 
          2.5067518179499915e-01, 
          2.7308170043295299e-01, 
          2.9923165644921623e-01, 
          3.2670651619049274e-01, 
          3.5488605833898679e-01, 
          3.8356368161790211e-01, 
          4.1264154879954701e-01, 
          4.4206132991284341e-01, 
          4.7178318417560017e-01, 
          5.0177774958174326e-01, 
          5.3202251254241339e-01, 
          5.6249992102768298e-01, 
          5.9319640320449418e-01, 
          6.2410173262716340e-01, 
          6.5520873661416879e-01, 
          6.8651280590359287e-01, 
          7.1801277568472022e-01, 
          7.4971030516593817e-01, 
          7.8161424879996133e-01, 
          8.1374899659119626e-01, 
          8.4616111090172197e-01, 
          8.7910800331170069e-01, 
          9.1405149366238070e-01, 
          9.5623769770932743e-01)
  
  gw <- c(1.3158109639902134e-07, 
          3.4262543803194494e-06, 
          2.9760561009721337e-05, 
          1.4170149519462160e-04, 
          4.5818570532242872e-04, 
          1.1321935880671628e-03, 
          2.2968035970710459e-03, 
          4.0027607768915521e-03, 
          6.1688253884964774e-03, 
          8.5398963899097589e-03, 
          1.0476270652386517e-02, 
          9.8155372802538974e-03, 
          9.9827686069404245e-03, 
          1.5219506000563725e-02, 
          1.8284735354731164e-02, 
          2.0324582923350147e-02, 
          2.1888765891758585e-02, 
          2.3109125676823113e-02, 
          2.4000778165476255e-02, 
          2.4409563071531602e-02, 
          2.3429581499251423e-02, 
          1.8132920380634698e-02, 
          1.9021640675265461e-02, 
          2.4967741279101181e-02, 
          2.6986577522376015e-02, 
          2.7877980988375722e-02, 
          2.8450142752054142e-02, 
          2.8889685328033834e-02, 
          2.9256620565013172e-02, 
          2.9576419596863371e-02, 
          2.9862450428147645e-02, 
          3.0122958378570940e-02, 
          3.0363656749165541e-02, 
          3.0588909434433681e-02, 
          3.0802353645674175e-02, 
          3.1007120102372672e-02, 
          3.1206109628651280e-02, 
          3.1401812940439804e-02, 
          3.1598474495187191e-02, 
          3.1797517989457728e-02, 
          3.2015445283346267e-02, 
          3.2257598848695605e-02, 
          3.2604667010320047e-02, 
          3.3454059051937490e-02, 
          3.7403084267749798e-02, 
          4.8035994401521127e-02)
  # Inicialización de pdf
  pdf <- rep(0, length(x))
  
  # Escalar gx y gw
  rank_scaling <- (-log(eps))^(1 / alpha)
  gx <- rank_scaling * gx
  gw <- rank_scaling / pi * gw
  
  gx_to_a <- gx^alpha
  exp_gx_to_a <- exp(-gx_to_a)
  
  # Cálculo de la integral para cada j
  for (j in seq_along(gx)) {
    pdf <- pdf + gw[j] * cos(x*gx[j]) * exp_gx_to_a[j]
  }
  return(pdf)
}

init_bounds <- function(final_df, parametros_iniciales) {
  resultados <- list()
  # Filtrar las columnas que terminan en 'Return'
  empresas <- grep("_Return$", colnames(final_df), value = TRUE)
  for (empresa in empresas) {
    # Obtener los parámetros iniciales para la empresa
    parametros <- parametros_iniciales[parametros_iniciales$Empresa == empresa, ]
    alpha <- parametros$alpha
    beta <- parametros$beta
    gamma <- parametros$gamma
    delta <- parametros$delta
    lower_bounds <- c(0.10000, -1, 0.001, 0)
    upper_bounds <- c(2.00000, 1, Inf, 0)
    init_vals <- c(alpha, beta, gamma, 0)  # [gamma, delta]
    # Guardar los resultados para la empresa en la lista
    resultados[[empresa]] <- list(lower_bounds = lower_bounds, upper_bounds = upper_bounds, init_vals = init_vals)
  }
  return(resultados)  # Ahora el return está fuera del ciclo for
}
bounds_vals <- init_bounds(final_df, parametros_iniciales)
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

stable_lm_custom <- function(formula, data, bounds, trace = FALSE, optim_control = list()) {
  # Extraer los valores de la lista
  lower_bounds <- bounds$lower_bounds
  upper_bounds <- bounds$upper_bounds
  init_vals <- bounds$init_vals
  
  # Crear el modelo
  mf <- model.frame(formula = formula, data = data)
  X <- model.matrix(attr(mf, "terms"), mf)
  y <- model.response(mf)
  n_regr <- ncol(X)  # Número de coeficientes en el modelo lineal
  
  # Definir la función objetivo
  objf <- function(p) {
    resids <- y - c(X %*% p[5:(4 + n_regr)])  # Ajuste de intercepto y pendiente con todas las variables
    a <- p[1]
    b <- p[2]
    gamma <- p[3]
    delta <- p[4]
    pdf_vals <- stablepdf((resids - delta) / gamma, a, b)
    pdf_vals <- pmax(.Machine$double.xmin, pdf_vals)
    return(-(-length(resids) * log1p(gamma - 1) + sum(log(pdf_vals))))
  }
  
  # Ajustar las dimensiones de los límites e inicialización
  init_vals <- c(init_vals, rep(0, n_regr))  # Agregar espacio para coeficientes
  lower_bounds <- c(lower_bounds, rep(-Inf, n_regr))
  upper_bounds <- c(upper_bounds, rep(Inf, n_regr))
  
  # Optimización
  r <- nlminb(objective = objf, start = init_vals, lower = lower_bounds,
              upper = upper_bounds, control = list(trace = trace))
  
  if (r$convergence != 0)
    warning("Optimization did not converge")
  
  # Cálculo de los residuos y métricas
  resids <- y - c(X %*% r$par[5:(4 + n_regr)])
  log_lik <- -r$objective
  k <- n_regr + 4  # Total de parámetros estimados
  bic <- log(length(resids)) * k - 2 * log_lik
  mse <- mean(resids^2, na.rm = TRUE)
  values <- floor(sqrt(length(resids)))
  heavy_tails <- sapply(values, function(n) perform_test(resids, n))
  
  # Resultado final con todos los coeficientes y métricas
  res <- structure(list(
    coefs = structure(r$par, names = c("alpha", "beta", "gamma", "delta", colnames(X))),
    aic = 2 * k + 2 * log_lik,
    bic = bic,
    mse = mse,
    heavy_tails = heavy_tails,
    resids = resids
  ))
  
  return(res)
}

ajustar_serie2 <- function(retornos_empresa) {
  # Eliminar los NA del inicio de la serie y mantener la fecha
  start_index <- min(which(!is.na(retornos_empresa$Retorno)))
  if (!is.na(start_index)) {
    retornos_empresa <- retornos_empresa[start_index:nrow(retornos_empresa), ]
  }
  return(retornos_empresa)
}
apply_stable_lm_capm <- function(final_df_filtrado, parametros_iniciales) {
  
  # Crear una lista para almacenar los resultados de la optimización por empresa
  resultados_empresas <- list()
  
  # Iterar sobre cada empresa en el dataframe
  for (empresa in colnames(final_df_filtrado)) {
    if (grepl("_Return$", empresa)) {
      cat("Procesando empresa:", empresa, "\n")
      
      tryCatch({
        # Extraer los retornos de la empresa con la columna de fecha
        retornos_empresa <- final_df_filtrado[, c("Date", empresa)]
        colnames(retornos_empresa) <- c("Date", "Retorno")
        
        # Ajustar la serie eliminando los NAs de la columna de retorno
        retornos_empresa <- ajustar_serie2(retornos_empresa)
        
        # Extraer market_returns y variables adicionales también con la columna de fecha
        variables_adicionales <- final_df_filtrado[, c("Date", "Rm_Rf")]
        
        # Realizar el merge entre retornos_empresa y variables_adicionales por la columna "Date"
        emp_df <- merge(variables_adicionales, retornos_empresa, by = "Date", all.x = TRUE)
        
        # Inicializar los bounds para la empresa
        bounds_vals <- init_bounds(final_df_filtrado, parametros_iniciales)
        bounds <- bounds_vals[[empresa]]
        
        # Aplicar stable_lm_custom para la empresa con la fórmula que incluye las variables adicionales
        fit_result <- stable_lm_custom(
          formula = Retorno ~ Rm_Rf,
          data = emp_df,
          bounds = bounds
        )
        
        # Guardar los resultados en la lista
        resultados_empresas[[empresa]] <- fit_result
        cat("Empresa procesada con éxito:", empresa, "\n")  # Mensaje de éxito
        
      }, error = function(e) {
        # Capturar errores y continuar con la siguiente empresa
        cat("Error en la empresa:", empresa, "\nMensaje de error:", e$message, "\n")
        resultados_empresas[[empresa]] <- NA  # O puedes guardar algún mensaje o valor especial
      })
    }
  }
  
  return(resultados_empresas)  # Devolver los resultados de todas las empresas
}

# Función para procesar cada empresa con mensajes de depuración
procesar_empresa <- function(empresa) {
  tryCatch({
    cat("Procesando empresa:", empresa, "\n")
    
    # Extraer los retornos de la empresa con la columna de fecha
    retornos_empresa <- final_df_filtrado[, c("Date", empresa)]
    colnames(retornos_empresa) <- c("Date", "Retorno")
    
    # Ajustar la serie eliminando los NAs de la columna de retorno
    retornos_empresa <- ajustar_serie2(retornos_empresa)
    cat("Serie ajustada para", empresa, "\n")
    
    # Extraer market_returns y otras variables adicionales con la columna de fecha
    market_returns <- final_df_filtrado[, c("Date", "Rm_Rf")]
    
    # Realizar el merge entre retornos_empresa y market_returns por la columna "Date"
    emp_df <- merge(market_returns, retornos_empresa, by = "Date", all.x = TRUE)
    cat("Merge realizado para", empresa, "\n")
    
    # Inicializar los bounds para la empresa
    bounds_vals <- init_bounds(final_df_filtrado, parametros_iniciales)
    bounds <- bounds_vals[[empresa]]
    
    # Aplicar stable_lm_custom para la empresa con la fórmula del CAPM y variables adicionales
    fit_result <- stable_lm_custom(
      formula = Retorno ~ Rm_Rf,
      data = emp_df,
      bounds = bounds
    )
    
    cat("Optimización completada para", empresa, "\n")
    return(fit_result)
    
  }, error = function(e) {
    cat("Error en la empresa:", empresa, "\nMensaje de error:", e$message, "\n")
    return(NA)  # Devuelve NA en caso de error
  })
}
# Detectar la cantidad de núcleos disponibles
num_cores <- detectCores() - 1  # Usa todos los núcleos menos uno
cl <- makeCluster(num_cores)

# Exportar las funciones y variables necesarias al cluster
clusterExport(cl, list("final_df_filtrado", "parametros_iniciales", "init_bounds", 
                       "stable_lm_custom", "ajustar_serie2", "procesar_empresa", "stablepdf", "ament_oneill_pdf"
                       ,"stable_pdf_series_infinity", "stable_pdf_fourier_integral", "dstable", "stable_sym_pdf_fourier_integral", "dnorm"
                       , "perform_test", "calculate_S_hat"))


# Procesar las empresas en paralelo con más mensajes de depuración
empresas <- colnames(final_df_filtrado)[grepl("_Return$", colnames(final_df_filtrado))]

# Aplicar la paralelización con pblapply
resultados_empresas <- pblapply(empresas, procesar_empresa, cl = cl)

# Detener el clúster
stopCluster(cl)
exportar_a_excel_openxlsx <- function(resultados_empresas, parametros_iniciales, archivo_salida) {
  
  # Crear un data frame inicial vacío incluyendo nuevas variables
  datos_a_exportar <- data.frame(
    Empresa = character(),
    Alpha_Ini = numeric(),
    Beta_Ini = numeric(),
    Gamma_Ini = numeric(),
    Delta_Ini = numeric(),
    Alpha = numeric(),
    Beta = numeric(),
    Gamma = numeric(),
    Delta = numeric(),
    Coef_5 = numeric(),
    Coef_6 = numeric(),
    AIC = numeric(),
    BIC = numeric(),
    MSE = numeric(),
    Heavy_Tails = logical(),
    stringsAsFactors = FALSE
  )
  
  # Obtener los nombres de las empresas
  nombres_empresas <- colnames(final_df_filtrado)[grepl("_Return$", colnames(final_df_filtrado))]
  
  for (i in seq_along(resultados_empresas)) {
    resultado <- resultados_empresas[[i]]
    
    # Validar que `resultado` sea una lista con el campo `coefs`
    if (!is.list(resultado)) {
      cat("Advertencia: El resultado para la empresa", nombres_empresas[i], "no es una lista\n")
      next
    }
    if (is.null(resultado$coefs) || !is.numeric(resultado$coefs) || length(resultado$coefs) < 6) {
      cat("Advertencia: El resultado para la empresa", nombres_empresas[i], "no tiene coeficientes válidos\n")
      next
    }
    
    # Validar que `parametros_iniciales` contiene la empresa correspondiente
    parametros_ini <- parametros_iniciales[parametros_iniciales$Empresa == nombres_empresas[i], ]
    if (nrow(parametros_ini) == 0) {
      cat("Advertencia: No se encontraron parámetros iniciales para la empresa:", nombres_empresas[i], "\n")
      next
    }
    
    # Crear una fila para los datos de la empresa
    fila_empresa <- data.frame(
      Empresa = nombres_empresas[i],
      Alpha_Ini = parametros_ini$alpha,
      Beta_Ini = parametros_ini$beta,
      Gamma_Ini = parametros_ini$gamma,
      Delta_Ini = parametros_ini$delta,
      Alpha = resultado$coefs[1],
      Beta = resultado$coefs[2],
      Gamma = resultado$coefs[3],
      Delta = resultado$coefs[4],
      Coef_5 = resultado$coefs[5],
      Coef_6 = resultado$coefs[6],
      AIC = resultado$aic,
      BIC = resultado$bic,
      MSE = resultado$mse,
      Heavy_Tails = resultado$heavy_tails,
      stringsAsFactors = FALSE
    )
    
    # Agregar la fila al data frame final
    datos_a_exportar <- rbind(datos_a_exportar, fila_empresa)
  }
  
  # Crear un workbook de openxlsx y agregar una hoja
  wb <- createWorkbook()
  addWorksheet(wb, "Resultados")
  
  # Escribir los datos en la hoja
  writeData(wb, "Resultados", datos_a_exportar)
  
  # Guardar el archivo Excel
  saveWorkbook(wb, archivo_salida, overwrite = TRUE)
  
  cat("Archivo exportado con éxito:", archivo_salida, "\n")
}
# Crear el nombre del archivo de exportación con las fechas
start_year <- format(as.Date(start_date), "%Y")
end_year <- format(as.Date(end_date), "%Y")

# Definir el nombre del archivo de salida
#archivo_salida <- paste0("C:/Users/alexi/OneDrive - Universidad Técnica Federico Santa María/2024-2/Paper CAPM/CAPM Oct/results_est_", start_year, "_to_", end_year, ".xlsx")
archivo_salida <- paste0("C:/Users/usuario/OneDrive - Universidad Técnica Federico Santa María/2025/Paper CAPM/Análisis/Salidas_Estable/results_est_", start_year, "_to_", end_year, ".xlsx")
# Llamar a la función para exportar a Excel con el nombre de archivo
exportar_a_excel_openxlsx(resultados_empresas, parametros_iniciales, archivo_salida)
#carpeta_histogramas <- "C:/Users/usuario/OneDrive - Universidad Técnica Federico Santa María/2024-2/Paper CAPM/CAPM Oct/carpeta_histogramas_est_2224"
carpeta_histogramas <- "C:/Users/alexi/OneDrive - Universidad Técnica Federico Santa María/2024-2/Paper CAPM/CAPM Oct/carpeta_histogramas_est_cv_1724"
dir.create(carpeta_histogramas, showWarnings = FALSE, recursive = TRUE)
generar_histogramas_residuos <- function(resultados_empresas, carpeta_histogramas) {
  
  # Verificar si la carpeta existe, si no, crearla
  if (!dir.exists(carpeta_histogramas)) {
    dir.create(carpeta_histogramas)
  }
  
  for (empresa in seq_along(resultados_empresas)) {
    resultado <- resultados_empresas[[empresa]]
    
    # Verificar si 'resultado' y 'resids' son válidos
    if (!is.null(resultado$resids)) {
      
      # Obtener los residuos
      resids <- resultado$resids
      coefs <- resultado$coefs
      
      # Crear el dataframe con los residuos
      resids_df <- data.frame(Residuos = resids)
      
      # Parámetros de la distribución estable (alpha, beta, gamma, delta)
      alpha <- coefs[1]
      beta <- coefs[2]
      gamma <- coefs[3]
      delta <- coefs[4]
      
      # Crear la secuencia de valores para la curva
      x_seq <- seq(min(resids), max(resids), length.out = 100)
      
      # Calcular la densidad de la distribución estable
      densidad_estable <- stabledist::dstable(x_seq, alpha = alpha, beta = beta, gamma = gamma, delta = delta, pm = 0)
      
      # Calcular la densidad normal con media `delta` y desviación estándar `gamma`
      densidad_normal <- dnorm(x_seq, mean = delta, sd = gamma)
      
      # Crear histograma y añadir ambas curvas de densidad
      plot_hist <- ggplot(resids_df, aes(x = Residuos)) +
        geom_histogram(bins = 100, fill = "blue", color = "white", alpha = 1) +
        
        # Curva de densidad estable (roja)
        geom_line(data = data.frame(x_seq, densidad_estable), 
                  aes(x = x_seq, y = densidad_estable * length(resids) * diff(range(resids)) / 100), 
                  color = "red", linewidth = 1) +
        
        # Curva de densidad normal (verde)
        geom_line(data = data.frame(x_seq, densidad_normal), 
                  aes(x = x_seq, y = densidad_normal * length(resids) * diff(range(resids)) / 100), 
                  color = "green", linewidth = 1) +
        
        # Detalles del gráfico
        labs(title = paste("Histograma de Residuos para Empresa", empresa),
             x = "Residuos", y = "Frecuencia") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "white", color = "black")) +
        
        # Ajustar el eje y para incluir anotaciones
        ylim(0, max(hist(resids, plot = FALSE)$counts) * 1.3) +
        
        # Anotaciones con parámetros de la distribución estable
        annotate("text", x = min(resids), y = max(hist(resids, plot = FALSE)$counts) * 1.25, 
                 label = paste0("alpha = ", round(alpha, 4), 
                                "\nbeta = ", round(beta, 4), 
                                "\ngamma = ", round(gamma, 4), 
                                "\ndelta = ", round(delta, 4)), 
                 hjust = 0, vjust = 1, size = 4, color = "black")
      
      # Guardar el histograma como imagen PNG
      ruta_guardado <- file.path(carpeta_histogramas, paste0("histograma_residuos_", empresa, ".png"))
      ggsave(ruta_guardado, plot = plot_hist, width = 8, height = 6)
      
    } else {
      cat("Saltando la empresa:", empresa, "- valores de residuos inválidos o faltantes.\n")
    }
  }
  
  cat("¡Histogramas guardados correctamente en la carpeta!\n")
}

# Llamada a la función con el nombre de la carpeta de destino
generar_histogramas_residuos(resultados_empresas, carpeta_histogramas)

