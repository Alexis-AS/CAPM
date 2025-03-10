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
#install.packages("pbapply")
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
library(pbapply)
#library(stabreg)
start_date <- "2017-10-01"
end_date <- "2024-07-01"
fecha_inicial <- as.Date(start_date)
fecha_final <- as.Date(end_date)
fechas_completas <- data.frame(Date = seq(fecha_inicial, fecha_final, by = "day"))
# Listar todos los archivos CSV que comiencen con "Alpha"
files <- list.files(pattern = "^Alpha.*\\.csv$")

# Cargar los archivos en una lista
data_list <- lapply(files, read.csv)

# Opcional: nombrar cada elemento de la lista por su nombre de archivo
names(data_list) <- files

# Mostrar cuántos archivos se cargaron
cat("Archivos cargados:", length(data_list), "\n")
# Importar y preparar los archivos

files <- list.files(pattern = "^Alpha.*\\.csv$")
data_list <- lapply(files, read.csv)
names(data_list) <- files
# Crear un data frame para almacenar los parámetros extraídos
parameters_df <- data.frame(
  alpha = numeric(), 
  beta = numeric(),
  gamma = numeric(),
  delta = numeric()
)

# Extraer los valores de alpha y beta desde los nombres de los archivos
for (file in files) {
  # Extraer valores de alpha y beta usando expresiones regulares
  matches <- regmatches(file, regexec("Alpha_([0-9]+coma[0-9]+|[0-9]+)_Beta_(Neg)?([0-9]+coma[0-9]+|[0-9]+)", file))[[1]]
  
  # Si se encontró una coincidencia, procesarla
  if (length(matches) > 0) {
    alpha_str <- gsub("coma", ".", matches[2])  # Reemplazar "coma" por "."
    beta_str <- gsub("coma", ".", matches[4])
    if (matches[3] == "Neg") beta_str <- paste0("-", beta_str)  # Añadir signo negativo si corresponde
    
    # Crear un nuevo registro con gamma=1 y delta=0
    parameters_df <- rbind(
      parameters_df, 
      data.frame(alpha = as.numeric(alpha_str), 
                 beta = as.numeric(beta_str), 
                 gamma = 1, 
                 delta = 0)
    )
  }
}
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

stable_mle_fit <- function(x, bounds, trace = FALSE) {
  lower_bounds <- bounds$lower_bounds
  upper_bounds <- bounds$upper_bounds
  init_vals <- bounds$init_vals
  
  # Función objetivo
  objf <- function(p) {
    -(-length(x) * log(p[3]) + sum(log(stablepdf((x - p[4]) / p[3], p[1], p[2]))))
  }
  
  # Optimización
  r <- nlminb(objective = objf, start = init_vals, lower = lower_bounds, upper = upper_bounds, control = list(trace = trace))
  if (r$convergence != 0)
    warning("Optimization did not converge")
  
  # Resultado
  res <- list(coefs = structure(r$par, names = c("alpha", "beta", "scale", "loc")),
              log_lik = -r$objective,
              aic = 8 + 2 * r$objective,
              x = x)
  
  return(res)
}
# Crear un clúster
num_cores <- detectCores() - 1  # Usar todos los núcleos menos uno
cl <- makeCluster(num_cores)

# Exportar datos y funciones necesarias al clúster
clusterExport(cl, list("data_list", "parameters_df", "stable_mle_fit", "stablepdf", "ament_oneill_pdf", 
                       "stable_pdf_series_infinity", "stable_pdf_fourier_integral", "dstable", 
                       "stable_sym_pdf_fourier_integral", "dnorm", "dcauchy"))
clusterEvalQ(cl, library(openxlsx))
# Función auxiliar para procesar una sublista
procesar_sublista <- function(sublist_idx) {
  sublist_key <- names(data_list)[sublist_idx]
  message(paste0("Iniciando procesamiento de sublista: ", sublist_key))
  
  # Inicializar una lista para registrar errores
  errores <- list()
  
  # Obtener los datos de la sublista
  datos_sublista <- data_list[[sublist_idx]]
  datos_sublista <- datos_sublista[, -1]  # Eliminar la primera columna
  
  # Obtener los parámetros correspondientes a la sublista
  parametros <- parameters_df[sublist_idx, ]
  alpha_ini <- parametros$alpha
  beta_ini <- parametros$beta
  gamma_ini <- parametros$gamma
  delta_ini <- parametros$delta
  
  # Definir los límites y valores iniciales
  lower_bounds <- c(0.10000, -1, 0.001, 0)
  upper_bounds <- c(2.00000, 1, Inf, 0)
  init_vals <- c(alpha_ini, beta_ini, 1, 0)
  bounds <- list(lower_bounds = lower_bounds, upper_bounds = upper_bounds, init_vals = init_vals)
  
  # Lista para almacenar resultados de empresas
  resultados_totales <- list()
  
  # Iterar sobre las empresas dentro de la sublista
  for (empresa_key in colnames(datos_sublista)) {
    tryCatch({
      # Obtener los datos de la empresa
      datos_empresa <- datos_sublista[[empresa_key]]
      
      # Estimar los parámetros
      resultados_estable <- stable_mle_fit(datos_empresa, bounds)
      coeficientes <- resultados_estable$coefs
      
      # Calcular errores
      error_alpha <- abs(alpha_ini - coeficientes["alpha"])
      error_beta <- abs(beta_ini - coeficientes["beta"])
      error_scale <- abs(1 - coeficientes["scale"])
      error_loc <- abs(0 - coeficientes["loc"])
      
      # Guardar resultados
      resultados_df <- data.frame(
        Empresa = empresa_key,
        Alpha = coeficientes["alpha"],
        Beta = coeficientes["beta"],
        Scale = coeficientes["scale"],
        Loc = coeficientes["loc"],
        Alpha_ini = alpha_ini,
        Beta_ini = beta_ini,
        Gamma_ini = gamma_ini,
        Delta_ini = delta_ini,
        Error_Alpha = error_alpha,
        Error_Beta = error_beta,
        Error_Scale = error_scale,
        Error_Loc = error_loc
      )
      resultados_totales[[empresa_key]] <- resultados_df
    }, error = function(e) {
      # Registrar errores con detalles
      errores[[empresa_key]] <- list(
        sublist_key = sublist_key,
        empresa = empresa_key,
        error = conditionMessage(e)
      )
      message(paste0("Error en sublista: ", sublist_key, ", empresa: ", empresa_key, ". ", conditionMessage(e)))
    })
  }
  
  # Guardar resultados y errores
  if (length(resultados_totales) > 0) {
    resultados_combinados <- do.call(rbind, resultados_totales)
    excel_filename <- paste0("resultados_", sublist_key, "_estimaciones.xlsx")
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "Resultados")
    writeData(wb, sheet = "Resultados", resultados_combinados)
    saveWorkbook(wb, excel_filename, overwrite = TRUE)
    message(paste0("Archivo guardado: ", excel_filename))
  }
  
  # Guardar errores en un archivo
  if (length(errores) > 0) {
    error_filename <- paste0("errores_", sublist_key, ".txt")
    writeLines(
      sapply(errores, function(err) paste0("Sublista: ", err$sublist_key, ", Empresa: ", err$empresa, 
                                           ", Error: ", err$error)),
      con = error_filename
    )
    message(paste0("Errores registrados en: ", error_filename))
  }
  
  return(paste0("Procesamiento completado para sublista: ", sublist_key))
}

# Aplicar la paralelización con pblapply
resultados_paralelos <- pblapply(seq_along(data_list), procesar_sublista, cl = cl)

# Detener el clúster
stopCluster(cl)