source('Funciones.R')

datos = read.csv("Food_Waste_data.csv") 
datos_filtrados = datos[, c(1, 4, 6, 8, 9)]
colnames(datos_filtrados) = c("Country", "HouseholdEstimate", "RetailEstimate", "FoodServiceEstimate", "ConfidenceInEstimate")

# Servicios = Cantidad estimada de desperdicio de comida de servicios/proveedores de comida del país medida en toneladas en el año 2021.

# Como la distribucion de la variable Servicios cambia notoriamente en los disntintos niveles de confianza, se eligió por
# Very Low Confidence al ser el mas cercano a los modelos de distribucion Pareto y Log-Normal

# v.a. elegida --> Servicio con Very Low Confidence

datos_Very_Low_Confidence = filter(datos_filtrados, ConfidenceInEstimate == "Very Low Confidence")

x = datos_Very_Low_Confidence$FoodServiceEstimate

# Estimacion de la mediana Me = Q2 = X0.5

# NO PARAMETRICA, LOG-NORMAL y PARETO

# 1) CALCULO DE ESTIMADORES

estimadores_mediana = estimar_mediana(x)

# 2) CALCULO DEL ERROR ESTANDAR DE CADA ESTIMADOR CON BOOTSTRAP

# Error estandar de Log-Normal MV

set.seed(123)
medianas_MV_boot = replicate(1000, {
  x_boot = sample(x, size = length(x), replace = TRUE)
  
  m_mv = mean(log(x_boot))
  D2_mv = mean((log(x_boot) - m_mv)^2)
  exp(m_mv + qnorm(0.5) * sqrt(D2_mv))
})

se_mediana_MV = sd(medianas_MV_boot)

# Error estandar de Pareto MV

set.seed(456)
medianas_pareto_boot = replicate(1000, {
  x_boot = sample(x, size = length(x), replace = TRUE)
  
  theta_mv = min(x_boot)
  beta_mv = length(x_boot)/(sum(log(x_boot/theta_mv)))
  qpareto(0.5, scale = theta_mv, shape = beta_mv)
})

se_mediana_pareto = sd(medianas_pareto_boot)

# Error estandar de No-parametrico

set.seed(789)
medianas_no_par_boot = replicate(1000, {
  x_boot = sample(x, size = length(x), replace = TRUE)
  
  quantile(x_boot, 0.5, type=1)
})

se_mediana_np = sd(medianas_no_par_boot)

column_values = c(se_mediana_np, se_mediana_pareto, se_mediana_MV)
estimadores_mediana$Error_estandar = column_values

# 3) AJUSTE DE DISTRIBUCIONES

m_mv = mean(log(x))
D2_mv = mean((log(x) - m_mv)^2)

theta_mv = min(x)
beta_mv = length(x)/(sum(log(x/theta_mv)))

hist(x, main = '', col = '#a2d2df', ylim =c(0, 0.00000024), probability = TRUE)
title(main = 'Histograma de X con modelos Log-normal y Pareto', cex.main = 1, adj = 0.5)
curve(dlnorm(x, meanlog = m_mv, sdlog = sqrt(D2_mv)), col = '#618e3c', add = TRUE, lwd = 3)
curve(dpareto(x, scale = theta_mv, shape = beta_mv), col = '#214dc4', add = TRUE, lwd = 3)
legend("topright", legend = c("Histograma", "Modelo Log-normal", "Modelo Pareto"), col = c("#a2d2df", "#618e3c", "#214dc4"), lwd = 3)
