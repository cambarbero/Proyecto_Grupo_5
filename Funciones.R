library(dplyr)
library(ggplot2)
library(grid)
library(fitdistrplus)
library(VGAM)

analisis_descriptivo = function(variable){ # defino la funcion
  minimo = min(variable)
  maximo = max(variable)
  media = mean(variable) # media
  mediana = median(variable) # mediana
  q1 = quantile(variable, 0.25, type=1) # cuantil 0,25
  q3 = quantile(variable, 0.75, type=1) # cuantil 0,75
  iqr = IQR(variable) # rango inter-cuartilico
  desv_est = sd(variable) # desviacion estandar
  coef_var = desv_est/media # coeficiente de variacion
  coef_asim = mean(((variable-media)/desv_est)^3) # coeficiente de asimetria
  coef_curt = mean(((variable-media)/desv_est)^4) # coeficiente de curtosis
  
  data_frame = data.frame(
    Variable = deparse(substitute(variable)),
    Minimo = minimo,
    Maximo = maximo,
    Media = media,
    Mediana = mediana,
    Q1 = q1,
    Q3 = q3,
    IQR = iqr,
    Desviacion_estandar = desv_est,
    Coeficiente_de_variacion = coef_var,
    Coeficiente_de_asimetria = coef_asim,
    Coeficiente_de_curtosis = coef_curt
  )
  
  return (data_frame)
}

f_muestrales = function(variable1, variable2, variable3){ # Grafico de distribucion empírica (no-parametrica)
  par(oma = c(0, 0, 0, 0), mar = c(3, 3, 3, 2), mfrow = c(2, 2))
  plot(ecdf(variable1), main = paste(c('Función de distribución empírica de', deparse(substitute(variable1)), collapse = " ")), col = 'darkgreen')
  plot(ecdf(variable2), main = paste(c('Función de distribución empírica de', deparse(substitute(variable2)), collapse = " ")), col = 'mediumseagreen')
  plot(ecdf(variable3), main = paste(c('Función de distribución empírica de', deparse(substitute(variable3)), collapse = " ")), col = 'turquoise3')
  par(oma = c(5, 4, 4, 2) + 0.1, mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1, 1))
}

f_densidades = function(variable1, variable2, variable3){ # Grafico de densidad
  par(oma = c(0, 0, 0, 0), mar = c(3, 3, 3, 2), mfrow = c(2, 2))
  plot(density(variable1), main = paste(c('Función de densidad de', deparse(substitute(variable1)), collapse = " ")), lwd=3, col = 'darkgreen')
  plot(density(variable2), main = paste(c('Función de densidad de', deparse(substitute(variable2)), collapse = " ")), lwd=3, col = 'mediumseagreen')
  plot(density(variable3), main = paste(c('Función de densidad de', deparse(substitute(variable3)), collapse = " ")), lwd=3, col = 'turquoise3')
  par(oma = c(5, 4, 4, 2) + 0.1, mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1, 1))
}

beeswarms = function(variable1, variable2, variable3){
  par(oma = c(0, 0, 0, 0), mar = c(3, 3, 3, 2), mfrow = c(2, 2))
  beeswarm::beeswarm(variable1, corral="wrap", corralWidth=1, pch=20, col="darkgreen", main = paste(c('Beeswarm de', deparse(substitute(variable1)), collapse = " ")))
  beeswarm::beeswarm(variable2, corral="wrap", corralWidth=1, pch=20, col="mediumseagreen", main = paste(c('Beeswarm de', deparse(substitute(variable2)), collapse = " ")))
  beeswarm::beeswarm(variable3, corral="wrap", corralWidth=1, pch=20, col="turquoise3", main = paste(c('Beeswarm de', deparse(substitute(variable3)), collapse = " ")))
  par(oma = c(5, 4, 4, 2) + 0.1, mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1, 1))
}

encontrar_outliers = function(variable){
  outliers = variable < quantile(variable, 0.25, type=1) - 1.5 * IQR(variable) | variable > quantile(variable, 0.75, type=1) + 1.5 * IQR(variable)
  return(variable[outliers])
}

muestra_sin_outliers = function(variable){
  sin_outliers = variable >= quantile(variable, 0.25, type=1) - 1.5 * IQR(variable) & variable <= quantile(variable, 0.75, type=1) + 1.5 * IQR(variable)
  return(variable[sin_outliers])
}

scatterplots = function(variable1, variable2, variable3){ # scatterplots
  data = data.frame(variable1, variable2, variable3)
  
  p1 = ggplot(data, aes(x = variable1, y = variable2)) +
    geom_point(color = "blue") +
    labs(title = paste('Scatter Plot entre', deparse(substitute(variable1)), 'y', deparse(substitute(variable2))),
         x = deparse(substitute(variable1)), y = deparse(substitute(variable2)))
  
  p2 = ggplot(data, aes(x = variable2, y = variable3)) +
    geom_point(color = "blue") +
    labs(title = paste('Scatter Plot entre', deparse(substitute(variable2)), 'y', deparse(substitute(variable3))),
         x = deparse(substitute(variable2)), y = deparse(substitute(variable3)))
  
  p3 = ggplot(data, aes(x = variable1, y = variable3)) +
    geom_point(color = "blue") +
    labs(title = paste('Scatter Plot entre', deparse(substitute(variable1)), 'y', deparse(substitute(variable3))),
         x = deparse(substitute(variable1)), y = deparse(substitute(variable3)))

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 2)))
  print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  print(p3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
}

f_muestral = function(x, x0){ # defino la funcion muestral F
  contador = sum(x <= x0)
  
  F_hat = contador/length(x) 
  return (F_hat)
}

cuantil_muestral = function(x, omega){ # funcion para encontrar el cuantil muestral con min, NO max
  x_temporal = sort(x)
  # sort(x, descending=False) --> ordena de menor a mayor, descending=False esta predeterminado
  i = 1
  
  while (f_muestral(x_temporal, x_temporal[i]) < omega){ 
    i = i + 1
  }
  # ordena los datos de menor a mayor y va calculando la prob. acumulada de cada valor de la muestra 
  # y comparando que sea menor a omega, hasta encontrar aquel con la probabilidad <= omega
  # devolviendote ese valor x_temporal[i]
  
  return (x_temporal[i])
}

mediana_hat_no_par = function(x){
  return (cuantil_muestral(x, 0.5))
}

mediana_hat_lnorm_MV = function(x){
  m_mv = mean(log(x))
  D2_mv = mean((log(x) - m_mv)^2)
  return (exp(m_mv + qnorm(0.5) * sqrt(D2_mv)))
}

mediana_hat_pareto_MV = function(x){
  theta_mv = min(x)
  beta_mv = length(x)/(sum(log(x/theta_mv)))
  return (qpareto(0.5, scale = theta_mv, shape = beta_mv))
}

estimar_mediana = function(x){
  # Estimacion no parametrica
  mediana_hat_np = mediana_hat_no_par(x)
  
  # Estimacion Log-normal MV
  mediana_hat_lnorm = mediana_hat_lnorm_MV(x)
  
  # Estimacion Pareto MV
  mediana_hat_pareto = mediana_hat_pareto_MV(x)
  
  estimadores = data.frame(
    Estimadores = c('No paramétrico', 'Pareto MV', 'Log-Normal MV'),
    Mediana_hat = c(mediana_hat_np, mediana_hat_pareto, mediana_hat_lnorm)
  )
  
  return (estimadores)
}

