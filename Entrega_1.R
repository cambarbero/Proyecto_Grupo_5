source('Funciones.R')

# 1) Carga de los datos

datos = read.csv("Food_Waste_data.csv") 

# 2) Filtrado de las columnas de interes

datos_filtrados = datos[, c(1, 4, 6, 8, 9)]
colnames(datos_filtrados) = c("Country", "HouseholdEstimate", "RetailEstimate", "FoodServiceEstimate", "ConfidenceInEstimate")

# 3) Definicion de las variables aleatorias

# Hogares = Cantidad estimada de desperdicio de comida de los hogares del país medida en toneladas en el año 2021.
# Minoristas = Cantidad estimada de desperdicio de comida de vendedores minoristas del país medida en toneladas en el año 2021.
# Servicios = Cantidad estimada de desperdicio de comida de servicios/proveedores de comida del país medida en toneladas en el año 2021.
# Confianza = Nivel de confianza establecido por las Naciones Unidas acerca de los datos cuantitativos proporcionados por el país.

# ------- T2.1) ----------

estadistica_descriptiva = data.frame(
  Variable = c(),
  Minimo = c(),
  Maximo = c(),
  Media = c(),
  Mediana = c(),
  Q1 = c(),
  Q3 = c(),
  IQR = c(),
  Desviacion_estandar = c(),
  Coeficiente_de_variacion = c(),
  Coeficiente_de_asimetria = c(),
  Coeficiente_de_curtosis = c()
)

# ------- La variable 'Hogares' ----------

Hogares = datos_filtrados$HouseholdEstimate # v.a. continua

nue_fila = analisis_descriptivo(Hogares)

estadistica_descriptiva = bind_rows(estadistica_descriptiva, nue_fila)

# ------- La variable 'Minoristas' ----------

Minoristas = datos_filtrados$RetailEstimate # v.a. continua

nue_fila = analisis_descriptivo(Minoristas)

estadistica_descriptiva = bind_rows(estadistica_descriptiva, nue_fila)

# ------- La variable 'Servicios' ----------

Servicios = datos_filtrados$FoodServiceEstimate # v.a. continua

nue_fila = analisis_descriptivo(Servicios)

estadistica_descriptiva = bind_rows(estadistica_descriptiva, nue_fila)

# ------- La variable 'Confianza' ----------

Confianza = datos_filtrados$ConfidenceInEstimate # v.a. categorica

estadistica_descriptiva # llamo a la variable

# Graficos de distribucion muestral
f_muestrales(Hogares, Minoristas, Servicios)

par(oma = c(0, 0, 0, 0), mar = c(3, 3, 3, 2), mfrow = c(2, 2))
plot(ecdf(Hogares), main = paste(c('Función de distribución empírica de', deparse(substitute(Hogares)), collapse = " ")), col = 'darkgreen')
plot(ecdf(Minoristas), main = paste(c('Función de distribución empírica de', deparse(substitute(Minoristas)), collapse = " ")), col = 'mediumseagreen')
plot(ecdf(Servicios), main = paste(c('Función de distribución empírica de', deparse(substitute(Servicios)), collapse = " ")), col = 'turquoise3')
par(oma = c(5, 4, 4, 2) + 0.1, mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1, 1))

# Graficos de densidad (por ser continuas)
f_densidades(Hogares, Minoristas, Servicios)

par(oma = c(0, 0, 0, 0), mar = c(3, 3, 3, 2), mfrow = c(2, 2))
plot(density(Hogares), main = paste(c('Función de densidad de', deparse(substitute(Hogares)), collapse = " ")), lwd=3, col = 'darkgreen')
plot(density(Minoristas), main = paste(c('Función de densidad de', deparse(substitute(Minoristas)), collapse = " ")), lwd=3, col = 'mediumseagreen')
plot(density(Servicios), main = paste(c('Función de densidad de', deparse(substitute(Servicios)), collapse = " ")), lwd=3, col = 'turquoise3')
par(oma = c(5, 4, 4, 2) + 0.1, mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1, 1))

# Graficos de boxplot

variables_num <- data.frame(
  Hogares,
  Minoristas,
  Servicios
)
ggplot(variables_num, aes(x = "Hogares", y = Hogares, fill = "Hogares")) +
  geom_boxplot(width = 0.4, alpha = 0.6) +
  labs(x=paste("Hogares")) +
  geom_boxplot(aes(x = "Minoristas", y = Minoristas, fill = "Minoristas"), width = 0.4, alpha = 0.6) +
  geom_boxplot(aes(x = "Servicios", y = Servicios, fill = "Servicios"), width = 0.4, alpha = 0.6) +
  scale_fill_manual(name = "Variables", values = c("Hogares" = "darkgreen", "Minoristas" = "mediumseagreen", "Servicios" = "turquoise3")) +
  scale_shape_manual(name = "Variables", values = c("Hogares" = 0, "Minoristas" = 1, "Servicios" = 2)) +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  xlab("") +
  ylab("Valor") +
  theme_classic() +
  ggtitle("Diagrama de caja y bigotes de Hogares, Minoristas y Servicios") +
  theme(plot.title = element_text(hjust = 0.5))

# Identificar outliers en los datos. ¿Deberian excluirse? ¿Como se modifican las medidas obtenidas anteriormente si se los excluye?
beeswarms(Hogares, Minoristas, Servicios)

# Buscar outliers y agregar al data frame
outliers_variables = list(encontrar_outliers(Hogares), encontrar_outliers(Minoristas), encontrar_outliers(Servicios))
estadistica_descriptiva$Outliers = outliers_variables

# Analisis descriptivo sin outliers
estadistica_descriptiva_sin_outliers = data.frame(
  Variable = c(),
  Minimo = c(),
  Maximo = c(),
  Media = c(),
  Mediana = c(),
  Q1 = c(),
  Q3 = c(),
  IQR = c(),
  Desviacion_estandar = c(),
  Coeficiente_de_variacion = c(),
  Coeficiente_de_asimetria = c(),
  Coeficiente_de_curtosis = c()
)

Hogares_sin_outliers = muestra_sin_outliers(Hogares)
Minoristas_sin_outliers = muestra_sin_outliers(Minoristas)
Servicios_sin_outliers = muestra_sin_outliers(Servicios)

nue_fila = analisis_descriptivo(Hogares_sin_outliers)
estadistica_descriptiva_sin_outliers = bind_rows(estadistica_descriptiva_sin_outliers, nue_fila)

nue_fila = analisis_descriptivo(Minoristas_sin_outliers)
estadistica_descriptiva_sin_outliers = bind_rows(estadistica_descriptiva_sin_outliers, nue_fila)

nue_fila = analisis_descriptivo(Servicios_sin_outliers)
estadistica_descriptiva_sin_outliers = bind_rows(estadistica_descriptiva_sin_outliers, nue_fila)

f_densidades(Servicios, Servicios_sin_outliers)

# Scatter Plots
scatterplots(Hogares, Minoristas, Servicios)

# Matriz de correlacion entre las variables
variables_num = select(variables_num, Hogares, Minoristas, Servicios)
cor_matrix = cor(variables_num)

# ------- T2.4) ----------

datos_Very_Low_Confidence = filter(datos_filtrados, ConfidenceInEstimate == "Very Low Confidence")
datos_Low_Confidence = filter(datos_filtrados, ConfidenceInEstimate == "Low Confidence")
datos_High_Confidence = filter(datos_filtrados, ConfidenceInEstimate == "High Confidence")
datos_Medium_Confidence = filter(datos_filtrados, ConfidenceInEstimate == "Medium Confidence")

par(oma = c(0, 0, 0, 0), mar = c(3, 3, 1, 2), mfrow = c(2, 1))
# Distribucion de Servicios
plot(density(datos_filtrados$HouseholdEstimate), main = 'Distribucion de Servicios', xlim = c(0, 25000000), lwd = 3, col = 'turquoise3', xlab = "", ylab = "", mgp = c(3, 1, 0))

# Distribucion de Servicios en los distintos niveles de confianza
plot(density(datos_Very_Low_Confidence$HouseholdEstimate), lwd = 3, xlim = c(0, 25000000), main = 'Distribucion de Servicios en distintos niveles de confianza', col = "#00CED1", xlab = "", ylab = "", mgp = c(3, 1, 0))
lines(density(datos_Low_Confidence$HouseholdEstimate), lwd = 3, xlim = c(0, 25000000), col = 'plum')
lines(density(datos_High_Confidence$HouseholdEstimate), lwd = 3, xlim = c(0, 25000000), col = "#1E90FF")
lines(density(datos_Medium_Confidence$HouseholdEstimate), lwd = 3, xlim = c(0, 25000000), col = "#6B8E23")

legend("topright", legend = c("Very Low Confidence", "Low Confidence", "High Confidence", "Medium Confidence"), col = c("#00CED1", "plum", "#1E90FF", "#6B8E23"), lwd = 3)
par(mfrow = c(1, 1))
