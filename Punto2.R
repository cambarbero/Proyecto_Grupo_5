library(dplyr)

# 1) cargamos los datos

datos = read.csv("Food_Waste_data.csv") 

# 2) filtro las columnas que nos interesan

datos_filtrados = select(datos,Country,Household.estimate..tonnes.year.,Retail.estimate..tonnes.year.,Food.service.estimate..tonnes.year.,Confidence.in.estimate)

# 3) Defino las variables aleatorias

# X = Toneladas estimadas de desperdicio de comida de hogares por año del pais
# Y = Toneladas estimadas de desperdicio de comida de minoristas por año del pais
# Z = Toneladas estimadas de desperdicio de comida de servicios de comida por año del pais
# W = Estimacion de confianza

# ------- T2.1) ----------

# ------- La variable 'x' ----------

x = datos_filtrados$Household.estimate..tonnes.year. # v.a.

# --- a) ---
range(x) # --> minimo y el maximo de los datos (muestra)
plot(ecdf(x), main = 'Distribucion Empirica de X')
# --- b) ---
hist(x, freq = FALSE, col = 'purple') # histograma de frequencia relativa 
plot(density(x), col= 'blue', lwd=2) # Grafico de densidad

# Medidas de posicion
# --- c) ---
mean(x) # media
median(x) # mediana
# --- d) ---
quantile(x, 0.25, type=1) # cuantiles 0,25 y 0,75
quantile(x, 0.75, type=1)
IQR(x) # rango inter-cuartilico

# Boxplot
boxplot(x, main='Diagrama de caja y bigotes de X')

# Medidas de dispersion
# --- e) ---
sd(x) # desviacion estandar
sd(x)/mean(x) # coeficiente de variacion
mean(((x-mean(x))/sd(x))^3) # coeficiente de asimetria
mean(((x-mean(x))/sd(x))^4) # coeficiente de curtosis

# --- f) ---
outliers_x = x[x < quantile(x, 0.25, type=1) - 1.5 * IQR(x) | x > quantile(x, 0.75, type=1) + 1.5 * IQR(x)]

# ------- La variable 'y' ----------

y = datos_filtrados$Retail.estimate..tonnes.year. # v.a.

# --- a) ---
range(y) # --> minimo y el maximo de los datos (muestra)
plot(ecdf(y), main = 'Distribucion Empirica de Y')
# --- b) ---
hist(y, freq = FALSE, col = 'red') # histograma de frequencia relativa 
plot(density(y), col= 'orange', lwd=2) # Grafico de densidad

# Medidas de posicion
# --- c) ---
mean(y) # media
median(y) # mediana
# --- d) ---
quantile(y, 0.25, type=1) # cuantiles 0,25 y 0,75
quantile(y, 0.75, type=1)
IQR(y) # rango inter-cuartilico

# Boxplot
boxplot(y, main='Diagrama de caja y bigotes de Y')

# Medidas de dispersion
# --- e) ---
sd(y) # desviacion estandar
sd(y)/mean(y) # coeficiente de variacion
mean(((y-mean(y))/sd(y))^3) # coeficiente de asimetria
mean(((y-mean(y))/sd(y))^4) # coeficiente de curtosis

# --- f) ---
outliers_y = y[y < quantile(y, 0.25, type=1) - 1.5 * IQR(y) | y > quantile(y, 0.75, type=1) + 1.5 * IQR(y)]

# ------- La variable 'z' ----------

z = datos_filtrados$Food.service.estimate..tonnes.year. # v.a.

# --- a) ---
range(z) # --> minimo y el maximo de los datos (muestra)
plot(ecdf(z), main = 'Distribucion Empirica de Z')
# --- b) ---
hist(z, freq = FALSE, col = 'green') # histograma de frequencia relativa 
plot(density(z), col= 'cyan', lwd=2) # Grafico de densidad

# Medidas de posicion
# --- c) ---
mean(z) # media
median(z) # mediana
# --- d) ---
quantile(z, 0.25, type=1) # cuantiles 0,25 y 0,75
quantile(z, 0.75, type=1)
IQR(z) # rango inter-cuartilico

# Boxplot
boxplot(z, main='Diagrama de caja y bigotes de Z')

# Medidas de dispersion
# --- e) ---
sd(z) # desviacion estandar
sd(z)/mean(z) # coeficiente de variacion
mean(((z-mean(z))/sd(z))^3) # coeficiente de asimetria
mean(((z-mean(z))/sd(z))^4) # coeficiente de curtosis

# --- f) ---
outliers_z = z[z < quantile(z, 0.25, type=1) - 1.5 * IQR(z) | z > quantile(z, 0.75, type=1) + 1.5 * IQR(z)]

# ------- La variable 'w' ----------
w = datos_filtrados$Confidence.in.estimate # v.a.

# --- g) ---
# scatter plot entre v.a de a pares
plot(x, y, pch = 16, col = "blue", xlab = "X-axis", ylab = "Y-axis", main = "Scatter Plot")
plot(y, z, pch = 16, col = "blue", xlab = "Y-axis", ylab = "Z-axis", main = "Scatter Plot")
plot(x, z, pch = 16, col = "blue", xlab = "X-axis", ylab = "Z-axis", main = "Scatter Plot")

# --- h) ---

variables_num = select(datos_filtrados,Household.estimate..tonnes.year.,Retail.estimate..tonnes.year.,Food.service.estimate..tonnes.year.)
cor_matrix = cor(variables_num)

# ------- T2.4) ----------

datos_Very_Low_Confidence = filter(datos_filtrados, Confidence.in.estimate == "Very Low Confidence")
datos_Low_Confidence = filter(datos_filtrados, Confidence.in.estimate == "Low Confidence")
datos_High_Confidence = filter(datos_filtrados, Confidence.in.estimate == "High Confidence")
datos_Medium_Confidence = filter(datos_filtrados, Confidence.in.estimate == "Medium Confidence")

# Distribucion de X
hist(datos_filtrados$Household.estimate..tonnes.year., freq=FALSE, main='Histograma de distribucion de X')
lines(density(datos_filtrados$Household.estimate..tonnes.year.), lwd=3, col='maroon4')

# Distribucion de X en los diferentes niveles de confianza
plot(density(datos_Very_Low_Confidence$Household.estimate..tonnes.year.), lwd=3, main='Distribucion de X en distintos niveles de confianza', xlim = c(0,25000000),col='red')
lines(density(datos_Low_Confidence$Household.estimate..tonnes.year.), lwd=3, main='Distribucion de X en distintos niveles de confianza', xlim = c(0,25000000),col='orange')
lines(density(datos_High_Confidence$Household.estimate..tonnes.year.), lwd=3, main='Distribucion de X en distintos niveles de confianza', xlim = c(0,25000000),col='blue')
lines(density(datos_Medium_Confidence$Household.estimate..tonnes.year.), lwd=3, main='Distribucion de X en distintos niveles de confianza', xlim = c(0,25000000),col='green')



# hola
