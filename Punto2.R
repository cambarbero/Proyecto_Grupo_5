library(dplyr)

# 1) cargamos los datos

datos = read.csv("Food Waste data.csv") 

# 2) filtro las columnas que nos interesan

datos_filtrados = select(datos,Country,Household.estimate..tonnes.year.,Retail.estimate..tonnes.year.,Food.service.estimate..tonnes.year.,Confidence.in.estimate)

# 3) Defino las variables aleatorias

# X = Toneladas estimadas de desperdicio de comida de hogares por año del pais
# Y = Toneladas estimadas de desperdicio de comida de minoristas por año del pais
# Z = Toneladas estimadas de desperdicio de comida de servicios de comida por año del pais
# W = Region del pais

# ------- La variable 'x' ----------

x = datos_filtrados$Household.estimate..tonnes.year. # v.a.
range(x) # --> minimo y el maximo de los datos (muestra)
plot(ecdf(x), main = 'Distribucion Empirica de X')
hist(x, freq = FALSE, col = 'purple') # histograma de frequencia relativa 
plot(density(x), col= 'blue', lwd=2) # Grafico de densidad

# Medidas de posicion
mean(x) # media
median(x) # mediana
quantile(x, 0.25, type=1) # cuantiles 0,25 y 0,75
quantile(x, 0.75, type=1)
IQR(x) # rango inter-cuartilico

# Boxplot
boxplot(x)

# Medidas de dispersion
sd(x) # desviacion estandar
sd(x)/mean(x) # coeficiente de variacion
mean(((x-mean(x))/sd(x))^3) # coeficiente de asimetria
mean(((x-mean(x))/sd(x))^4) # coeficiente de curtosis
lower_bound = quantile(x, 0.25, type=1) - 1.5 * IQR(x)
upper_bound = quantile(x, 0.75, type=1) + 1.5 * IQR(x)
which(x < lower_bound | x > upper_bound) # outliers
# PREGUNTAR OUTLIERS


# ------- La variable 'y' ----------

y = datos_filtrados$Retail.estimate..tonnes.year. # v.a.
range(y) # --> minimo y el maximo de los datos (muestra)
plot(ecdf(y), main = 'Distribucion Empirica de Y')
hist(y, freq = FALSE, col = 'red') # histograma de frequencia relativa 
plot(density(y), col= 'orange', lwd=2) # Grafico de densidad

# Medidas de posicion
mean(y) # media
median(y) # mediana
quantile(y, 0.25, type=1) # cuantiles 0,25 y 0,75
quantile(y, 0.75, type=1)
IQR(y) # rango inter-cuartilico

# Boxplot
boxplot(y)

# Medidas de dispersion
sd(y) # desviacion estandar
sd(y)/mean(y) # coeficiente de variacion
mean(((y-mean(y))/sd(y))^3) # coeficiente de asimetria
mean(((y-mean(y))/sd(y))^4) # coeficiente de curtosis
lower_bound = quantile(y, 0.25, type=1) - 1.5 * IQR(y)
upper_bound = quantile(y, 0.75, type=1) + 1.5 * IQR(y)
which(y < lower_bound | y > upper_bound) # outliers
# PREGUNTAR OUTLIERS

# ------- La variable 'z' ----------

z = datos_filtrados$Food.service.estimate..tonnes.year. # v.a.
range(z) # --> minimo y el maximo de los datos (muestra)
plot(ecdf(z), main = 'Distribucion Empirica de Z')
hist(z, freq = FALSE, col = 'green') # histograma de frequencia relativa 
plot(density(z), col= 'cyan', lwd=2) # Grafico de densidad

# Medidas de posicion
mean(z) # media
median(z) # mediana
quantile(z, 0.25, type=1) # cuantiles 0,25 y 0,75
quantile(z, 0.75, type=1)
IQR(z) # rango inter-cuartilico

# Boxplot
boxplot(z)

# Medidas de dispersion
sd(z) # desviacion estandar
sd(z)/mean(z) # coeficiente de variacion
mean(((z-mean(z))/sd(z))^3) # coeficiente de asimetria
mean(((z-mean(z))/sd(z))^4) # coeficiente de curtosis
lower_bound = quantile(z, 0.25, type=1) - 1.5 * IQR(z)
upper_bound = quantile(z, 0.75, type=1) + 1.5 * IQR(z)
which(z < lower_bound | z > upper_bound) # outliers
# PREGUNTAR OUTLIERS (muy ache)

# ------- La variable 'w' ----------
w = datos_filtrados$Confidence.in.estimate # v.a.

# scatter plot entre v.a de a pares
plot(x, y, pch = 16, col = "blue", xlab = "X-axis", ylab = "Y-axis", main = "Scatter Plot")
plot(y, z, pch = 16, col = "blue", xlab = "Y-axis", ylab = "Z-axis", main = "Scatter Plot")
plot(x, z, pch = 16, col = "blue", xlab = "X-axis", ylab = "Z-axis", main = "Scatter Plot")

datos_Very_Low_Confidence = filter(datos_filtrados, Confidence.in.estimate == "Very Low Confidence")
datos_Low_Confidence = filter(datos_filtrados, Confidence.in.estimate == "Low Confidence")
datos_High_Confidence = filter(datos_filtrados, Confidence.in.estimate == "High Confidence")
datos_Medium_Confidence = filter(datos_filtrados, Confidence.in.estimate == "Medium Confidence")

# Distribucion de X
hist(datos_filtrados$Household.estimate..tonnes.year., freq=FALSE)
lines(density(datos_filtrados$Household.estimate..tonnes.year.), lwd=3, col='maroon4')

# Distribucion de X en los diferentes niveles de confianza
plot(density(datos_Very_Low_Confidence$Household.estimate..tonnes.year.), lwd=3, main='Distribucion de X en distintos niveles de confianza', xlim = c(0,25000000),col='magenta')
lines(density(datos_Low_Confidence$Household.estimate..tonnes.year.), lwd=3, main='Distribucion de X en distintos niveles de confianza', xlim = c(0,25000000),col='steelblue')
lines(density(datos_High_Confidence$Household.estimate..tonnes.year.), lwd=3, main='Distribucion de X en distintos niveles de confianza', xlim = c(0,25000000),col='green')
lines(density(datos_Medium_Confidence$Household.estimate..tonnes.year.), lwd=3, main='Distribucion de X en distintos niveles de confianza', xlim = c(0,25000000),col='orange')
