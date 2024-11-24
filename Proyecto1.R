
#importamos el data set
library(readr)
Streaming_dataset <- read_csv("C:\\Users\\Station\\Music\\PROYECTO PROBABILIDAD\\Streaming_dataset.csv")
View(Streaming_dataset)
#suscriptores

sus = Streaming_dataset$`Suscriptores (millones)`#suscriptores

#medidas de tendencia central de la variable suscriptores
media_suciptores = mean(sus)
mediana_suscriptores = median(sus)

moda = table(sus)

#medidas de dispercion de la variable suscriptores
rango_suscriptores = range(sus)
varianza_suscriptores = var(sus) 
desviacionEs_suscriptores = sd(sus)
Cv_suscriptores = desviacionEs_suscriptores/media_suciptores

#medidad de posicion de la variable suscriptores
quantile(sus, probs = seq(0, 1, 0.25))#cuartil 0.25
quantile(sus, probs = seq(0, 1, 0.2))#quintil 0.2
quantile(sus, probs = seq(0, 1, 0.1))#decil 0.1
quantile(sus, probs = seq(0, 1, 0.01))#percentil 0.01

#tabla de frecuencia e histograma
Amplitud_suscriptores = max(sus)-min(sus)
n = length(sus)
ka = sqrt(n)
st=trunc(1+3.33*log10(n))
Aint = Amplitud_suscriptores/ka

#install.packages("agricolae")
library("agricolae")
b=c()
for(i in 0:10){
  b[i]=1.97+Aint*i 
}

hist_suscriptores = hist(sus, xlim = c(0, 290), ylim = c(0, 15),xlab = "Suscriptores", ylab = "Frecuencia", 
                          main = "Numero de suscriptores (mill)",col = "lightblue", border = "white",
                          axes = FALSE, breaks = c(1.97, b))

axis(1, at = seq(0, 290, by = 10), las = 1)
axis(2, at = seq(0, 15, by = 5), las = 2)

midpoints <- (hist_suscriptores$breaks[-1] + hist_suscriptores$breaks[-length(hist_suscriptores$breaks)]) / 2
lines(midpoints, hist_suscriptores$counts, type = "b", col = "red", pch = 16, lwd = 2)
###############################
#Tiempo en horas
# Ordenar primero por 'Plataforma' y luego por 'Tiempo.Usado..horas.'
tiempo_horas_ordenado = tiempo_horas[order(tiempo_horas$Plataforma, tiempo_horas$Tiempo.Usado..horas.), ]

#Medidas de tendencia central
media_tiempo_horas = mean(tiempo_horas_ordenado$Tiempo.Usado..horas.)
mediana_tiempo_horas = median(tiempo_horas_ordenado$Tiempo.Usado..horas.)
moda_tiempo_horas = table(tiempo_horas_ordenado$Tiempo.Usado..horas.)

#Medidas de dispercion
rango_tiempo_horas = range(tiempo_horas_ordenado$Tiempo.Usado..horas.)
varianza_tiempo_horas= var(tiempo_horas_ordenado$Tiempo.Usado..horas.)
desviacion_estandar_tiempo_horas= sd(tiempo_horas_ordenado$Tiempo.Usado..horas.)
cv_tiempo_horas= desviacion_estandar_tiempo_horas/media_tiempo_horas

#Medidas de posicion
quartil = quantile(tiempo_horas_ordenado$Tiempo.Usado..horas., probs = seq(0, 1, 0.25))
quintil = quantile(tiempo_horas_ordenado$Tiempo.Usado..horas., probs = seq(0, 1, 0.2))
decil= quantile(tiempo_horas_ordenado$Tiempo.Usado..horas., probs = seq(0, 1, 0.1))
percentil= quantile(tiempo_horas_ordenado$Tiempo.Usado..horas., probs = seq(0, 1, 0.01))

#Graficos
# Tabla de frecuencia e histograma
Amplitud = max(tiempo_horas_ordenado$Tiempo.Usado..horas.) - min(tiempo_horas_ordenado$Tiempo.Usado..horas.)
n= length(tiempo_horas_ordenado$Tiempo.Usado..horas.)
ka= sqrt(n) #Kaiser
st= trunc(1 + 3.33 * log10(n)) #Sturges
Ancho_intervalos= Amplitud / ka #Ancho intervalos

# Cálculo de intervalos
intervalos = c()
for (i in 0:10) {
  intervalos[i] = 7.28 + Ancho_intervalos * i
}

# Histograma
hist_tiempo_horas = hist(tiempo_horas_ordenado$Tiempo.Usado..horas., breaks =c(7.28,intervalos),xlim = c(0, 500), 
                         ylim = c(0, 20),xlab = "Horas", ylab = "Frecuencia", 
                         main = "Numero de Horas",col = "orange", border = "white",
                         axes = FALSE)

axis(1, at = seq(0, 500, by = 10), las = 1)
axis(2, at = seq(0, 20, by = 2), las = 2)

puntosme = (hist_tiempo_horas$breaks[-1] + hist_tiempo_horas$breaks[-length(hist_tiempo_horas$breaks)]) / 2
lines(puntosme, hist_tiempo_horas$counts, type = "b",col = "blue", pch = 16, lwd = 2)
points(puntosme, hist_tiempo_horas$counts, col = "black", pch = 16)

# Diagrama de caja
caja_tiempo_horas = boxplot(tiempo_horas_ordenado$Tiempo.Usado..horas., main = "Diagrama de Caja de Tiempo (Horas)", 
                            ylab = "Tiempo (Horas)", col = "lightgreen")
#################################
#Ingreso
# Extraemos la variable "Ingresos (millones)"
ingresos <- Streaming_dataset$`Ingresos Anuales (millones)`

# Medidas de tendencia central de la variable "Ingresos (millones)"
media_ingresos <- mean(ingresos)
mediana_ingresos <- median(ingresos)
moda_ingresos <- table(ingresos)

# Medidas de dispersión de la variable "Ingresos (millones)"
rango_ingresos <- range(ingresos)
varianza_ingresos <- var(ingresos)
desviacionEs_ingresos <- sd(ingresos)
  Cv_ingresos <- desviacionEs_ingresos / media_ingresos

# Medidas de posición de la variable "Ingresos (millones)"
cuartiles_ingresos <- quantile(ingresos, probs = seq(0, 1, 0.25)) # Cuartiles
quintiles_ingresos <- quantile(ingresos, probs = seq(0, 1, 0.2))  # Quintiles
deciles_ingresos <- quantile(ingresos, probs = seq(0, 1, 0.1))    # Deciles
percentiles_ingresos <- quantile(ingresos, probs = seq(0, 1, 0.01)) # Percentiles

# Tabla de frecuencia e histograma
Amplitud_ingresos <- max(ingresos) - min(ingresos)
n <- length(ingresos)
ka <- sqrt(n)
st <- trunc(1 + 3.33 * log10(n))
Aint <- Amplitud_ingresos / ka

# Cálculo de intervalos de clase
intervalos <- numeric(10)
for (i in 0:10) {
  intervalos[i + 1] <- min(ingresos) + Aint * i
}

# Histograma
hist_ingresos = hist(ingresos, breaks = intervalos, col = "lightgreen", main = "Histograma de Ingresos (millones)", 
                     xlab = "Ingresos (millones)", ylab = "Frecuencia",xlim = c(0, 20000), ylim = c(0, 16),
                     border = "white",axes = FALSE)

axis(1, at = seq(0, 20000, by = 1000), las = 1)
axis(2, at = seq(0, 16, by = 2), las = 2)
# Polígono de frecuencias
puntosme <- (hist_ingresos$breaks[-1] + hist_ingresos$breaks[-length(hist_ingresos$breaks)]) / 2
lines(puntosme, hist_ingresos$counts, type = "b", col = "blue", pch = 16, lwd = 2)
points(puntosme, hist_ingresos$counts, col = "black", pch = 16)
# Diagrama de caja
caja_ingresos <- boxplot(ingresos, main = "Diagrama de Caja de Ingresos (millones)", 
                         ylab = "Ingresos (millones)", col = "lightgreen")
# Diagrama de caja
caja_tiempo_horas = boxplot(tiempo, main = "Diagrama de Caja de Tiempo (Horas)", 
                            ylab = "Tiempo (Horas)", col = "orange")
################################################################################################333
#Numero de pelis
#Se selecciona la varibale de peliculas
num_peliculas=Streaming_dataset$'Número de Películas'
stem(num_peliculas)

#medidas de tendencia central
media_peliculas = mean(num_peliculas)
mediana_pelciulas = median(num_peliculas)
moda_peliculas= table(num_peliculas)

#medidas de dispercion de la variable num_peliculas
rango_num_peliculas= range(num_peliculas)
varianza_num_peliculas= var(num_peliculas)
desviacionES_num_peliculas=sd(num_peliculas)
CV_num_peliculas=desviacionES_num_peliculas/media_peliculas

#MEDIDA DE POSICION DE LA VARIABLE NUM_PELICULAS
cuartil=quantile(num_peliculas, probs = seq(0,1,0.25)) #cuartil 0.25
quantil=quantile(num_peliculas, probs = seq(0,1,0.20)) #quantil 0.2
decil=quantile(num_peliculas, probs = seq(0,1,0.10)) #decil 0.1
percentil=quantile(num_peliculas, probs = seq(0,1,0.01)) #percentil 0.01

summary(num_peliculas)

#TABLA DE FRECUENCIAS e HISTOGRAMA
Amplitud_num_peliculas = max(num_peliculas)-min(num_peliculas)

n= length(num_peliculas)
ka = sqrt(n)
st=trunc(1+3.33*log10(n))
Aint=Amplitud_num_peliculas/ka


library("agricolae")
b=c()
for(i in 0:10){
  b[i]=81+Aint*i
}
sort(num_peliculas)
hist_num_peliculas= hist(num_peliculas, breaks = c(81,b), col = "lightyellow", main = "Histograma de Numero de peliculas", 
                         xlab = "Numero de peliculas", ylab = "Frecuencia",xlim = c(0, 5000), ylim = c(0, 20),
                         border = "black",axes = FALSE)
axis(1, at = seq(0, 5000, by = 100), las = 1)
axis(2, at = seq(0, 20, by = 2), las = 2)


puntosme = (hist_num_peliculas$breaks[-1] + hist_num_peliculas$breaks[-length(hist_num_peliculas$breaks)])/2

lines(puntosme,hist_num_peliculas$counts,type="b",col="red",pch=16,lwd=2)
points(puntosme, hist_num_peliculas$counts, col = "black", pch = 16)

caja_num_peliculas=boxplot(num_peliculas)
##############################################################################3
#numero de series
# Extraemos la variable "Número de series"
numero_series <- Streaming_dataset$`Número de Series`
stem(numero_series)


# Medidas de tendencia central de la variable "Número de series"
media_series <- mean(numero_series)
mediana_series <- median(numero_series)
moda_series <- table(numero_series)

# Medidas de dispersión de la variable "Número de series"
rango_series <- range(numero_series)
varianza_series <- var(numero_series)
desviacionEs_series <- sd(numero_series)
Cv_series <- desviacionEs_series / media_series

# Medidas de posición de la variable "Número de series"
cuartiles_series <- quantile(numero_series, probs = seq(0, 1, 0.25)) # Cuartiles
quintiles_series <- quantile(numero_series, probs = seq(0, 1, 0.2))  # Quintiles
deciles_series <- quantile(numero_series, probs = seq(0, 1, 0.1))    # Deciles
percentiles_series <- quantile(numero_series, probs = seq(0, 1, 0.01)) # Percentiles

# Tabla de frecuencia e histograma
Amplitud_series <- max(numero_series) - min(numero_series)
n_series <- length(numero_series)
ka_series <- sqrt(n_series)
st_series <- trunc(1 + 3.33 * log10(n_series))
Aint_series <- Amplitud_series / ka_series

# Cálculo de intervalos de clase
intervalos_series <- numeric(10)
for (i in 0:10) {
  intervalos_series[i + 1] <- min(numero_series) + Aint_series * i
}

# Histograma
hist_series <- hist(numero_series, breaks = intervalos_series, col = "pink",main = "Histograma de Número de Series", 
                    xlab = "Número de Series", ylab = "Frecuencia",xlim = c(0, 1000), ylim = c(0, 16),
                    border = "white",axes = FALSE)
axis(1, at = seq(0, 1000, by = 10), las = 1)
axis(2, at = seq(0, 16, by = 2), las = 2)
# Polígono de frecuencias
puntosme_series <- (hist_series$breaks[-1] + hist_series$breaks[-length(hist_series$breaks)]) / 2
lines(puntosme_series, hist_series$counts, type = "b",col = "green", pch = 16, lwd = 2)
points(puntosme_series, hist_series$counts, col = "black", pch = 16)

# Diagrama de caja
caja_series <- boxplot(numero_series, main = "Diagrama de Caja de Número de Series", 
                       ylab = "Número de Series", col = "lightgreen")
######################################################################################3
#calificacion
#Selecionamos la variable de calificacion promedio
cal_prom <- Streaming_dataset$`Calificación Promedio /5`

#medidas de tendencia central de la Calificación Promedio
media_cal_prom = mean(cal_prom)
mediana_cal_prom = median(cal_prom)
moda_cal_prom = table(cal_prom)

#medidas de dispercion de la Calificación Promedio
rango_cal_prom = range(cal_prom)
varianza_cal_prom = var(cal_prom) 
desviacionEs_cal_prom = sd(cal_prom)
Cv_cal_prom = desviacionEs_cal_prom/media_cal_prom

#medidad de posicion de la Calificación Promedio
cuartiles_cal_prom = quantile(cal_prom, probs = seq(0, 1, 0.25))#cuartil 0.25
quintiles_cal_prom = quantile(cal_prom, probs = seq(0, 1, 0.2))#quintil 0.2
deciles_cal_prom = quantile(cal_prom, probs = seq(0, 1, 0.1))#decil 0.1
percentiles_cal_prom = quantile(cal_prom, probs = seq(0, 1, 0.01))#percentil 0.01

summary(cal_prom)
#tabla de frecuencia e histograma
Amplitud_cal_prom = max(cal_prom)-min(cal_prom)
n = length(cal_prom)
ka = sqrt(n)
st=trunc(1+3.33*log10(n))
Aint = Amplitud_cal_prom/ka

install.packages("agricolae")
library("agricolae")

b=c()
for (i in 0:10) {  
  b[i] = 1.04 + Aint * i  
}

hist_cal_prom = hist(cal_prom, breaks = c(1.04, b),col = "purple",main = "Histograma de califaciones /5", 
                     xlab = "Calificacion", ylab = "Frecuencia",xlim = c(1.04, 5), ylim = c(0, 16),
                     border = "white",axes = FALSE)
axis(1, at = seq(1.04, 5, by = 0.394), las = 1)
axis(2, at = seq(0, 16, by = 2), las = 2)

puntosme <- (hist_cal_prom$breaks[-1] + hist_cal_prom$breaks[-length(hist_cal_prom$breaks)]) / 2
lines(puntosme, hist_cal_prom$counts, type = "b",col = "orange", pch = 16, lwd = 2)
points(puntosme, hist_cal_prom$counts, col = "black", pch = 16)

caja_cal_prom = boxplot(cal_prom)
###############################3
#diagramas de caja y bigotes conjunto
boxplot(sus, h,ns,
        names = c("Suscip", "Horas","Series"),
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Diagramas de caja y bigotes",
        xlab = "Grupos", ylab = "Valores")


