rm(list = ls()) # Limpia el environment

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(psych)
library(readxl)

# Cargar el dataset
dataset <- read.csv("C:\\Users\\Station\\Music\\PROYECTO PROBABILIDAD\\Streaming_dataset_for_R.csv")

# Renombrar columnas para evitar caracteres especiales
names(dataset) <- c("Plataforma", 
                    "Suscriptores_millones", 
                    "Tiempo_Usado_horas", 
                    "Ingresos_Anuales_millones", 
                    "Numero_de_Peliculas", 
                    "Numero_de_Series", 
                    "Calificacion_Promedio")

# Vista rápida de los datos
print(head(dataset))

# Medidas de tendencia central
medias <- dataset %>% summarise_if(is.numeric, mean, na.rm = TRUE)
medianas <- dataset %>% summarise_if(is.numeric, median, na.rm = TRUE)
modas <- dataset %>% summarise_if(is.numeric, ~names(which.max(table(.))))

print("Medias:")
print(medias)

print("Medianas:")
print(medianas)

print("Modas:")
print(modas)

# Medidas de dispersión
desviaciones <- dataset %>% summarise_if(is.numeric, sd, na.rm = TRUE)
print("Desviaciones estándar:")
print(desviaciones)

# Tablas de frecuencia (ejemplo con "Plataforma")
tabla_frecuencia <- table(dataset$Plataforma)
print("Tabla de frecuencia:")
print(tabla_frecuencia)

# Gráficos
# Histograma de Suscriptores
histograma <- ggplot(dataset, aes(x = Suscriptores_millones)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Distribución de Suscriptores", 
       x = "Suscriptores (millones)", 
       y = "Frecuencia")

# Mostrar histograma
print(histograma)

# Diagrama de caja para Calificación Promedio
boxplot <- ggplot(dataset, aes(y = Calificacion_Promedio, x = Plataforma)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Distribución de Calificaciones por Plataforma", 
       x = "Plataforma", 
       y = "Calificación Promedio (/5)")

# Mostrar boxplot
print(boxplot)

# Guardar los gráficos
ggsave("histograma_suscriptores.png", histograma, width = 8, height = 6)
ggsave("boxplot_calificaciones.png", boxplot, width = 8, height = 6)
