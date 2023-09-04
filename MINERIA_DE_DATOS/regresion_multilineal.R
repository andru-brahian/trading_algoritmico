

# ACTIVIDAD 1

url <- "https://raw.githubusercontent.com/andru-brahian/trading_algoritmico/main/usados_regresion.csv"
#url <- "https://raw.githubusercontent.com/andru-brahian/trading_algoritmico/main/df_caja.csv"


# Carga de paquetes necesarios
library(ggplot2)

# Carga del conjunto de datos desde el archivo CSV (ajusta la ruta según tu ubicación)
base_datos <- read.csv((url), sep = ";")

# Reemplazar las comas por puntos y convertir a numeric (equivalente a float)
base_datos$Close_AAPL <- as.numeric(gsub(",", ".", base_datos$Close_AAPL))
base_datos$Volume_AAPL <- as.numeric(gsub(",", ".", base_datos$Volume_AAPL))

# trabajar con la columna Close_AAPL como valores float
is.numeric(base_datos$Close_AAPL)
is.numeric(base_datos$Volume_AAPL)
#NA
anyNA(base_datos$Close_AAPL)
anyNA(base_datos$Volume_AAPL)

# Reemplazar las comas por puntos y convertir a numeric (equivalente a float) para Close_COIN
base_datos$Close_COIN <- as.numeric(gsub(",", ".", base_datos$Close_COIN))
base_datos$Volume_COIN <- as.numeric(gsub(",", ".", base_datos$Volume_COIN))

# Trabajar con la columna Close_COIN como valores float
is.numeric(base_datos$Close_COIN)
is.numeric(base_datos$Volume_COIN)

# Verificar si hay valores NA en Close_COIN y Volume_COIN
anyNA(base_datos$Close_COIN)
anyNA(base_datos$Volume_COIN)

# Reemplazar las comas por puntos y convertir a numeric (equivalente a float) para Close_GEHC
base_datos$Close_GEHC <- as.numeric(gsub(",", ".", base_datos$Close_GEHC))
base_datos$Volume_GEHC <- as.numeric(gsub(",", ".", base_datos$Volume_GEHC))

# Trabajar con la columna Close_GEHC como valores float
is.numeric(base_datos$Close_GEHC)
is.numeric(base_datos$Volume_GEHC)

# Verificar si hay valores NA en Close_GEHC y Volume_GEHC
anyNA(base_datos$Close_GEHC)
anyNA(base_datos$Volume_GEHC)


# Cargar las bibliotecas necesarias
install.packages("dplyr")
library(dplyr)

# Asegurarse de que la base de datos esté ordenada por fecha (si aún no lo está)
base_datos <- base_datos %>% arrange(Date)

# Calcular los rendimientos diarios para cada acción
base_datos <- base_datos %>%
  mutate(
    Rendimiento_AAPL = (Close_AAPL - lag(Close_AAPL)) / lag(Close_AAPL),
    Rendimiento_COIN = (Close_COIN - lag(Close_COIN)) / lag(Close_COIN),
    Rendimiento_GEHC = (Close_GEHC - lag(Close_GEHC)) / lag(Close_GEHC)
  )

# Definir los pesos del portafolio (ajusta los pesos según tus preferencias)
pesos <- c(0.4, 0.3, 0.3)  # Por ejemplo, 40% en Close_AAPL, 30% en Close_COIN y 30% en Close_GEHC

# Calcular el rendimiento diario del portafolio
base_datos$Rendimiento_Portafolio <- rowSums(base_datos[, c("Rendimiento_AAPL", "Rendimiento_COIN", "Rendimiento_GEHC")] * pesos)

# Ajustar un modelo de regresión múltiple
modelo <- lm(Rendimiento_Portafolio ~ Rendimiento_AAPL + Rendimiento_COIN + Rendimiento_GEHC, data = base_datos)

# Ver un resumen del modelo
summary(modelo)