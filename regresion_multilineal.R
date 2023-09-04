

# ACTIVIDAD 1

url <- "https://raw.githubusercontent.com/andru-brahian/trading_algoritmico/main/usados_regresion.csv"
#url <- "https://raw.githubusercontent.com/andru-brahian/trading_algoritmico/main/df_caja.csv"


# Carga de paquetes necesarios
library(ggplot2)

# Carga del conjunto de datos desde el archivo CSV (ajusta la ruta según tu ubicación)
base_datos <- read.csv((url), sep = ";")

base_datos <- na.omit(base_datos)

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
#install.packages("dplyr")
library(dplyr)

# Asegurarse de que la base de datos esté ordenada por fecha (si aún no lo está)
base_datos <- base_datos %>% arrange(new_date)

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


# Cargar la biblioteca necesaria
library(ggplot2)


# Añadir un valor NA al final del vector de predicciones
base_datos$Prediccion_Portafolio <- c(predict(modelo), NA)

print(base_datos$Prediccion_Portafolio)


# Gráfico de rendimientos diarios y predicción
ggplot(base_datos, aes(x = Rendimiento_AAPL, y = Rendimiento_Portafolio)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
  geom_smooth(aes(y = Prediccion_Portafolio), color = "red", linetype = "dashed") +
  labs(title = "Rendimiento Portafolio vs. Rendimiento AAPL con Predicción",
       x = "Rendimiento AAPL",
       y = "Rendimiento Portafolio") +
  scale_color_manual(values = c("purple" = "purple", "blue" = "blue", "red" = "red")) +
  theme_minimal()



# Crear una matriz de gráficos de dispersión
matriz_correlacion_multidimensional <- pairs(base_datos[c("Rendimiento_AAPL", "Rendimiento_COIN", "Rendimiento_GEHC", "Rendimiento_Portafolio")])
matriz_correlacion_multidimensional

#DESCARGAR IMÁGEN
# Especifica la ruta completa donde deseas guardar el archivo .png
#ruta_guardado <- "C:/Users/USUARIO/Documents/1 univalle/0 1 VIU/2_MODULO_2/6MBDI, ESTADISTICA AVANZADA/ACTIVIDAD 1/MINERIA_DE_DATOS/matriz_correlacion_multidimensional.png"

# Utiliza ggsave para guardar el gráfico en la ubicación especificada
#ggsave(filename = ruta_guardado, plot = matriz_correlacion_multidimensional, device = "png", bg = "white")
#matriz_correlacion_multidimensional


# Crear una matriz de gráficos de dispersión
#plot(base_datos[c("Rendimiento_AAPL", "Rendimiento_COIN", "Rendimiento_GEHC", "Rendimiento_Portafolio")])

# Agregar una línea de predicción a cada gráfico de dispersión
#for (i in 1:3) {
#  lines(x = base_datos[, i], y = base_datos$Prediccion_Portafolio, col = "red")
#}






# INTENTOS 3D

# Instalar y cargar la biblioteca rgl
#install.packages("rgl")
library(rgl)
install.packages("plotly")
# Cargar el paquete plotly
library(plotly)




# Crear un conjunto de datos de ejemplo con tus variables
Rendimiento_AAPL <- rnorm(100)
Rendimiento_COIN <- rnorm(100)
Rendimiento_Portafolio <- rnorm(100)

# Ajustar un modelo de regresión múltiple
modelo <- lm(Rendimiento_Portafolio ~ Rendimiento_AAPL + Rendimiento_COIN)

# Crear un data frame con los datos
data <- data.frame(
  Rendimiento_AAPL = Rendimiento_AAPL,
  Rendimiento_COIN = Rendimiento_COIN,
  Rendimiento_Portafolio = Rendimiento_Portafolio
)

# Crear el gráfico de dispersión 3D
fig <- plot_ly(data, x = ~Rendimiento_AAPL, y = ~Rendimiento_COIN, z = ~Rendimiento_Portafolio, 
               type = "scatter3d", mode = "markers", marker = list(size = 5, color = ~Rendimiento_Portafolio))

# Obtener los coeficientes del modelo de regresión
coeficientes <- coef(modelo)

# Crear una línea de regresión utilizando los coeficientes
x <- seq(min(data$Rendimiento_AAPL), max(data$Rendimiento_AAPL), length.out = 100)
y <- seq(min(data$Rendimiento_COIN), max(data$Rendimiento_COIN), length.out = 100)
superficie <- outer(x, y, function(x, y) coeficientes[1] + coeficientes[2] * x + coeficientes[3] * y)

# Agregar la línea de regresión al gráfico
fig <- fig %>% add_surface(z = ~superficie, colorscale = "Viridis")

# Mostrar el gráfico
fig






















# Crear un conjunto de datos de ejemplo con tus variables
Rendimiento_AAPL <- rnorm(100)
Rendimiento_COIN <- rnorm(100)
Rendimiento_GEHC <- rnorm(100)
Rendimiento_Portafolio <- rnorm(100)

# Ajustar un modelo de regresión múltiple
modelo <- lm(Rendimiento_Portafolio ~ Rendimiento_AAPL + Rendimiento_COIN + Rendimiento_GEHC)

# Crear un data frame con los datos
data <- data.frame(
  Rendimiento_AAPL = Rendimiento_AAPL,
  Rendimiento_COIN = Rendimiento_COIN,
  Rendimiento_GEHC = Rendimiento_GEHC,
  Rendimiento_Portafolio = Rendimiento_Portafolio
)

# Crear el gráfico de dispersión 3D
fig <- plot_ly(data, x = ~Rendimiento_AAPL, y = ~Rendimiento_COIN, z = ~Rendimiento_GEHC, 
               type = "scatter3d", mode = "markers", marker = list(size = 5, color = ~Rendimiento_Portafolio))

# Obtener los coeficientes del modelo de regresión
coeficientes <- coef(modelo)

# Crear un plano de regresión utilizando los coeficientes
x <- seq(min(data$Rendimiento_AAPL), max(data$Rendimiento_AAPL), length.out = 100)
y <- seq(min(data$Rendimiento_COIN), max(data$Rendimiento_COIN), length.out = 100)
superficie <- outer(x, y, function(x, y) coeficientes[1] + coeficientes[2] * x + coeficientes[3] * y)

# Agregar el plano de regresión al gráfico
fig <- fig %>% add_surface(z = ~superficie, colorscale = "Viridis")

# Mostrar el gráfico
fig
























# Crear un conjunto de datos de ejemplo con tus variables
Rendimiento_AAPL <- rnorm(100)
Rendimiento_COIN <- rnorm(100)
Rendimiento_GEHC <- rnorm(100)
Rendimiento_Portafolio <- rnorm(100)

# Ajustar un modelo de regresión múltiple
modelo <- lm(Rendimiento_Portafolio ~ Rendimiento_AAPL + Rendimiento_COIN + Rendimiento_GEHC)

# Crear un data frame con los datos
data <- data.frame(
  Rendimiento_AAPL = Rendimiento_AAPL,
  Rendimiento_COIN = Rendimiento_COIN,
  Rendimiento_GEHC = Rendimiento_GEHC,
  Rendimiento_Portafolio = Rendimiento_Portafolio
)

# Crear el gráfico de dispersión 3D
fig <- plot_ly(data, x = ~Rendimiento_AAPL, y = ~Rendimiento_COIN, z = ~Rendimiento_GEHC, 
               type = "scatter3d", mode = "markers", marker = list(size = 5, color = ~Rendimiento_Portafolio))

# Mostrar el gráfico
fig



























# Crear un conjunto de datos de ejemplo con tus variables
Rendimiento_AAPL <- rnorm(100)
Rendimiento_COIN <- rnorm(100)
Rendimiento_GEHC <- rnorm(100)
Rendimiento_Portafolio <- rnorm(100)

# Ajustar un modelo de regresión múltiple
modelo <- lm(Rendimiento_Portafolio ~ Rendimiento_AAPL + Rendimiento_COIN + Rendimiento_GEHC)

# Crear un data frame con los datos
data <- data.frame(
  Rendimiento_AAPL = Rendimiento_AAPL,
  Rendimiento_COIN = Rendimiento_COIN,
  Rendimiento_GEHC = Rendimiento_GEHC
)

# Agregar la predicción del modelo al data frame
data$Prediccion_Portafolio <- predict(modelo)

# Crear el gráfico de dispersión 3D
fig <- plot_ly(data, x = ~Rendimiento_AAPL, y = ~Rendimiento_COIN, z = ~Rendimiento_GEHC, 
               type = "scatter3d", mode = "markers", marker = list(size = 5))

# Agregar la línea de regresión al gráfico
fig <- fig %>% add_trace(
  x = ~Rendimiento_AAPL,
  y = ~Rendimiento_COIN,
  z = ~Prediccion_Portafolio,
  type = "scatter3d",
  mode = "lines",
  line = list(color = "red", width = 2)
)

# Mostrar el gráfico
fig





















# Crear un conjunto de datos de ejemplo con tus variables
Rendimiento_AAPL <- rnorm(100)
Rendimiento_COIN <- rnorm(100)
Rendimiento_GEHC <- rnorm(100)
Rendimiento_Portafolio <- rnorm(100)

# Ajustar un modelo de regresión múltiple
modelo <- lm(Rendimiento_Portafolio ~ Rendimiento_AAPL + Rendimiento_COIN + Rendimiento_GEHC)

# Crear un gráfico de dispersión 3D
scatterplot3d(
  Rendimiento_AAPL, Rendimiento_COIN, Rendimiento_GEHC,
  color = "blue", pch = 16,
  main = "Gráfico 3D con Línea de Regresión"
)

# Agregar la línea de regresión al gráfico
s3d.abline(modelo, col = "red")

# Puedes rotar y explorar el gráfico 3D en la ventana gráfica interactiva que se abrirá

















# Crear un conjunto de datos de ejemplo con tus variables
Rendimiento_COIN <- rnorm(100)
Rendimiento_GEHC <- rnorm(100)
Rendimiento_Portafolio <- rnorm(100)

# Crear un gráfico de dispersión 3D
plot3d(Rendimiento_COIN, Rendimiento_GEHC, Rendimiento_Portafolio, col = "blue", size = 2)

# Agregar anotaciones de texto para las variables
text3d(Rendimiento_COIN, Rendimiento_GEHC, Rendimiento_Portafolio, 
       labels = c("Rendimiento_COIN", "Rendimiento_GEHC", "Rendimiento_Portafolio"), 
       col = "red", adj = c(-0.5, -0.5))

# Puedes rotar y explorar el gráfico 3D en l

















# Gráfica de los rendimientos diarios
ggplot(base_datos, aes(x = new_date)) +
  geom_line(aes(y = Rendimiento_AAPL, color = "AAPL"), size = 1) +
  geom_line(aes(y = Rendimiento_COIN, color = "COIN"), size = 1) +
  geom_line(aes(y = Rendimiento_GEHC, color = "GEHC"), size = 1) +
  geom_line(aes(y = Rendimiento_Portafolio, color = "Portafolio"), size = 1) +
  labs(title = "Rendimientos Diarios de Acciones y Portafolio",
       x = "Fecha",
       y = "Rendimiento") +
  scale_color_manual(values = c("AAPL" = "blue", "COIN" = "red", "GEHC" = "green", "Portafolio" = "purple")) +
  theme_minimal()
















# Gráfica de los rendimientos diarios en líneas y puntos
ggplot(base_datos, aes(x = new_date)) +
  geom_line(aes(y = Rendimiento_AAPL, color = "AAPL"), size = 1) +
  geom_line(aes(y = Rendimiento_COIN, color = "COIN"), size = 1) +
  geom_line(aes(y = Rendimiento_GEHC, color = "GEHC"), size = 1) +
  geom_line(aes(y = Rendimiento_Portafolio, color = "Portafolio"), size = 1) +
  geom_point(aes(y = Rendimiento_AAPL, color = "AAPL"), size = 2) +
  geom_point(aes(y = Rendimiento_COIN, color = "COIN"), size = 2) +
  geom_point(aes(y = Rendimiento_GEHC, color = "GEHC"), size = 2) +
  geom_point(aes(y = Rendimiento_Portafolio, color = "Portafolio"), size = 2) +
  labs(title = "Rendimientos Diarios de Acciones y Portafolio",
       x = "Fecha",
       y = "Rendimiento") +
  scale_color_manual(values = c("AAPL" = "blue", "COIN" = "red", "GEHC" = "green", "Portafolio" = "purple")) +
  theme_minimal()




# Gráfica de los rendimientos diarios en líneas y puntos
ggplot(base_datos, aes(x = new_date)) +
  geom_line(aes(y = Rendimiento_AAPL, color = "AAPL"), size = 1) +
  geom_line(aes(y = Rendimiento_COIN, color = "COIN"), size = 1) +
  geom_line(aes(y = Rendimiento_GEHC, color = "GEHC"), size = 1) +
  geom_line(aes(y = Rendimiento_Portafolio, color = "Portafolio"), size = 1) +
  geom_point(aes(y = Rendimiento_AAPL, color = "AAPL"), size = 2) +
  geom_point(aes(y = Rendimiento_COIN, color = "COIN"), size = 2) +
  geom_point(aes(y = Rendimiento_GEHC, color = "GEHC"), size = 2) +
  geom_point(aes(y = Rendimiento_Portafolio, color = "Portafolio"), size = 2) +
  labs(title = "Rendimientos Diarios de Acciones y Portafolio",
       x = "Fecha",
       y = "Rendimiento") +
  scale_color_manual(values = c("AAPL" = "blue", "COIN" = "red", "GEHC" = "green", "Portafolio" = "purple")) +
  theme_minimal()




# Gráfico de rendimientos diarios y predicción
ggplot(base_datos, aes(x = Rendimiento_AAPL, y = Rendimiento_Portafolio)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
  geom_smooth(aes(y = Prediccion_Portafolio), color = "red", linetype = "dashed") +
  labs(title = "Rendimiento Portafolio vs. Rendimiento AAPL con Predicción",
       x = "Rendimiento AAPL",
       y = "Rendimiento Portafolio") +
  scale_color_manual(values = c("purple" = "purple", "blue" = "blue", "red" = "red")) +
  theme_minimal()


