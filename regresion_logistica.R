
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



# Funcion para conocer si una variable concreta esta totalmente documentada o si contiene nulos:
NA_COUNT<-function(x){
  for(i in 1:length(x)){
    if(is.na(x[i])) {
      print("Algun valor de la variable es nulo")
      break}
  }}

NA_COUNT(base_datos$Close_AAPL)
# Corroboramos que no disponemos de variables con valores nulos, ya que no nos aparece el print "Algun valor de la variable es nulo"

# Deteccion de duplicados basandonos en todas las columnas:
nrow(base_datos[duplicated(base_datos), ])    # 0 duplicados
nrow(base_datos[!duplicated(base_datos), ])   #5836 no duplicados


# Contruccion de la tabla final correctamente documentada.
#base_datos <-base_datos[!duplicated(base_datos)&(!is.na(base_datos$quality)), ]


# Instalar la librería si aún no está instalada
#install.packages("summarytools")
# Cargar la librería
#library(summarytools)

# Ahora, intenta usar la función 'aggr' nuevamente
#aggr(base_datos, numbers = TRUE)
#summary(base_datos$quality) # Corroboramos que no existen valores nulos (Na)

base_datos$Up <- ifelse(base_datos$Close_AAPL > lag(base_datos$Close_AAPL), 1, 0)
head(base_datos$Up)

sim_logistic_data <- function(sample_size = 25, beta_0 = -2, beta_1 = 3) {
  # Generar datos de ejemplo usando Close_AAPL de base_datos
  Close_AAPL <- rnorm(n = sample_size)
  eta <- beta_0 + beta_1 * Close_AAPL
  p = 1 / (1 + exp(-eta))
  Up = rbinom(n = sample_size, size = 1, prob = p)
  data.frame(Up, Close_AAPL)
}

set.seed(2)
example_data <- sim_logistic_data()
head(example_data)

fit_glm <- glm(Up ~ Close_AAPL, data = example_data, family = binomial)


summary(base_datos$Close_AAPL)  # Muestra estadísticas descriptivas para Close_AAPL
head(base_datos$Up)
summary(base_datos$Up)




# Cargar la librería ggplot2 si aún no está cargada
library(ggplot2)

# Crear un gráfico de dispersión de los datos
ggplot(data = base_datos2, aes(x = Close_AAPL, y = Up)) +
  geom_point() +
  ggtitle("Gráfico de dispersión de Up vs. Close_AAPL") +
  xlab("Close_AAPL") +
  ylab("Up") +
  
  # Agregar la curva de regresión logística
  #geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              formula = y ~ x, color = "blue") +
  
  # Personalizar la estética del gráfico (opcional)
  #theme_minimal()
