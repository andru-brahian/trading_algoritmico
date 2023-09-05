
# ACTIVIDAD : regresión logistica

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


# Contruccion de la tabla final correctamente documentada. esta borra todos los valores, no es viable
#base_datos2 <-base_datos[!duplicated(base_datos)&(!is.na(base_datos$quality)), ]

#originalmente la base de datos usa 5800 filas, pero tiene varias para el mismo día porque en twitter se hacen varias recomendaciones en un día, y por eso se deben borrar los duplicados, para que la regresión tenga sentido, de lo contrario, dará todo cerca de 0 en la regresión
base_datos2 <- base_datos[!duplicated(base_datos$Close_AAPL), ]
summary(base_datos2)


base_datos2$Up <- ifelse(base_datos2$Close_AAPL > lag(base_datos2$Close_AAPL), 1, 0)
head(base_datos2$Up)

sim_logistic_data <- function(sample_size = 25, beta_0 = -2, beta_1 = 3) {
  # Generar datos de ejemplo usando Close_AAPL de base_datos2
  Close_AAPL <- rnorm(n = sample_size)
  eta <- beta_0 + beta_1 * Close_AAPL
  p = 1 / (1 + exp(-eta))
  Up = rbinom(n = sample_size, size = 1, prob = p)
  data.frame(Up, Close_AAPL)
}

set.seed(2)
example_data <- sim_logistic_data()
head(example_data)

regresion_logistica <- glm(Up ~ Close_AAPL, data = base_datos2, family = binomial)
summary(regresion_logistica)

regresion_logistica <- glm(Up ~ Close_AAPL, data = base_datos2, family = binomial, control = glm.control(maxit = 100))
summary(regresion_logistica)

summary(base_datos2$Close_AAPL)  # Muestra estadísticas descriptivas para Close_AAPL
head(base_datos2$Up)
summary(base_datos2$Up)
summary(base_datos2$Close_AAPL)

base_datos3 <- base_datos2[, c("Up", "Close_AAPL")]

# Muestra los primeros 10 registros de los nuevos datos
head(base_datos3, 200)


#aparecen numeros por encima de 1 o por debajo de.6 y por eso usamos tiplo response
prediccion_logistica = predict(object = regresion_logistica, newdata = base_datos3, type = "response")
prediccion_logistica

# CATEGORIZAR VALORES MAYORES A 0,5 COMO SUBIO EL PRECIO DE Close_AAPL , 1, Y DE LO CONTRARIO 0, baja
prediccion_logistica= ifelse(prediccion_logistica>0.5, yes= 1, no = 0)
prediccion_logistica


# Cargar la biblioteca dplyr
library(dplyr)

#comparar la realidad con las predicciones 
#mutate para añadir la columna de las predicciones
#comparacion para comparar (true si acertamos,falso de lo contrario)
base_datos3 %>%
  mutate(predicciones = prediccion_logistica) %>%
  select(predicciones, everything()) %>%
  mutate(comparacion=predicciones == Up) -> variable_auxiliar
  
variable_auxiliar

#na para no tener en cuenta VALORES NA en la suma
sum(variable_auxiliar$comparacion, na.rm= TRUE)

#precisión del modelo de regresión logística
mean(variable_auxiliar$comparacion, na.rm= TRUE)




library(ggplot2)


# Contar cuántos 1 tiene la columna Up
contar_unos <- sum(base_datos2$Up == 1)
contar_unos
contar_ceros <- sum(base_datos2$Up == 0)
contar_ceros
summary(base_datos2$Up)
head(base_datos2$Up, 100)
head(base_datos2[, c("Up", "Close_AAPL")], 300)

base_datos2 <- base_datos2[-1, ]
head(base_datos2[, c("Up", "Close_AAPL")], 300)




# Cargar el paquete ggplot2 si aún no está cargado
# install.packages("ggplot2")
library(ggplot2)

# Crear un gráfico de dispersión de los datos y agregar la línea de regresión
ggplot(data = base_datos3, aes(x = Close_AAPL, y = Up)) +
  geom_point(data = variable_auxiliar, aes(color = comparacion)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              formula = y ~ x, color = "blue") +
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  ggtitle("Gráfico de dispersión de Up vs. Close_AAPL con Regresión Logística") +
  xlab("Close_AAPL") +
  ylab("Up") +
  theme_minimal() +
  geom_hline(yintercept = unique(variable_auxiliar$predicciones), linetype = "dashed", color = "purple")






