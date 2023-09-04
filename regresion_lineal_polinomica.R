


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

# Ahora puedes trabajar con la columna Close_AAPL como valores float
is.numeric(base_datos$Close_AAPL)
is.numeric(base_datos$Volume_AAPL)
#NA
anyNA(base_datos$Close_AAPL)
anyNA(base_datos$Volume_AAPL)



# Creación del gráfico de dispersión original
ggplot(data = base_datos, aes(x = Volume_AAPL, y = Close_AAPL)) +
  geom_point() +
  ggtitle("Gráfico de dispersión original") +
  xlab("Volume_AAPL") +
  ylab("Close_AAPL")

# Verificar si hay valores no numéricos en las columnas Close_AAPL y Volume_AAPL
non_numeric_close <- base_datos[!is.numeric(base_datos$Close_AAPL), ]
non_numeric_volume <- base_datos[!is.numeric(base_datos$Volume_AAPL), ]

# Mostrar las filas con valores no numéricos
print(non_numeric_close)
print(non_numeric_volume)

#POR SI ME ENCUENTRO CON ALGÚN NA, LOS DETECTO ASÍ:
# Calcular la cantidad de valores NA en la columna Close_AAPL
#cantidad_na <- sum(is.na(base_datos$Close_AAPL))
#anyNA(base_datos$Close_AAPL)
# Imprimir la cantidad de valores NA
#print(cantidad_na)


# Usando class()
class(base_datos$Close_AAPL)

# Usando typeof()
typeof(base_datos$Close_AAPL)
typeof(base_datos$Volume_AAPL)

# Usando str() para información más detallada
str(base_datos$Close_AAPL)



#intento 4


#REGRESIÓN LINEAL
# Ajustar modelo de regresión lineal con el conjunto de datos

## así sería si quisiera incluir absolutamente todas las variables de la base de datos
##lin_reg = lm(formula = Close_AAPL ~ ., data = base_datos)

## Añadiendo sólamente las variables deseadas:
regresion_lin = lm(formula = Close_AAPL ~ Volume_AAPL,data = base_datos)

## Predicciones del modelo lineal
prediccion_lineal= predict(regresion_lin, newdata = base_datos)

#visualización de modelo lineal:
ggplot(base_datos, aes(x = Volume_AAPL, y = Close_AAPL)) +
  geom_point(aes(x = Volume_AAPL , y = Close_AAPL), color = "red") +
  geom_line(aes(x = Volume_AAPL , y = prediccion_lineal), color = "blue") +
  ggtitle("Prediccion lineal, precio de cierre AAPL en función del volumen de ventas de acciones") +
  xlab("Volume_AAPL") +
  ylab("Close_AAPL")
              

#REGRESIÓN POLINOMIAL, VARIAS VARIABLES
#agregar columnas con nuevos datos:
base_datos$Volume_AAPL_cuadrado = base_datos$Volume_AAPL^2
base_datos$Volume_AAPL_cubo = base_datos$Volume_AAPL^3
base_datos$Volume_AAPL_cuarta_potencia = base_datos$Volume_AAPL^4



# Ajustar regresión polinomial con variables originales y sus potencias
#I() para indicar explícitamente que estas son variables independientes en la fórmula. 
#regresion_polinomica <- lm(Close_AAPL ~ Volume_AAPL, data = base_datos)
#regresion_polinomica <- lm(Close_AAPL ~ Volume_AAPL + Volume_AAPL_cuadrado + Volume_AAPL_cubo + Volume_AAPL_cuarta_potencia, data = base_datos)
#regresion_polinomica <- lm(Close_AAPL ~ Volume_AAPL + Volume_AAPL_cuadrado + Volume_AAPL_cubo + Volume_AAPL_cuarta_potencia, data = base_datos)
#regresion_polinomica <- lm(Close_AAPL ~ Volume_AAPL + I(Volume_AAPL_cuadrado) + I(Volume_AAPL_cubo) + I(Volume_AAPL_cuarta_potencia), data = base_datos)

#regresion_polinomica <- lm(formula = Close_AAPL ~.,
 #                          data = base_datos)




#regresion_polinomica = lm(Close_AAPL ~ poly(Volume_AAPL, 8), data = base_datos)
# Calcular el número de valores únicos en la variable Volume_AAPL
#num_valores_unicos <- length(unique(base_datos$Volume_AAPL))
#num_valores_unicos
# otra forma: regresion_polinomica = lm(Close_AAPL ~ poly(Volume_AAPL, 7), data = base_datos)
#Si aplicas poly(Volume_AAPL, 2), obtendrás una matriz de diseño polinómica de segundo grado para Volume_AAPL. La matriz resultante se vería así:

#volume  volume^2
#1   1
#2   4
#3   9

# Luego, el modelo de regresión lineal se ajusta a los datos de Close_AAPL en función de esta matriz de diseño polinómica. El resultado es una regresión polinómica de segundo grado que puede expresarse como:
# Close_AAPL = a + b1 * Volume_AAPL + b2 * (Volume_AAPL^2)


#poly(Volume_AAPL, degree = 7) especifica la variable independiente y su transformación. En este caso, Volume_AAPL se está transformando en una variable polinómica de septimo grado. Esto significa que, en lugar de usar Volume_AAPL directamente, se está utilizando Volume_AAPL elevado a la tercera potencia como variable independiente. Esta transformación permite modelar relaciones no lineales entre las variables.
#SI AUMENTO MÁS EL GRADO A MAYORES DE 8, SE PRODUCE SOBRE AJUSTE, LO QUE HACE QUE LA PREDICCIÓN REACCIONE DE FORMA DEMASIADO SENSIBLE Y ARROJA VALORES EXORBITANTEMENTE IRREALES
regresion_polinomica <- lm(Close_AAPL ~ poly(Volume_AAPL, degree = 8), data = base_datos)
# Mostrar el resumen del modelo
summary(regresion_polinomica)



# Creación de un rango de valores de Volume_AAPL para la predicción (los valores solo son para ubicar Close_AAPL en la gráfica, no son los valores originales de Volume_AAPL)
# length.out = nrow(base_datos) para que la longitud o cantidad de valores creada sea igual a la longitud de filas de la base de datos
volume_range <- seq(min(base_datos$Volume_AAPL), max(base_datos$Volume_AAPL), length.out = nrow(base_datos))
length(volume_range)


# Predicciones del modelo polinómico
#predictions <- predict(poly_reg, newdata = data.frame(Volume_AAPL = volume_range))
predicciones <- predict(regresion_polinomica, newdata = data.frame(Volume_AAPL = volume_range))



#DESCARGAR IMÁGEN
# Creación del gráfico, modelo polinómico
regresion_polinomica <- ggplot(base_datos, aes(x = Volume_AAPL, y = Close_AAPL)) +
  geom_point(color = "red") +
  geom_line(aes(x = volume_range, y = predicciones), color = "blue") +
  ggtitle("Predicción polinómica precio de cierre de AAPL en función del volumen") +
  xlab("Volumen AAPL") +
  ylab("Close AAPL") +
  theme_minimal()


regresion_polinomica






