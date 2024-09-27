#Actividad: Proyecto de integracion de python y R by Alexander Guevara

# Importando librerias a utilizar
library(reticulate)
library(ggplot2)
library(glmnet)
library(tidymodels)
library(caret)
library(pROC)
library(rsample)
library(dplyr)


#1. Elección del conjunto de datos
# Cargando los datos de interes a utilizar
# los datos de interes estan relacionados a un dataset de vehiculos compartido en clases que llamo mi atencion

# Usar reticulate para cargar pandas
py_run_string("import pandas as pd")

# Cargar los datos en un dataframe de pandas
py_run_string("
import pandas as pd
df_vehiculos = pd.read_csv('cars.csv')
")

# 2. Limpieza y transformación de los datos en python

# Realizando cambio de tipos en Python a tipos correspondientes de categoria y string
py_run_string("
df_vehiculos['manufacturer_name'] = df_vehiculos['manufacturer_name'].astype('string')
df_vehiculos['model_name'] = df_vehiculos['model_name'].astype('string')
df_vehiculos['color'] = df_vehiculos['color'].astype('string')
df_vehiculos['engine_fuel'] = df_vehiculos['engine_fuel'].astype('string')
df_vehiculos['body_type'] = df_vehiculos['body_type'].astype('string')
df_vehiculos['location_region'] = df_vehiculos['location_region'].astype('string')
df_vehiculos['transmission'] = df_vehiculos['transmission'].astype('category')
df_vehiculos['engine_type'] = df_vehiculos['engine_type'].astype('category')
df_vehiculos['state'] = df_vehiculos['state'].astype('category')
df_vehiculos['drivetrain'] = df_vehiculos['drivetrain'].astype('category')
")

# Eliminar las filas con valor NA en python
py_run_string("
df_vehiculos = df_vehiculos.dropna()
              ")

# Eliminar las filas duplicadas en python
py_run_string("
df_vehiculos = df_vehiculos.drop_duplicates()
")

# 3. Tranferir los datos a R
df_r <- py$df_vehiculos
df_vehiculos <- as.data.frame(df_r)

# Visualizando los datos transferidos a R
head(df_vehiculos)
str(df_vehiculos)

# Verificando que no existen valores nulos en R, se realizo la limpieza de nulos previamente en python exitosamente
colSums(is.na(df_vehiculos))

#Verificando que no existen duplicados en R, se realizo la limpieza de duplicados previamente en python exitosamente
sum(duplicated(df_vehiculos))

# 4. Análisis exploratorio de datos (EDA)

# Visualizando los primeros cinco registros
head(df_vehiculos, 5)

# Conociendo las columnas del dataframe
colnames(df_vehiculos)

# Verificando las dimenciones del dataframe
dim(df_vehiculos)

# Verificando los tipos de datos de las columnas del Dataframe
# Se evidencia en el dataframe los tipos factor que fueron convertidos anteriormente en python
# transmission, engine_type, state, drivetrain
sapply(df_vehiculos, class)

# Obteniendo mas informacion datos del conjunto de datos
str(df_vehiculos)

# Medidas de tendencia central
summary(df_vehiculos)

# visualizando los años de produccion de los vehiculos
table(df_vehiculos$year_produced)

# Analizando un poco mas los vehiculos
print(paste("El precio promedio de los vehiculos es: ", mean(df_vehiculos$price_usd)))
print(paste("La mediana del precio de los vehiculos es: ", median(df_vehiculos$price_usd)))
print(paste("La moda del color de los vehiculos es: ", names(sort(-table(df_vehiculos$color)))[1]))
print(paste("La varianza del precio de los vehiculos es: ", var(df_vehiculos$price_usd)))
print(paste("La desviacion estandar del precio de los vehiculos es: ", sd(df_vehiculos$price_usd)))
print(paste("El rango de precio de los vehiculos es: ", max(df_vehiculos$price_usd) - min(df_vehiculos$price_usd)))
# el 75% de los precios se encuentra en este precio o debajo
print(paste("El tercer cuartil del precio de los vehiculos es: ", quantile(df_vehiculos$price_usd, 0.75)))

# utilizando paquetes como dplyr 
# Agrupar por fabricante y calcular el precio promedio de los vehículos de cada fabricante
df_vehiculos %>%
  group_by(manufacturer_name) %>%
  summarise(mean_price = mean(price_usd)) %>%
  arrange(desc(mean_price))

# Agrupar por tipo de motor y transmisión, y contar la cantidad de vehículos en cada grupo
df_vehiculos %>%
  group_by(engine_type, transmission) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Agrupar por año de producción y calcular el precio máximo de los vehículos de cada año
df_vehiculos %>%
  group_by(year_produced) %>%
  summarise(max_price = max(price_usd)) %>%
  arrange(desc(max_price))

# Agrupar por color y calcular el conteo vehículos, los vehiculos encabezan los colores negro, silver y azul en manufactura
df_vehiculos %>%
  group_by(color) %>%
  summarise(count = n()) %>% 
  arrange(desc(count))

#4. Visualización de resultados

# Visualizamos la transmision de los vehiculos y observamos que el dataset tiene mas vehiculos con transmision mecanica
ggplot(df_vehiculos, aes(x = transmission)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Transmision de vehiculos")

# Visualizamos el tipo de motor de los vehiculos y observamos que el dataset tiene mas vehiculos con tipo de motor gasolina
ggplot(df_vehiculos, aes(x = engine_type)) + 
  geom_bar(fill = "darkgreen") + 
  labs(title = "Tipo de motor")


# obtenemos numero de colores
color_counts <- table(df_vehiculos$color)
# Crear la lista explode con 0 para todas las porciones excepto la primera.
num_colors <- length(color_counts)
explode <- c(0.1, rep(0, num_colors - 1))

suppressWarnings({
  # visualizar el gráfico de pastel de los colores de los vehiculos, observamos que el color black encabeza el color de los vehiculos
  pie(color_counts, 
      labels = names(color_counts), 
      autopct = "%1.2f%%", 
      explode = explode,
      main = "Color de Vehiculos")
})

# Seleccionar las columnas numericas
numeric_df <- df_vehiculos[, sapply(df_vehiculos, is.numeric)]

# Calcular la matriz de correlacion
corr <- cor(numeric_df)

# graficando la correlación lineal entre las variables
# observamos que hay una alta correlacion entre el año de produccion del vehiculo (year_produced) y el precio (price_usd)
# tambien hay una correlacion negativa entre el valor de distancia recorrida (odometer_value) y el precio (price_usd)
heatmap(corr, 
        symm = TRUE, 
        col = colorRampPalette(c("yellow", "green", "blue"))(100),
        main = "Correlation between features")


# visualizando boxplot de los precios
boxplot(price_usd ~ transmission, 
        data = df_vehiculos, 
        main = "Precio por transmission", 
        xlab = "Transmission", 
        ylab = "Price (USD)",col = "blue")


# calcular el conteo de vehiculos por fabricante
manufacturer_counts <- table(df_vehiculos$manufacturer_name)

# obtener el top 10 de fabricantes
top_10_manufacturers <- sort(manufacturer_counts, decreasing = TRUE)[1:10]

# Visualizando top 10 de cantidad de vehiculos por fabricante en el dataset
barplot(top_10_manufacturers, 
        main = "Vehiculos por fabricante", 
        xlab = "Fabricante", 
        ylab = "Cantidad de vehiculos",
        col = "blue")

# Conclusiones
# La función py_run_string permitió ejecutar fragmentos de código Python directamente desde R, sin necesidad de scripts externos, lo que agilizó la integración entre ambos lenguajes.
# Después de realizar las tareas iniciales de procesamiento en Python, los datos fueron transferidos a R mediante reticulate, lo que permitió una transición sin complicaciones entre ambos lenguajes para continuar el análisis en R.
# Una vez en R, se aprovecharon las capacidades de dplyr para realizar transformaciones adicionales de los datos, como filtrado, agrupación y resúmenes, lo que complementó el procesamiento inicial hecho con pandas.
# El uso combinado de py_run_string y reticulate permitió maximizar las fortalezas de cada lenguaje: Python para la manipulación y limpieza rápida de datos con pandas, y R para un análisis detallado y flexible con paquetes como dplyr y ggplot2

