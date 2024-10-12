# AIML- INCAF-1
# Módulo 1: Identificar origen de datos
# Desarrollar un proyecto completo de ciencia de datos utilizando R
# Gabriel Guzmán

# 1. Elección del conjunto de datos
# Selecciona un conjunto de datos que despierte tu interés. Pueden ser datos provenientes cualquiera área, como salud, finanzas, deportes, medio ambiente, etc. Recuerda que el conjunto de datos debe ser suficientemente grande para realizar un análisis significativo (mínimo 1000 filas y varias variables) y debe estar disponible en un formato legible por python, como csv, json, excel, etc. Puedes encontrar conjuntos de datos en plataformas como kaggle, uci machine learning repository, o google dataset search

# Instalar y cargar la librería readr
install.packages("readr")
library(readr)

# Leer el archivo CSV
data <- read_csv("vgsales.csv")

# Mostrar las primeras filas del DataFrame
head(data)

# Mostrar las columnas del DataFrame
colnames(data)

# Verificar si hay valores nulos en las columnas
colSums(is.na(data))

### 2. Limpieza y transformación de los datos
# Una vez que tengas el conjunto de datos, el siguiente paso es limpiarlo y transformarlo para que esté listo para el análisis. Recuerda verificar las variables, gestionar los valores faltantes, corregir errores y similares

# Verificar si hay valores nulos en el conjunto de datos
colSums(is.na(data))

# Eliminar filas con valores nulos
data_cleaned <- na.omit(data)

# Verificar que no hay valores nulos
colSums(is.na(data_cleaned))

# Verificar los tipos de datos
str(data)

# Eliminar las filas donde el valor de 'Year' es nulo
data <- data[!is.na(data$Year), ]

# Convertir a entero
data$Year <- as.integer(data$Year)

# Convertir la columna 'Year' a un tipo de dato entero
data$Year <- as.integer(data$Year)

# Crear una nueva columna con las ventas totales por región
data$Total_Sales <- data$NA_Sales + data$EU_Sales + data$JP_Sales + data$Other_Sales

# Verificar que la nueva columna se ha creado
head(data)

# Eliminar la columna 'Rank' si no es necesaria para el análisis
data_cleaned <- data[ , -which(names(data) == "Rank")]

# Verificar si hay filas duplicadas
sum(duplicated(data))

# Eliminar filas duplicadas
data_cleaned <- unique(data)

### 3. Análisis exploratorio de datos (eda)
# Ejecuta el análisis exploratorio es esencial para comprender mejor la estructura, las relaciones y las características clave del conjunto de datos generando un resumen estadístico de las variables para identificar su distribución, medias, medianas, desviaciones estándar, etc. Recuerda también identificar patrones y tendencias dentro de los datos.

# Resumen estadístico básico del conjunto de datos
summary(data)

# Esta es una visión rápida de la distribución de las ventas por región y las ventas globales, así como la cantidad de videojuegos por año, si ya se realizó la conversión.
### Distribución y conteo de valores categóricos

# Contar cuántos juegos hay por género
table(data$Genre)

# Contar cuántos juegos hay por plataforma
table(data$Platform)

# Contar cuántos juegos hay por editor (Publisher)
head(sort(table(data$Publisher), decreasing = TRUE), 10)  # Mostramos solo los 10 principales editores

# Matriz de correlación entre las ventas por región
correlation_matrix <- cor(data[, c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", "Global_Sales")])

correlation_matrix

### Distribución de los géneros de videojuegos
### Tendencias de ventas globales a lo largo de los años

# Instalar y cargar la librería ggplot2
install.packages("ggplot2")
library(ggplot2)

# Agrupar las ventas globales por año
sales_by_year <- aggregate(Global_Sales ~ Year, data = data, sum)

# Graficar las ventas globales por año
ggplot(sales_by_year, aes(x = Year, y = Global_Sales)) +
  geom_line() +
  labs(title = "Tendencia de las ventas globales a lo largo de los años",
       x = "Año",
       y = "Ventas globales (en millones)")

### Comparación de ventas por región
# Preparar los datos para el gráfico
sales_by_region <- data.frame(
  Region = c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales"),
  Sales = colSums(data[, c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")])
)

# Graficar la comparación de ventas por región
ggplot(sales_by_region, aes(x = Region, y = Sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de ventas por región",
       x = "Región",
       y = "Ventas (en millones)")

### Identificación de tendencias por editor o género
# Agrupar las ventas globales por editor
sales_by_publisher <- aggregate(Global_Sales ~ Publisher, data = data, sum)
sales_by_publisher <- sales_by_publisher[order(sales_by_publisher$Global_Sales, decreasing = TRUE), ]
sales_by_publisher <- head(sales_by_publisher, 10)

# Graficar las ventas por los 10 principales editores
ggplot(sales_by_publisher, aes(x = Publisher, y = Global_Sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Ventas globales por los principales editores",
       x = "Editor",
       y = "Ventas globales (en millones)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### 4. Visualización de resultados
# Gráfico de Barras: Distribución de géneros de videojuegos
# Crear un DataFrame con la distribución de géneros
genre_counts <- data.frame(Genre = names(table(data$Genre)), Count = as.numeric(table(data$Genre)))

# Graficar la distribución de géneros
ggplot(genre_counts, aes(x = Genre, y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución de géneros de videojuegos",
       x = "Género",
       y = "Cantidad de juegos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###  Gráfico de Pastel: Distribución de ventas globales por los principales editores
# Agrupar las ventas globales por editor
sales_by_publisher <- aggregate(Global_Sales ~ Publisher, data = data, sum)
sales_by_publisher <- sales_by_publisher[order(sales_by_publisher$Global_Sales, decreasing = TRUE), ]
sales_by_publisher <- head(sales_by_publisher, 5)

# Graficar las ventas por los 5 principales editores
ggplot(sales_by_publisher, aes(x = "", y = Global_Sales, fill = Publisher)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribución de ventas globales por los 5 principales editores",
       fill = "Editor") +
  theme_void()

### Boxplot: Ventas globales por plataforma
# Graficar un boxplot de ventas globales por plataforma
ggplot(data, aes(x = Platform, y = Global_Sales)) +
  geom_boxplot() +
  labs(title = "Distribución de ventas globales por plataforma",
       x = "Plataforma",
       y = "Ventas globales (en millones)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Scatter Plot: Relación entre ventas en Norteamérica y Europa
# Graficar un diagrama de dispersión entre ventas en Norteamérica y Europa
ggplot(data, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point(alpha = 0.5) +
  labs(title = "Relación entre ventas en Norteamérica y Europa",
       x = "Ventas en NA (en millones)",
       y = "Ventas en EU (en millones)") +
  theme_bw()


### 5. Documentación y presentación
# Finalmente, proyecto debe ser entregado en un repositorio de github, que incluya el notebook de jupyter, los conjuntos de datos utilizados, y un breve informe que resuma tus hallazgos, observaciones clave, y visualizaciones más importantes, comentarios en el código y una sección de conclusiones donde expliques los resultados obtenidos y las posibles implicaciones o próximos pasos a seguir.

### Debido a que es es la conversión del anterior proyecto las conclusiones son las mimas

# 
# ### Resumen del Proyecto: Análisis Exploratorio de Ventas de Videojuegos
# Este proyecto aborda el análisis exploratorio de un conjunto de datos sobre ventas de videojuegos a nivel global, enfocado en la identificación de tendencias, patrones y relaciones entre diferentes variables, como las ventas por región, el año de lanzamiento, el género del videojuego y las plataformas.
# 
# ### 1. Limpieza y Transformación de los Datos
# Los datos presentaban algunos valores nulos en la columna del año de lanzamiento. Se tomaron las siguientes medidas:
#   Las filas con valores nulos en la columna "Year" fueron eliminadas.
# 
# Se agregaron nuevas columnas para facilitar el análisis, como la suma de las ventas por regiones para obtener la "Total_Sales".
# 
# ### 2. Análisis Exploratorio de Datos (EDA)
# Se realizaron diversas exploraciones estadísticas y visualizaciones para entender mejor el conjunto de datos:
#   Resumen estadístico: Las ventas en Norteamérica tienen una mayor media en comparación con otras regiones, lo que sugiere que este mercado tiene una fuerte influencia en las ventas globales.
# 
# Distribución de géneros: Los géneros más comunes en el mercado de videojuegos son Acción, Deportes y Aventura.
# Ventas globales por año: Las ventas globales alcanzaron su punto máximo en 2008, lo que indica un auge de la industria de videojuegos durante ese período.
# 
# Comparación de ventas por región: Las ventas en Norteamérica son significativamente mayores que las de otras regiones, seguida por Europa y Japón.
# 
# ### 3. Visualización de Resultados
# Se generaron gráficos para comunicar mejor los hallazgos, entre ellos:
#   Gráfico de barras que muestra la distribución de géneros de videojuegos.
# 
# Gráfico de líneas que refleja la tendencia de las ventas globales a lo largo de los años, mostrando un crecimiento marcado hasta el 2008, seguido de una caída.
# 
# Gráfico de pastel que destaca las ventas globales de los principales editores de videojuegos.
# 
# Gráfico de dispersión que evidencia la relación entre las ventas en Norteamérica y Europa, mostrando una correlación significativa.
# 
# ### 4. Conclusiones
# Tendencias en las ventas globales: Las ventas de videojuegos tuvieron un crecimiento notable durante el período 2005-2010, con una clara tendencia descendente en los últimos años del conjunto de datos.
# 
# Preferencias por género: Los videojuegos de acción y deportes dominan el mercado, lo que sugiere una clara preferencia por parte de los consumidores hacia títulos dinámicos y competitivos.
# 
# Importancia de las regiones: Norteamérica representa la mayor porción de ventas globales, indicando su relevancia como mercado principal de videojuegos.
# 
# Saludos GG!!! 
#   
#   :D