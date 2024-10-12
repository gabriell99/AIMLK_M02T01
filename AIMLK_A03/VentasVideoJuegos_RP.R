# AIML- INCAF-1
# Módulo 1: Identificar origen de datos
# Proyecto de ciencia de datos integrando Phyton y R
# Gabriel Guzmán

# 1. Elección del conjunto de datos
# Selecciona un conjunto de datos que despierte tu interés. Pueden ser datos de cualquier área, como salud, finanzas, deportes, medio ambiente, etc. Recuerda que el conjunto de datos debe ser lo suficientemente grande para permitir un análisis significativo (mínimo 1000 filas y varias variables) y ser compatible tanto con Python como con R, como CSV, JSON, Excel, etc.

# Instalar y cargar la librería readr
install.packages("reticulate")  # Si no lo has instalado
library(reticulate)
library(dplyr)

# Invocando pandas de Python desde R
pd <- import("pandas")

# Leer el archivo CSV usando pandas en Python
data <- pd$read_csv("vgsales.csv")

# Convertir el DataFrame de pandas a un DataFrame de R
data_r <- as.data.frame(data)

# Mostrar las primeras filas del DataFrame cargado con código embedido Phyton
print(head(data_r))

# Mostrar las columnas del DataFrame
colnames(data)


### 2. Limpieza y transformación de los datos
# Emplea bibliotecas como pandas para cargar los datos y realizar tareas iniciales de limpieza, como manejo de valores faltantes, eliminación de duplicados y corrección de errores. Posteriormente transfiere los datos a R usando rpy2 o reticulate y utiliza paquetes como dplyr 

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

### invocando dplyr
data_filtered <- data_r %>%
  filter(Global_Sales > 1)  # Filtrar juegos con ventas globales mayores a 1 millón

# Mostrar el resultado
print(head(data_filtered))

### 3. Análisis exploratorio de datos (eda)
# Documenta y presenta tu trabajo, asegurándote de que todo el proceso esté bien explicado y accesible para los demás:
#   
# Código documentado: Asegúrate de que cada sección de código esté bien comentada, explicando el propósito de cada paso y la razón de las decisiones tomadas.
# Informe final: Redacta un informe que resuma tus hallazgos, observaciones clave, visualizaciones más importantes y conclusiones. Incluye comentarios en el código y explica los resultados obtenidos, así como las posibles implicaciones o próximos pasos.
# Entrega en GitHub: Sube el proyecto a un repositorio de GitHub, incluyendo el Jupyter Notebook, scripts de R, conjuntos de datos utilizados, y el informe final.
# Integración: El repositorio debe reflejar claramente cómo se integraron Python y R a lo largo del proyecto, con una estructura lógica y fácil de seguir.
###
### De este bloque en adelante se mantiene R despues de haber utilizado código embebido Phyton y librerias como reticulate o dplyr 
###

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



###
### Debido a que es es la conversión del anterior proyecto las conclusiones son las mismas
###
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