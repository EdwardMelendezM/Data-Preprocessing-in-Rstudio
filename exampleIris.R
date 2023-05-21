# 1.1 CONJUNTO DE DATOS IRIS
library(tidyverse)
library(GGally)

data(iris)
iris <- as_tibble(iris)
iris

print(iris, n = 3, width = Inf)


# 1.2 CALIDAD DE DATOS

# Evaluar la calidad de los datos disponibles es crucial antes de comenzar a utilizarlos.
# Comience con estadísticas de resumen para cada columna para identificar valores atípicos y faltantes.
summary(iris)

# También puede resumir columnas específicas usando una función estadística como mean().
iris %>% summarize_if(is.numeric, mean)

# Un método visual para inspeccionar los datos es usar una matriz de diagrama de dispersión
# (aquí usamos ggpairs() del paquete GGally). En este gráfico, podemos identificar visualmente
# puntos de datos de ruido y valores atípicos (puntos que están lejos de la mayoría de los otros puntos).
# ggpairs(iris, aes(color = Species))



# Muchos métodos de minería de datos requieren datos completos, es decir, los datos no pueden contener
# valores faltantes (NA). Para eliminar valores faltantes y duplicados (puntos de datos idénticos que
# pueden ser un error en los datos), a menudo hacemos esto:
clean.data <- iris %>% drop_na() %>% unique()
summary(clean.data)


# 1.3 AGREGACION

# Los datos a menudo contienen grupos y queremos comparar estos grupos. Agrupamos el conjunto de datos
# de iris por especie y luego calculamos una estadística de resumen para cada grupo.
iris %>% group_by(Species) %>% summarize_all(mean)
iris %>% group_by(Species) %>% summarize_all(median)

# 1.4 MUESTREO

# El muestreo se usa a menudo en la minería de datos para reducir el tamaño del conjunto de datos
# antes del modelado o la visualización.

# 1.4.1 MUESTREO ALEATORIO

# La función de muestra integrada puede tomar muestras de un vector. Aquí tomamos muestras con reemplazo.
sample(c("A", "B", "C"), size = 10, replace = TRUE)

# A menudo queremos muestrear filas de un conjunto de datos. Esto se puede hacer muestreando sin reemplazo
# de un vector con índices de fila (usando las funciones seq() y nrow()). Luego, el vector de muestra se
# usa para dividir las filas del conjunto de datos.
take <- sample(seq(nrow(iris)), size = 15)
take

iris[take, ]

# dplyr de tidyverse nos permite muestrear filas de tibbles directamente usando slice_sample(). Configuré
# la semilla del generador de números aleatorios para que los resultados fueran reproducibles.
set.seed(1000)

s <- iris %>% slice_sample(n = 15)
ggpairs(s, aes(color = Species))

# 1.4.2 MUESTREO ESTRATIFICADO

# El muestreo estratificado es un método de muestreo de una población que se puede dividir en subpoblaciones,
# mientras se controlan las proporciones de la subpoblación en la muestra resultante.

library(sampling)
id2 <- strata(iris, stratanames = "Species", size = c(5,5,5), method = "srswor")
id2
s2 <- iris %>% slice(id2$ID_unit)
ggpairs(s2, aes(color = Species))


# 1.5 CARACTERISTICAS

# library(plotly) # No cargo el paquete porque su espacio de nombres choca con select en dplyr.
plotly::plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, z = ~Sepal.Width, size = ~Petal.Width, color = ~Species, type="scatter3d")
pc <- iris %>% select(-Species) %>% as.matrix() %>% prcomp()
summary(pc)
plot(pc, type = "line")
str(pc)
iris_projected <- as_tibble(pc$x) %>% add_column(Species = iris$Species)
ggplot(iris_projected, aes(x = PC1, y = PC2, color = Species)) + geom_point()
ggplot(iris_projected, aes(x = PC1, y = 0, color = Species)) + 
  geom_point() +
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank()
  )
library(factoextra)
fviz_pca(pc)
fviz_pca_var(pc)


# 1.5 ESCALADO MULTIDIMENSIONAL
# 1.5.1 REDUCCION DE DIMENSIONALIDAD

d <- iris %>% select(-Species) %>% dist()
fit <- cmdscale(d, k = 2)
colnames(fit) <- c("comp1", "comp2")
fit <- as_tibble(fit) %>% add_column(Species = iris$Species)

ggplot(fit, aes(x = comp1, y = comp2, color = Species)) + geom_point()

# 1.5.2 SELECCION DE FUNCIONES

# 1.5.3  DISCRETIZACION DE  CARACTERISTICAS

ggplot(iris, aes(x = Petal.Width)) + geom_histogram(binwidth = .2)
iris %>% pull(Sepal.Width) %>% cut(breaks = 3)
library(arules)
iris %>% pull(Petal.Width) %>% discretize(method = "interval", breaks = 3)
iris %>% pull(Petal.Width) %>% discretize(method = "frequency", breaks = 3)
iris %>% pull(Petal.Width) %>% discretize(method = "cluster", breaks = 3)

ggplot(iris, aes(Petal.Width)) + geom_histogram(binwidth = .2) +
  geom_vline(xintercept = iris %>% pull(Petal.Width) %>% discretize(
    method = "interval",
    breaks = 3,
    onlycuts = TRUE),
  color = "blue") +
  labs(title = "Discretization: interval", subtitle = "Blue lines are boundaries")

ggplot(iris, aes(Petal.Width)) + geom_histogram(binwidth = .2) +
  geom_vline(xintercept =
               iris %>% pull(Petal.Width) %>% discretize(method = "frequency", breaks = 3, onlycuts = TRUE),
             color = "blue") +
  labs(title = "Discretization: frequency", subtitle = "Blue lines are boundaries")

ggplot(iris, aes(Petal.Width)) + geom_histogram(binwidth = .2) +
  geom_vline(xintercept =
               iris %>% pull(Petal.Width) %>% discretize(method = "cluster", breaks = 3, onlycuts = TRUE),
             color = "blue") +
  labs(title = "Discretization: cluster", subtitle = "Blue lines are boundaries")


# 1.5.4 ESTANDARIZACION DE DATOS

scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))

iris.scaled <- iris %>% scale_numeric()
iris.scaled
summary(iris.scaled)

# 1.6 PROXIMIDADES : Semejanzas y distancias
# 1.6.1 DISTANCIAS DE MINKOSKY
iris_sample <- iris.scaled %>% select(-Species) %>% slice(1:5)
iris_sample

dist(iris_sample, method = "euclidean")
dist(iris_sample, method = "manhattan")
dist(iris_sample, method = "maximum")


# 1.6.2 DISTANCIAS PARA DATOS BINARIOS

b <- rbind(
  c(0,0,0,1,1,1,1,0,0,1),
  c(0,0,1,1,1,0,0,1,0,0)
)
b
#apply -> aplicar alguna funcion 
b_logical <- apply(b, MARGIN = 2, as.logical)
b_logical

# 1.6.2.1 DISTANIA DE HAMMING

dist(b, method = "manhattan")
dist(b, method = "euclidean")^2
dist(b, method = "binary")

# 1.6.3 DISTNCIAS PARA DATOS MIXTOS

#tibble -> es una estructura de rstudio ~ matriz
people <- tibble(
  height = c(      160,    185,    170),
  weight = c(       52,     90,     75),
  sex    = c( "female", "male", "male")
)
people

people <- people %>% mutate_if(is.character, factor)
people


# 1.6.3.1 COEFICIENTE DE GOWER

library(proxy)

d_Gower <- dist(people, method = "Gower")
d_Gower


# 1.6.3.2 USO DE LA DISTANCIA EUCLIDIANA CON DATOS MIXTOS

library(caret)
data_dummy <- dummyVars(~., people) %>% predict(people)
data_dummy
weight_matrix <- matrix(c(1, 1, 1/2, 1/2), ncol = 4, nrow = nrow(data_dummy), byrow = TRUE)
data_dummy_scaled <- scale(data_dummy) * weight_matrix

#dist -> me da una matriz de distancias
d_dummy <- dist(data_dummy_scaled)
d_dummy

ggplot(tibble(d_dummy, d_Gower), aes(x = d_dummy, y = d_Gower)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# 1.6.4 MEDIDAS DE PROXIMIDAD ADICIONALES DISPONIBLES EN EL PAQUETA PROXY

library(proxy)
pr_DB$get_entry_names()


# 1.7 RELACIONES ENTRE CARACTERISTICAS
# 1.7.1 CORRELACION

#Cor -> Me da una matriz de correlacion

cc <- iris %>% select(-Species) %>% cor()
cc

ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  geom_point() +
  geom_smooth(method = "lm")

#Correlaciones individuales
with(iris, cor(Petal.Length, Petal.Width))

with(iris, cor.test(Petal.Length, Petal.Width))

#Correlacion de rango
iris_ord <- iris %>% mutate_if(is.numeric,
                               function(x) cut(x, 3, labels = c("short", "medium", "long"), ordered = TRUE))
summary(iris_ord)

iris_ord %>% pull(Sepal.Length)
iris_ord %>% select(-Species) %>% sapply(xtfrm) %>% cor(method = "kendall")
iris_ord %>% select(-Species) %>% sapply(xtfrm) %>% cor(method = "spearman")


#ESTIMACION DE DENSIDAD


ggplot(iris, aes(x = Petal.Length, y = 0)) + geom_point()


# HISTOGRAMAS

#Histograma basico DE UNA DIMENSION 
ggplot(iris, aes(x = Petal.Length)) +
  geom_histogram() +
  geom_rug(alpha = 1/2)

#Histograma de dos dimensiones
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_bin2d(bins = 10) +
  geom_jitter(color = "red")

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_hex(bins = 10) +
  geom_jitter(color = "red")

# ESTIMACION DE DENSIDAD

library(tidyverse)

#Basico
ggplot(iris, aes(Petal.Length)) +
  geom_density(bw = .2) +
  geom_rug(alpha = 1/2)


#Dos dimensiones
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_density_2d_filled() +
  geom_jitter()

