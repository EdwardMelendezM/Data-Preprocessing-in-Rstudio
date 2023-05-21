# EXPLORACION DE DATOS

#Estadisticas basicas

summary(iris)

#Hallar la media y la desviacion estandar
iris %>% pull(Sepal.Length) %>% mean()
iris %>% pull(Sepal.Length) %>% sd()

#Ver N.A data
mean(c(1, 2, NA, 3, 4, 5))

#Para obtener la media sin que los NA nos den otro valor
mean(c(1, 2, NA, 3, 4, 5),  na.rm = TRUE)

#Media de sepal.length
iris %>% pull(Sepal.Length) %>% mean()

#Media de sepal.length con recorte de 10% de cada extremo
iris %>% pull(Sepal.Length) %>% mean(trim = .1)

#Obtener resumen de todas las caracteristicas numericas
iris %>% summarize_if(is.numeric, mean)
iris %>% summarize_if(is.numeric, sd)
iris %>% summarize_if(is.numeric, list(min = min, median = median, max = max))


#Desviacion absoluta media
iris %>% summarize_if(is.numeric, mad)


# AGRUPACION

#Usamos promedios grupales para ver si difieren entre grupos
#Acros es para calcular la media, o mediana segun se vea
# group_by, agrupa de species todas las que hay en ellas
iris %>% group_by(Species) %>% summarize(across(Sepal.Length, mean))
iris %>% group_by(Species) %>% summarize_all(mean)

#Ahora provemos la ANOVA, que es el analisis de varianza

#la suma de cuadrados, los grados de libertad y las estimaciones de los efectos
res.aov <- aov(Sepal.Length ~ Species, data = iris)
summary(res.aov)
#se utiliza para realizar pruebas de comparaciones múltiples de
#medias utilizando el método de Tukey en un análisis de varianza (ANOVA).
TukeyHSD(res.aov)


# DATOS TABULARES

#Contamos la cantidad de datos de species segun su grupo
iris %>% group_by(Species) %>% summarize(n())

#Discretizamos los datos usando cut
iris_ord <- iris %>% mutate_if(is.numeric,
                               function(x) cut(x, 3,
                                               labels = c("short", "medium", "long"),
                                               ordered = TRUE))
summary(iris_ord)

#La tabulacion cruzada se usa para averiguar si dos caracteristicas
#discretas estan relacionadas
tbl <- iris_ord %>% select(Sepal.Length, Species) %>% table()
tbl


#Calcular tabla cruzada con tidyverse
iris_ord %>%
  select(Species, Sepal.Length) %>%
  ### Relationship Between Nominal and Ordinal Features
  pivot_longer(cols = Sepal.Length) %>%
  group_by(Species, value) %>% count() %>% ungroup() %>%
  pivot_wider(names_from = Species, values_from = n)

#Pruea estadistica para determinar si existe una relacion significativa
#entre las dos caracteristicas
tbl %>% chisq.test()

fisher.test(tbl)



# PERCENTILES (CUANTILES)

iris %>% pull(Petal.Length) %>% quantile()

#Probando rango intercuartilico
iris %>% summarize(IQR =
                     quantile(Petal.Length, probs = 0.75)
                   -
                     quantile(Petal.Length, probs = 0.25))



# VISUALIZACION

#HISTOGRAMA

ggplot(iris, aes(Petal.Width)) + geom_histogram(bins = 20)

#DIAGRAMA DE CAJA

# Los bigotes (líneas verticales) abarcan típicamente 1,4 veces el rango
#intercuartílico. Los puntos que quedan fuera de ese rango suelen ser valores
#atípicos que se muestran como puntos.
ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_boxplot()

#Para comprar la distribucion de las cuatro caracteristicas,
#tranformamos los datos a formato largo. Es decir, todos los valores
#de caracteristicas se combinan en una sola columna

library(tidyr)
iris_long <- iris %>% mutate(id = row_number()) %>% pivot_longer(1:4)

ggplot(iris_long, aes(name, value)) + 
  geom_boxplot() +
  labs(y = "Original value")

#Comparar antes de agruparlas
library(tidyr)
iris_long_scaled <- iris %>% scale_numeric() %>%
    mutate(id = row_number()) %>% pivot_longer(1:4)
ggplot(iris_long_scaled, aes(name, value)) + 
  geom_boxplot() +
  labs(y = "Scaled value")


# DIAGRAMAS DE DISPERSION

# Los diagramas de dispersion muestran la relacion entre dos caracteristicas continuas
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + 
  geom_point()


# MATRIZ DE GRAFICO DE DISPERSION

#La matriz de diagramas de dispersion muestra la relacion entre varias
#caracteristicas

library("GGally")
ggpairs(iris,  aes(color = Species))


# Visualizacion de matriz de datos

#La visualizacion de matriz muestra los valores dela matriz una 
#Escala de colores
iris_matrix <- iris %>% select(-Species) %>% as.matrix()

#Necesitamos el formato largo para tidyverse
iris_long <- as_tibble(iris_matrix) %>% mutate(id = row_number()) %>% pivot_longer(1:4)
head(iris_long)

#1era forma
ggplot(iris_long,
       aes(x = name, y = id, fill = value)) + geom_tile()

#Otra forma
library(seriation)
ggpimage(iris_matrix, prop = FALSE)

#Podemos escalar las características a puntajes z para que sean más comparables.

iris_scaled <- scale(iris_matrix)
ggpimage(iris_scaled, prop = FALSE)

#Esto revela bloques rojos y azules. Cada fila es una flor y
#las flores en el conjunto de datos de Iris están ordenadas
#por especies. Los bloques azules para las 50 flores superiores
#muestran que estas flores son más pequeñas que el promedio
#para todas menos Sepal.Width y los bloques rojos muestran
#que las 50 flores inferiores son más grandes para la mayoría
#de las características.

#Juntando
ggpimage(iris_scaled, order = seriate(iris_scaled), prop = FALSE)


# MATRIZ DE CORRELACION
#Seleccionamos todos los datos menos species, lo hacemos matriz
# y por ultimo hallamos la correlacion
cm1 <- iris %>% select(-Species) %>% as.matrix %>% cor()

library(ggcorrplot)
#Mostramos un mapa de calor
ggcorrplot(cm1)

#Vemos otro
gghmap(cm1, prop = TRUE)

#Tambien podemos calcular la transposicion de la matriz de datos
cm2 <- iris %>% select(-Species) %>% as.matrix() %>% t() %>% cor()
#Las correlaciones de objeto a objet se pueden utilizar como
#una medida de similitud. Los bloques de rojo oscuro indican
#diferentes especies
ggcorrplot(cm2)


# GRAFICO DE COORDENADAS PARALELOS


#pueden visualizar varias caracteristicas en un solo grafico
library(GGally)
ggparcoord(iris, columns = 1:4, groupColumn = 5)

#Se puede mejorar reordenando las variables para colocar 
#caracteristicas correlacionadas una al lado de otra
o <- seriate(as.dist(1-cor(iris[,1:4])), method = "BBURCG")
get_order(o)
ggparcoord(iris, columns = get_order(o), groupColumn = 5)

