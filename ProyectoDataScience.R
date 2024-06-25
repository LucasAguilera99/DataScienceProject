install.packages("readxl")
install.packages("GGally")
install.packages("ggplot2")
install.packages("DataExplorer")
install.packages("networkD3")
install.packages("tibble")
install.packages("dplyr")
install.packages("plotly")
install.packages("cluster")
install.packages("factoextra")
install.packages("NbClust")
library(factoextra)
library(NbClust)
library(cluster)
library(plotly)
library(dplyr)
library(tibble)
library(networkD3)
library(DataExplorer)
library(readxl)
library(GGally)
library(ggplot2)
setwd("C:/Users/Lucas/Desktop/ProyectoDataScience")


#Ingreso de datos

file_path <- "C:/Users/Lucas/Desktop/ProyectoDataScience/BaseMuestra.xlsx"
datos <- read_excel(file_path)

datos <- as_tibble(datos)


datos$Puntos <- as.factor(datos$Puntos)

## Creemos que este grafico nos muestra como los puntos y asistencias influyen en la valoracion promedio por partido.

plotly::plot_ly(datos, x = ~datos$Puntos, y = ~datos$Asistencias, z = ~datos$ValoracionPromedioPartido, 
                color = ~EQUIPO, size = 1) |> 
  plotly::add_markers()

## Grafico que muestra valoraciones totales y promedio por partido en relacion a los partidos jugados
## Permite ver que no necesariamente hay que jugar muchos partidos para tener una buena valoracion
## Si no, que rendir en los partidos jugados, esto se evidencia en jugadores que poseen una valoracion
## mayor con 8 partidos a jugadores que tienen 11 partidos.


summary(datos)

# Comparando min y Max de los distintos valores, teniendo en consideracion mediana y promedio, se puede decir que no
# existen outliers o problemas en los datos, ya que algunos valores que son 0 significan jugadores que no jugaron partidos
# y tambien existen muchos jugadores que no tuvieron mas de 2 o 3 partidos.

ggplot(datos, aes(x = as.numeric(Puntos))) +
  geom_density(fill = "cyan", alpha = 0.5) +
  labs(title = "Densidad de Puntos", x = "Puntos", y = "Densidad") +
  theme_minimal()

ggplot(datos, aes(x = as.numeric(Puntos))) +
  geom_histogram (fill = "black", color = "pink") +
  labs(title = "Histograma de puntos realizados", x = "Puntos", y = "Frecuencia") +
  theme_minimal()

# AVANCE PRESENTACION
#NORMALIZAR LAS CIFRAS

datos <-scale(datos)

# CALCULAR DISTANCIAS

m.distancia <- get_dist(datos, method = "euclidean")
fviz_dist(m.distancia, gradient = list(low = "blue",mid = "white", high="red"))

## SE BORRAN LOS NaN Values

datos %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

#Estimar cantidad de clusters

fviz_nbclust(datos, kmeans, method = "wss")
fviz_nbclust(datos, kmeans, method = "silhouette")
fviz_nbclust(datos, kmeans, method = "gap_stat")

#Se  calcula con 5 clusters iniciales debido a las estimaciones previas

k5 <- kmeans(datos, centers = 5, nstart = 25)
k5
str(k5)

#plotear los cluster
fviz_cluster(k5, data = datos)
fviz_cluster(k5, data = datos, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k5, data = datos, ellipse.type = "norm")
fviz_cluster(k5, data = datos, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

res2 <- hcut(datos, k = 5, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF"))


#Pasos finales

datos %>%
  mutate(Cluster = k5$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

df <- datos
df
df$clus<-as.factor(k5$cluster)
df
