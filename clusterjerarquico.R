#Practica guiada de clustering jerarquico. Vamos a emplear el dataset  Country-data.csv
#Este dataset incluye para un conjunto de países información sobre variables relacionadas con la 
#riqueza, como mortalidad infantil, salud, exportaciones, etc. 
#Seria recomendable reducir la dimension de los datos antes de procesar, pero no lo vamos a hacer
#para centrarnos solo en el clustering

df <- read.csv("Country-data.csv")
#necesitamos trabajar con variables numericas, pero el nombre del pais en el dataset
#es un factor. Lo voy a convertir en el nombre de la fila y luego lo elimino
rownames(df)<-df[,1]
df<-df[,-1] #ahora todas las columnas son numericas
#necesitamos escalar la informacion para que las variables sean comparables
df_scaled <- as.data.frame(scale(df))
summary(df_scaled) #medias 0, desviacion estandar 1

dist_mat <- dist(df_scaled, method = 'euclidean') #calculamos matriz de distancias euclideas
#segun el caso, otras distancias pueden funcionar mejor. Comprobad el tipo de dato.

hclust_avg <- hclust(dist_mat, method = 'average') #escogemos el metodo de linkado por medias
par(ps = 8, cex = 1, cex.main = 1) #tamaño de fuente 8ppt
plot(hclust_avg)  #cex cambia el tamaño de letra, para que se vea
#si observamos a que altura se definen las clases, con un numero muy bajo casi no separamos,
#porque hay ciudades muy, muy separadas del resto, como Singapur. Por tanto, cogeremos un 
#numero de clases relativamente alto, por ejemplo, 10

cut_avg <- cutree(hclust_avg, k = 10) #con esto escogemos el numero de clases k

rect.hclust(hclust_avg , k = 10, border = 2:6) #y con esto dibujamos el corte
#abline(h = 10, col = 'red') #y la linea de corte donde ya no se separa mas

library(dendextend) #o, si se desea, se dibujan los clusters en color con esta libreria
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, k = 10)
par(ps = 8, cex = 1, cex.main = 1) #tamaño de fuente 8ppt
plot(avg_col_dend)

#añadamos la clase al df original como una nueva columna

library(dplyr)
df_cl <- mutate(df, cluster = cut_avg)
count(df_cl,cluster) #y veamos los paises en cada clase. Se ven dos grandes clases y el resto sueltos
#La agrupacion tiene cierto sentido: Nigeria, Luxemburgo, Malta/Singapur, Qatar, Haiti, USA...
#eso no significa que Nigeria sea el mejor, solo que es muy distinto al resto. Comparar por ejemplo
#con Singapur para ver las diferencias obvias (fila 114 contra 134) 
#Ahora podemos comprobar condiciones por cluster, por ejemplo, que la mortalidad infantil es un problema en el cluster 1
df_cl[df_cl$child_mort>100,]

#tambien podemos pintar variables con respecto al cluster (en color)
library(ggplot2)
ggplot(df_cl, aes(x=income, y = life_expec, color = factor(cluster))) + geom_point()
#la esperanza de vida crece de forma aproximadamente lineal en los clusters grandes (1 y 2), pero mucho 
#mas abruptamente -con poco crecimiento de ganancias se salvan muchas vidas- en el 1
#de los clusters pequeños se puede sacar poca cosa. Tal vez seria deseable tener un numero mayor
#de clusters y dejar los de 1 o 2 elementos como outliers en este caso o, directamente, eliminarlos del
#dataframe y volver a clusterizar.

