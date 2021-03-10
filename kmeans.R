#Algoritmo de clasificacionn en k clases. Vamos a emplear el dataset  Country-data.csv
#Este dataset incluye para un conjunto de países información sobre variables relacionadas con la 
#riqueza, como mortalidad infantil, salud, exportaciones, etc. 
#Seria recomendable reducir la dimension de los datos antes de procesar, pero no lo vamos a hacer
#para centrarnos solo en el clustering

library(clusterCrit)  #davies bouldin
library(ggplot2)
library (cluster) #dibujo cluster
library(fpc) #dibujo cluster

#Nuestro dataset tiene el coste de diferentes items en diferentes ciudades. Vamos a intentar hacer grupos por coste de las cosas, es decir
#ciudades en el mismo grupo tienen costes similares de las cosas
coste_ciudades <- read.csv('cost-of-living.csv', header = TRUE, sep = ',') #Tenemos de header los nombres de las ciudades
dim(coste_ciudades)  #55 elementos por 161 ciudades
summary(coste_ciudades)


#en realidad, queremos que las filas sean las ciudades y las columnas las caracter??sticas,
#asi que transponemos la matriz. Me gago funcion para usarla despues. 
#NOTA: La primera columna aquí tiene los nombres de las filas. Si el frame no es asi, la funcion no vale.

###################################################################
###################################################################
trasponer_dataframe <- function (df_trans) {
  
  #nombreciudades <- colnames(kmeans.df) Este no hace falta, traspone bien
  nombrevariables <- df_trans[,1] 
  #tengo que transponer como matriz, no como dataframe, o todo se va a string
  
  df_trans <- as.data.frame(t(as.matrix(df_trans[,-1])))  #esta es la clave. Adaptar si es necesario
  colnames(df_trans) <- nombrevariables #añadir nombre variables
  colnames(df_trans) <- make.unique(names(df_trans))  #esto en caso de que salga problema de ggplot2 nombres unicos  
return(df_trans)
  }
###################################################################
###################################################################


kmeans.df <- trasponer_dataframe(coste_ciudades)
#Ahora clusterizamos
set.seed(20)
k= 10 #por ejemplo, en 10 grupos
cityCluster <- kmeans(kmeans.df, k, nstart=20) 

#esto va asi: para no depender mucho de por que elemento empieza, el sistema prueba 20 configuraciones y se queda con la que de menor variaci??n
#intracluster, es decir, los elementos de cada clase son mas parecidos.
#cityCluster devuelve una lista de en que clase esta cada elemento de las 55 ciudades y una serie de datos, como la distancia 





#El problema del k.means es que no sabemos k a priori. Para calcularla, se puede usar el error intraclases, buscando el k que lo minimice. Voy 
#a hacer esto en funcion, porque lo volvere a usar mas adelante

###################################################################
###################################################################
bestkCluster <- function(dataframe, max_cluster_number) {

clusternumber<-0
#max_cluster_number <- 20 #por ejemplo, ya que hay 50 y pico ciudades
historico_a <-0 #lista de errores
smallest <-99999  #error que minimizo
#Davies Bouldin algorithm
totalm <- data.matrix(dataframe) #necesito los datos en formato matriz para intCriteria

for(k in 2:max_cluster_number){
  a <-99999
  for(i in 1:50){
    cl <- kmeans(totalm, k ,nstart = 20)  #un cluster para b clases
    #cl<-as.numeric(cl)
    intCriteria(totalm,cl$cluster,c("dav"))
    if(intCriteria(totalm,cl$cluster,c("dav"))$davies_bouldin < a){   #si el error para b clases es menor que a, ahora a es el error
      a <- intCriteria(totalm,cl$cluster,c("dav"))$davies_bouldin }
  }
  #me quedo con a para el historico
  historico_a<-rbind(historico_a,a)
  if(a<0.5*smallest){
    smallest <- a  # busco punto de inflexion, no minimo. Si el error es al menos un 25% mas pequeño
    clusternumber <-k  #me quedo con este numero de clusters
  }
}
plot(historico_a) #pinto el historico, por si hay una opcion mejor

return(clusternumber)
}
###################################################################
###################################################################


clusternumber<-bestkCluster(kmeans.df,20) #ya que hay 55 ciudades...

#A veces hay minimos aceptables no absolutos en el plot. Nunca esta de mas comprobar por si acaso. Cuantas menos clases, dentro de lo posible, mejor

cityCluster <- kmeans(kmeans.df, clusternumber, nstart=20) 
#Podemos pintar algo en 2D, no mucho porque tenemos muchas variables
cityCluster$cluster <- as.factor(cityCluster$cluster)  #factorizamos el cluster
ggplot(kmeans.df, aes(kmeans.df$`Meal for 2 People, Mid-range Restaurant, Three-course`, kmeans.df$`Price per Square Meter to Buy Apartment in City Centre`, color = cityCluster$cluster)) + geom_point()
#si miramos componente a componente, se ve todo bastante mezclado


###################################################################
#a ver que podemos pintar con las variables que tenemos

plotcluster(kmeans.df, cityCluster$cluster) #la clase 2 tiene un solo elemento..
clusplot(kmeans.df, cityCluster$cluster, color=TRUE, shade=TRUE,          
         labels=2, lines=0)  #ambos con 2 componentes del PCA, pero el segundo se ve regular por ser tantos datos.
###################################################################


###################################################################
#podemos ver que ciudades hay en un cluster determinado, por ejemplo, el 2. Ojo, todo esto depende del resultado

cityCluster$cluster[cityCluster$cluster==2]

#Podemos buscar, por ejemplo, la distancia del centroide del cluster 1 al 2 y del 2 al 3 para ver como de separados estan
dist(rbind(cityCluster$centers[1,], cityCluster$centers[2,]))
dist(rbind(cityCluster$centers[1,], cityCluster$centers[3,]))
#2 y el resto estan casi 6 veces mas distantes


#Singapur. Tiene bastante sentido, si se consulta la naturaleza de la ciudad
cityCluster$cluster[cityCluster$cluster==1]
#Vietnam, Noruega, Dinamarca, Israel, Tailandia, Islandia, Jordania, Irlanda .. Este es mas curioso, aunque incluye oriente medio, los nordicos y alguno suelto mas

###################################################################
#Para otras distancias -no euclideas, podemos usar el k-medianas. Mismos parametros
#library("Gmedian")
#cityClusterMedian <- kGmedian(kmeans.df, clusternumber, nstart=20) 
###################################################################

###################################################################
#El problema de kmeans es que el centroide no es ninguna ciudad en particular, sino la media de los valores de
#todas las del cluster. 

#Podemos buscar, por ejemplo, la distancia de Katmandu (11) y  Eindhoven (28)a su cluster -el 1-, ya que parece
#raro que las agrupen juntas (precios similares)

dist(rbind(cityCluster$centers[1,], kmeans.df[11,]))
dist(rbind(cityCluster$centers[1,], kmeans.df[28,]))
#en torno a 17800 Katmandu y 7000 Eindhoven. Comprobamos Porto (94) 
dist(rbind(cityCluster$centers[1,], kmeans.df[94,]))
#Unos 7000; el que esta fuera de lugar es Katmandu, como suponiamos. Tal vez Se ha acercado junto con Vietnam, e.g. 13
dist(rbind(cityCluster$centers[1,], kmeans.df[13,]))
#sube a 9500, pero el outlier maximo sigue siendo Katmandu, que no pega que sea tan caro como el resto


###################################################################
###################################################################
#Tratemos de comparar los precios visualmente. 
lenx_plot <- dim(kmeans.df)[2] #numero de puntos en x
chr<-cbind("Katmandu","Eindhoven","Porto", "Vietnam")
chr<- rep(chr, each=lenx_plot)  #etiqueto con su nombre todas las filas
pos <- rep(1:lenx_plot, 4)
cov <- c(as.numeric(kmeans.df[11,]), as.numeric(kmeans.df[28,]), as.numeric(kmeans.df[94,]), as.numeric(kmeans.df[13,]))
comp_df  <- data.frame(chr, pos, cov)

p <- ggplot(data = comp_df, aes(x=pos, y=cov)) + geom_area(aes(fill=chr))
p + facet_wrap(~ chr, ncol=1)
#Se comportan bastante similares. Podemos comparar Katmandu-11(clase 1) con Singapur clase 2, Quito clase 3 y Melbourne clase 4
which((rownames(kmeans.df)=="Singapore..Singapore")==TRUE) #40
which((rownames(kmeans.df)=="Quito..Ecuador")==TRUE) #160
which((rownames(kmeans.df)=="Melbourne..Australia")==TRUE) #156

chr2<-cbind("Katmandu","Singapur","Quito", "Melbourne")
chr2<- rep(chr2, each=lenx_plot)  #etiqueto con su nombre todas las filas
cov2 <- c(as.numeric(kmeans.df[11,]), as.numeric(kmeans.df[40,]), as.numeric(kmeans.df[160,]), as.numeric(kmeans.df[156,]))
comp_df2  <- data.frame(chr2, pos, cov2)

p2 <- ggplot(data = comp_df2, aes(x=pos, y=cov2)) + geom_area(aes(fill=chr2))
p2 + facet_wrap(~ chr2, ncol=1)

#ahora si se ven diferencias en los picos, en torno al rasgo 22, 37 y 55
#el 22, poco sorprendentemente, es el precio de un apartamento en el centro y el 37 el precio del metro cuadrado en el centro
colnames(kmeans.df)[22]
colnames(kmeans.df)[37]
#El maximo final es
which (kmeans.df[156,]==max(kmeans.df[156,40:55])) #el 53, es decir

colnames(kmeans.df)[53] #precio de un toyota corolla

###################################################################
###################################################################
###################################################################
###################################################################

#Problema: en los propios plots se ve que practicamente las cosas se agrupan solo por los picos. Logico, porque no se puede comparar el precio
#de un piso con el de una hamburguesa. Es hora de hacer un z-score a kmeans.df Ojo: lo que quieres es que los precios en la misma ciudad sean
#comparables, es decir, el score se aplica por filas. Por tanto, en lugar de escalar kmeans.df, escalamos directamente coste_ciudades y LUEGO trasponemos

ciudades.scaled <- coste_ciudades[,apply(coste_ciudades[,-1],2,var) !=0] #asegurar que tienen varianza: no puedes dividir por 0
for(i in seq(2,ncol(ciudades.scaled))) ciudades.scaled[,i] <- scale(ciudades.scaled[,i]) #lo hago con for porque si no hay que pasar el df a matriz y vuelta

kmeans.df.scaled <- trasponer_dataframe (ciudades.scaled)

#ahora repetimos el k medias y probablemente no salga igual.
kbestscaled<- bestkCluster(kmeans.df.scaled,20)

cityClusterScaled <- kmeans(kmeans.df.scaled, kbestscaled, nstart=20) 
#3 clases ahora. Mas sensato que separar solo Singapur..

###################################################################
###################################################################
#pintamos

plotcluster(kmeans.df.scaled, cityClusterScaled$cluster) 
clusplot(kmeans.df.scaled, cityClusterScaled$cluster, color=TRUE, shade=TRUE,          
         labels=2, lines=0) #ahora 2 componentes explican el 94.35% de variabilidad
###################################################################
###################################################################

#Pintemos una de cada
#Tratemos de comparar los precios visualmente. 
lenx_plot <- dim(kmeans.df.scaled)[2] #numero de puntos en x

#Se comportan bastante similares. Podemos comparar Katmandu-11(clase 1) con Singapur clase 2, Quito clase 3 y Melbourne clase 4
which((rownames(kmeans.df)=="Rome..Italy")==TRUE) #15  #grupo 1
which((rownames(kmeans.df)=="Delhi..India")==TRUE) #115  #grupo 2
which((rownames(kmeans.df)=="Madrid..Spain")==TRUE) #109  #grupo 3

chr<-cbind("Roma","Delhi","Madrid")
chr<- rep(chr, each=lenx_plot)  #etiqueto con su nombre todas las filas
pos <- rep(1:lenx_plot, 3)
cov <- c(as.numeric(kmeans.df[15,]), as.numeric(kmeans.df[115,]), as.numeric(kmeans.df[109,]))
comp_df  <- data.frame(chr, pos, cov)

p <- ggplot(data = comp_df, aes(x=pos, y=cov)) + geom_area(aes(fill=chr))
p + facet_wrap(~ chr, ncol=1)

#aparece un pico en el coste de la escuela primaria anual. Apuntan tambien los precios basicos de electricidad, agua, basura...
#claramente hay 3 niveles de vida

