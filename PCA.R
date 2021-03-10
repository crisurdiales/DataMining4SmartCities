install_github("vqv/ggbiplot") 

#Práctica guiada basada en R-bloggers y el fichero housing.data (14 variables de 504 casas de distintas ciudades en EEUU ; última columna: mediana del valor de las casas en miles de USD (MEDV)). 
#Trabajar con tantas variables a la hora de aplicar cualquier algoritmo es complicado y, lo que es más, si las variables están correladas, muchos algoritmos no funcionarán correctamente. 
#Por lo tanto, nuestro objetivo es reducir la dimensión y la correlación entre variables del conjunto de datos. 
#Para ello, usaremos la función nativa prcomp de R. 

#Cargamos el archivo y ajustamos los nombres de las columnas de acuerdo a la descripcion
houses <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data",header = F, na.string = "?")
colnames(houses) <- c("CRIM", "ZN", "INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")

#Aplicamos PCA  a nuestro dataframe, menos la columna MEDV, que es lo que buscariamos predecir (variable dependiente)
pcaHouses <- prcomp(scale(houses[,-14]))
#prcomp nos devuelve una lista con 3 campos: sdev -desviacion estandar de los componentes, rotation- o loadings, una matriz
#con los eigenvalores de las variables- y x -el valor de los datos rotados (i.e. reducidos). Vamos a usar estos ultimos.

print(pcaHouses) #si queremos ver los PCA, es mejor visualizar con print. Muestra la varianza y las rotaciones para cada variable

####################### ESTA PARTE ES LA DE t-SNE################
library(ggfortify) # for autplot support for the clustering
library(cluster) # for pam
data <- houses
rownames(data) <- paste0(c(1:506), " (", houses$MEDV, ")") #por evitar duplicados
cl <- pam(data, 3)  #3 clusters
# learn about the interpretation of the principal componenets:
autoplot(cl, frame = TRUE, frame.type = 'norm', loadings.label = TRUE,
         loadings.colour = "black", loadings.label.colour = "black")

autoplot(cl, frame = TRUE, frame.type = 'norm', label.repel = TRUE,
         label = TRUE, label.colour = "black", shape = FALSE)

library(Rtsne)
library(ggrepel) #para que se separen las etiquetas
set.seed(1234) # reproducibility
tsne <- Rtsne(data, dims = 2, perplexity = 5)
t.df <- as.data.frame(tsne$Y)
colnames(t.df) <- c("V1", "V2")
t.df <- cbind(t.df, Cluster = factor(cl$clustering))
t.df$Precios <- rownames(t.df)
ggplot(t.df, aes(x = V1, y = V2, color = Cluster, label = Precios)) +
  geom_text_repel()

##################################




scoresHouses <- pcaHouses$x #sacamos los componentes principales para todas las casas
#en realidad, no queremos quedarnos con todos los PCA. Como estan ordenados, solo queremos los que mas informacion dan.
#para ello, vamos a ver cuanto explica cada componente con el siguiente plot
plot(pcaHouses, type = "l") #primera inflexion en 2
summary(pcaHouses) #importancia de las componentes. Los tres primeros explican el 60% de varianza aprox

#Tambien podemos visualizar los componentes 2 a 2 en un plano si usamos, por ejemplo, la libreria ggbiplot
#ojo, que hay que instalarla con devtools!!
#ademas, necesita grupos definidos. Voy a probar a clusterizar precios...
#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(pcaHouses, obs.scale = 1, var.scale = 1, 
              groups = factor(round(houses$MEDV/10)), ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)




#Vamos a hacer, por ejemplo, un ajuste lineal del precio de las casas solo con respecto a 4 componentes: scoresHouses[,1:3]
#Para ello usaremos el predictor mas sencillo en R (ajuste lineal (lm)) y le indicaremos que queremos predecir MEDV con ellos
modHouses <- lm(houses$MEDV ~ scoresHouses[,1:4])
summary(modHouses) #R2 = 0.645
#el resumen muestra que los 3 componentes son significativos (p-value), pero no entraremos en este tema ahora
#en su lugar, vamos a comprobar si hubieramos ganado mucho haciendo el ajuste con TODAS las variables

modHousesFull <- lm(MEDV ~ ., data = houses) #esto es, con houses, usar todas las variables para predecir MEDV
summary(modHousesFull) #R2 = 0.741 - Este modelo da un ajuste algo mejor, pero usa 16 variables en vez de 3
#Si el modelo con todas las variables es mucho mejor, es que hay que usar mas componentes

# Compare obs. vs. pred. plots
par(mfrow = c(1,2))
plot(houses$MEDV, predict(modHouses), xlab = "Observed MEDV", ylab = "Predicted MEDV", main = "PCR", abline(a = 0, b = 1, col = "red"))
plot(houses$MEDV, predict(modHousesFull), xlab = "Observed MEDV", ylab = "Predicted MEDV", main = "Full model", abline(a = 0, b = 1, col = "red"))

