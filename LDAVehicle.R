#Ejemplo de uso e interpretacion de LDA
#Usamos el dataset vehicle, que contiene una serie de características 
#ya extraídas de imágenes de vehículos que definen su silueta, así como 
#el tipo de vehículo: OPEL, SAAB, BUS, VAN (4 clases). 
#Compararemos con PCA

require(MASS)
require(ggplot2)
require(scales)
require(gridExtra)

library(ggfortify)
library(cluster)

#Cargamos el archivo de https://archive.ics.uci.edu/ml/datasets/Statlog+%28Vehicle+Silhouettes%29

vehicle <- read.csv('vehicle.csv', stringsAsFactors = T)

#el set contiene una serie de caracteristicas extraidas de imagenes de vehiculos, que definen su silueta,
#asi como el tipo de vehiculo que es en cada caso, que puede ser OPEL, SAAB, BUS, VAN (4 clases)

mylda <- lda(Class ~ ., data=vehicle)

prop.lda = mylda$svd^2/sum(mylda$svd^2)

ggplotLDAPrep <- function(x){
  if (!is.null(Terms <- x$terms)) {
    data <- model.frame(x)
    X <- model.matrix(delete.response(Terms), data)
    g <- model.response(data)
    xint <- match("(Intercept)", colnames(X), nomatch = 0L)
    if (xint > 0L) 
      X <- X[, -xint, drop = FALSE]
  }
  means <- colMeans(x$means)
  X <- scale(X, center = means, scale = FALSE) %*% x$scaling
  rtrn <- as.data.frame(cbind(X,labels=as.character(g)))
  rtrn <- data.frame(X,labels=as.character(g))
  return(rtrn)
}

p1<- ggplot(ggplotLDAPrep(mylda), aes(LD1,LD2, color=labels)) + geom_point() + 
  stat_ellipse(aes(x=LD1, y=LD2, fill = labels), alpha = 0.2, geom = "polygon") +
xlab(paste0("LD1 ", round(prop.lda[1], digits=3))) + ylab(paste0("LD2 ", round(prop.lda[2], digits=3)))

p2 <- autoplot(pam(vehicle[,-19], 4), frame = TRUE, frame.type = 'norm')

grid.arrange(p1, p2)






