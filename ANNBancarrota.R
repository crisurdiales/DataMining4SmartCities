#Ejemplo de aplicacion de RNA para predecir la bancarrota de una empresa
#Practica tomada de Matteus Dadej: https://www.kaggle.com/squintrook/forecasting-bankruptcies-with-ann
#Descarga de datos: https://archive.ics.uci.edu/ml/machine-learning-databases/00365/

#Para empezar, hay que cargar y preparar los datos para su procesado
#usamos todos los años. Los atributos incluyen datos economicos
#de la compañia, como beneficios totales, netos, intereses ...
#Son todos numericos
#Más explicacion de los campos: https://archive.ics.uci.edu/ml/datasets/Polish+companies+bankruptcy+data#
#La variable a predecir (class) es binaria: si la compañia entro en bancarrota o no, y va por años segun el fichero
#1st year es datos de 2007 y bancarrota (o no) en 2013, 2nd year datos 2008 y bancarrota 2013 y asi sucesivamente
#nos vamos a limitar a predecir a 5 años (2007 a 2013).

library(foreign) #lee arff
library(neuralnet)
library(nnet)
library(tidyr)
library(dplyr)
set.seed(2)

po1 <- read.arff("5year.arff")
po2 <- read.arff("4year.arff")
po3 <- read.arff("3year.arff")
po4 <- read.arff("2year.arff")
po5 <- read.arff("1year.arff")

#Pasamos a eliminar los datos que no sabemos (NA)

po1 <- drop_na(po1)
po2 <- drop_na(po2)
po3 <- drop_na(po3)
po4 <- drop_na(po4)
po5 <- drop_na(po5)

#Ahora balanceamos los datos para que el numero de bancarrotas y no bancarrotas
#sea mas similar. Esto es habitual en RNA para evitar que "apuesten por lo 
#seguro", es decir, que si hay muchas mas bancarrotas que no, la prediccion tienda a 
#ser siempre bancarrota o viceversa

#se puede ver que no hay balanceo con un summary(datos). Por ejemplo, en po1 hay 2929 
#que no quiebran y solo 102 que si. Por suerte, los datos estan agrupados

po1 <- po1[2828:3031,] #si se miran los datos ahora, hay 102 clase 1 y 102 clase 0
po2 <- po2[4530:4769,]
po3 <- po3[4672:4885,]
po4 <- po4[3943:4088,]
po5 <- po5[3135:3195,]

df <- rbind(po1,po2,po3,po4,po5) #agrupamos todos los datos
#Vamos a hacer un hot-one-encoding, que es apropiado para manejar bits de estado
#y muy frecuente en ANN. En pocas palabras, en lugar de un bit para decidir si hay
#bancarrota (1) o no (0), pasamos a dos bits, uno solo para bancarrota y otro para no
#bancarrota, que valen 0 o 1 segun el estado este activo. En RNA se traduce facilmente 
#en 2 neuronas de salida (en este caso nunca seran 0 o 1 a la vez)

class_2 <- matrix(rep(0,nrow(df)), nrow = nrow(df), ncol = 1) #crear matriz salida

class_2 <- df$class #rellenar con las salidas que tenemos. Son factores, no una matriz
class_2 <- as.matrix(class_2)  #Pasar a matriz. Ahora son caracteres "0" o "1"
class_2 <- as.numeric(class_2) #pasar a numero

class_2[class_2==0] <- 2
class_2[class_2==1] <- 0
class_2[class_2==2] <- 1

#Llegados a este punto, class y class_2 son contrarios, es decir, o uno o el otro
#estan activos, representando el estado. Existen librerias que hacen esto de forma automatica

df <- cbind(df,class_2) #pegamos en hot-1-encoding al original
df <- drop_na(df)

#el siguiente paso es escalar los datos de entrada para que todas las variables vayan
#de 0 a 1. De no hacerlo asi, la estructura favoreceria a unas mas que a otras

#calculamos maximos y minimos por columnas
df<- apply(df,2, as.numeric)
maximo <- apply(df, 2, max, na.rm=TRUE)
minimo <- apply(df, 2, min, na.rm=TRUE)

#escalamos y pasamos de nuevo a dataframe
scaled <- as.data.frame(scale(df, center = minimo, scale = maximo-minimo))

#dividimos en dos sets: traning y testing 
indices <- sample(1:nrow(scaled),round(0.7*nrow(scaled)))

training_ <- scaled[indices,]
testing_  <- scaled[-indices,]

#ahora entrenamos la red. No vamos a entrar en teoria de redes, que llevaria un curso entero

n <- names(training_)
n <- n[1:64]

#en f vamos a montar el parametro formula, que viene a decir que clase y clase de salida
#dependen de todos los atributos que tenemos (menos de ellos mismos, claro)
f <- as.formula(paste("class + class_2 ~", 
                      paste(n[!n %in% "class,class_2"], collapse = " + ")))
#con esta formula, entrenamos la red. Configuramos 1 capa oculta con 10 neuronas
#indicamos act.fct logistica para predecir variables binarias (si no, seria regresion)
nn <- neuralnet(f,data=training_,hidden=c(10),
                act.fct = "logistic",linear.output=FALSE, 
                lifesign = "minimal",stepmax = 1e+6,threshold = 0.1,
                rep = 1, algorithm = "rprop+")
#stepmax maximum steps for training
#rep numero de repeticiones del entrenamiento
#threshold en que punto se para el entrenamiento (pendiente del error)
#lifesign-> feedback durante el entrenamiento
#no se yo si linear.output deberia ser TRUE...
#se puede pintar, pero con tantos atributos la visualizacion no es buena
plot(nn, rep="best")

#ahora, con la red entrenada, pasamos a ver el error de prediccion con el set
#que no conoce (para evitar el efecto del overfitting)

pr.nn <- neuralnet::compute(nn, testing_[, 1:64])
pr.nn_ <- pr.nn$net.result
head(pr.nn_) #la prediccion es muy cercana enteros para ambas salidas, pero lleva decimales
pr.nn_2 <- max.col(pr.nn_) #redondeamos hacia arriba

original_values <- max.col(testing_[,65:66]) #estas son las salidas originales de los datos de testeo

mean(pr.nn_2 == original_values) #veamos la media de los aciertos
#sale 0.84, es decir, un 84% de exito en prediccion

#podemos sacar la matriz de confusion para mas datos
MatConf  <- as.matrix(table(Truth = original_values, Prediction = pr.nn_2))
TruPos <- MatConf[1,1]
TruNeg <- MatConf[2,2]
FalPos <- MatConf[1,2]
FalNeg <- MatConf[2,1]
TruPosRate <- TruPos/(TruPos+FalNeg)
TruNegRate <- TruNeg/(FalPos+TruNeg)
precision  <- TruPos/(TruPos+FalPos)
accuracy   <- (TruPos + TruNeg)/(length(original_values))


tabla.MatConf <- data.frame(
  TruPos,TruNeg,FalPos,FalNeg,TruPosRate,TruNegRate,accuracy)

#tabla.MatConf muestra un 86% de aciertos en TP y un 81% en TN, es decir, acierta mas
#las bancarrotas que las no bancarrotas. La accuracy (los aciertos) es del 84%
#la precision,sin embargo, solo del 81% (considera los falsos positivos)
