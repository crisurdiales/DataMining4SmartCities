#Ejemplo de aplicacion del Naive Bayes para el dataset diabetes.csv
#es naive porque asume independencia en las variables de predicción, lo que no suele ser cierto
#bueno con salida categorica, es decir SI/NO o similar. Se usa, por ejemplo, para detectar spam

#para forzar instalaciones: library(devtools)
#install_github("juba/questionr")
#Bayes.> prob. posterior = prob. a priori x probabilidad condicional / evidencia
#post-> probabilidad de muestra en clase C 
#prior-> basado en evidencia, probabilidad de observar muestra X si la clase es C
#prob. cond->prob. observar el predictor 
#evidencia->prob de que se produzca la muestra X

library(e1071) #para el naive bates
library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(rpart)
library(randomForest)

df = read.csv("diabetes.csv", header = TRUE)
#https://www.kaggle.com/saurabh00007/diabetescsv
#['Pregnancies', 'Glucose', 'BloodPressure', 'SkinThickness', 'Insulin','BMI', 'DiabetesPedigreeFunction', 'Age', 'Outcome']
#‘Outcome’ is the column which we are going to predict, which says if the patient is diabetic or not. 1 means the person is diabetic and 0 means a person is not. We can identify that out of the 768 persons, 500 are labeled as 0 (non-diabetic) and 268 as 1 (diabetic)
#pasemos outcome a variable categorica:
#son todo enteros o num, pero queremos resultado categorico (si o no)
df$Outcome <- factor(df$Outcome, levels=c(0,1), labels=c("False", "True"))
str(df)
describe(df)  # requiere paquete psych

#fijate que los minimos en glucosa, presión, insulina y tal son 0. Eso es imposible, asi que se cambian por NA
df[, 2:7][df[, 2:7] == 0] <- NA

#visualizamos los datos
missmap(df)  #libreria Amelia (que datos faltan)

#usemos el mice para interpolar los huecos NA ahora. Necesito mice y RandomForest
library(mice)
mice_mod <- mice(df[, c("Glucose","BloodPressure","SkinThickness","Insulin","BMI")], method='rf')
mice_complete <- complete(mice_mod)

#Transfer the predicted missing values into the main data set
df$Glucose <- mice_complete$Glucose
df$BloodPressure <- mice_complete$BloodPressure
df$SkinThickness <- mice_complete$SkinThickness
df$Insulin<- mice_complete$Insulin
df$BMI <- mice_complete$BMI
#ya no hay huecos

#Data Visualization
#Visual 1
library(ggplot2)
ggplot(df, aes(Age, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by Outcome")

#visual 2
c <- ggplot(df, aes(x=Pregnancies, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="Pregnancy Distribution by Outcome")
c + theme_bw()


#visual 3
P <- ggplot(df, aes(x=BMI, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="BMI Distribution by Outcome")
P + theme_bw()

#visual 4
ggplot(df, aes(Glucose, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Glucose Distribution by Outcome")

library(GGally)
#visual 5 este es el scatter
ggpairs(df) #necesita el GGally


############# Ahora dividimos entre test y train ##############
library(caret)
indxTrain <- createDataPartition(y = df$Outcome, p = 0.75, list = FALSE)  #caret
training <- df[indxTrain,]
testing <- df[-indxTrain,]

#veamos las distribuciones
prop.table(table(df$Outcome)) * 100 
prop.table(table(training$Outcome)) * 100 
prop.table(table(testing$Outcome)) * 100 

#x which van a ser las variables independientes e y la dependiente
x = training[,-9]
y = training$Outcome
library(dplyr)
library(tidyr)
training %>%
  filter(Outcome == "True") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()
#para esto hace falta la libreria corrplot. OJO! ALGUNAS VARIABLES NO SON INDEPENDIENTES!!
#eso es un problema, porque calculamos la prob. de C sobre X como el producto de p(xi/C)
#esto se arregla con un suavizado de Laplace

training %>% 
  select(Age, Pregnancies, Glucose, BloodPressure, Insulin, BMI) %>% 
  gather(metric, value) %>% #esta requiere tydyr
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")
#tampoco son todas variables con distribucion normal

#COSAS A EVITAR: Si no hay valores de un determinado atributo, la prob. condicional de ese atributo es 0
#y nos hace 0 toda la probabilidad.

#aqui va el modelo

features <- setdiff(names(training), "Outcome") #entreno todo menos outcome
x <- training[, features]
y <- training$Outcome

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)

nb.m1 <- train(  #requiere caret Y klaR
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)

#con esto se ve el modelo
nb.m1$finalModel

#evaluemos ahora la prediccion
Predict <- predict(nb.m1,newdata = testing ) #con el segundo set de datos
head(Predict)  #clasificacion; si el individuo tiene o no diabetes
confusionMatrix(Predict, testing$Outcome) #como ya tenemos el Outcome en testing ...

#Intentemos ajustar parametros para encontrar el mejor modelo. Definimos un grid con los 3 parametros
nb_grid <- data.frame(fL=c(0,0.5,1.0), usekernel = TRUE, adjust=c(0,0.5,1.0))
nb_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 1:5,
  adjust = seq(0, 5, by = 1)
)
# Fit the Naive Bayes model 
set.seed(2550)
nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = nb_grid)

#O, ya a lo basto

# train model
nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = nb_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

# top 5 modesl
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# Selected tuning parameters
nb.m2$finalModel$tuneValue
nb.m2$finalModel
plot(nb.m2)

Predict <- predict(nb.m2,newdata = testing ) #con el segundo set de datos
head(Predict)  #clasificacion; si el individuo tiene o no diabetes
confusionMatrix(Predict, testing$Outcome)

#Plot Variable performance
X <- varImp(nb.m2) #amelia. Da la importancia de cada variable independiente en el modelo
plot(X)

Predict2 <- predict(nb.m2,newdata = testing )
confusionMatrix(Predict2, testing$Outcome)
#Ha subido la eficiencia