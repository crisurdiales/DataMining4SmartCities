#Regresion con resultado binario. Dataset = https://www.kaggle.com/fayomi/advertising
df = read.csv("advertising.csv", header = TRUE) #1000 obj. 10 variables. Target= Clicked.on.Ad

str(df) #hay muchos factores con demasiados niveles, los paso a numero

#indx <- sapply(df, is.factor) #columnas factores
#df[indx] <- lapply(df[indx], function(x) as.numeric(as.character(x)))
#no funciona. ???
df$Ad.Topic.Line<-as.numeric(df$Ad.Topic.Line)
df$Country<-as.numeric(df$Country)
df$City<-as.numeric(df$City)
df$Timestamp<-as.numeric(df$Timestamp)
ggpairs(df) #veamos relaciones
library(GGally)
ggcorr(df, method = c("everything", "pearson")) #ver correlaciones

#solo son significativas Daily.Time.Spent.on.Site+Age+Area.Income+Daily.Internet.Usage

#Crear sets entrenamiento (70%) y test (30%)
set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train <- df[sample, ]
test <- df[!sample, ] 
#ajustamos la regresion glm= general lineal model family= binomial->logistica
model <- glm(Clicked.on.Ad~ Daily.Time.Spent.on.Site+Age+Area.Income+Daily.Internet.Usage, family="binomial", data=train)
summary(model)
#se ve que todos los p values son bastante menores de 0.05
#como no devuelve R2, lo sacamos con esta funcion
library("pscl")
pR2(model)["McFadden"] #0.88. No esta mal, aunque podria ser mejor
library("caret")
varImp(model) #la variable menos importante es la edad, pero pesa
library("car")
vif(model) #la colinealidad de las variables. Ningún valor llega a 5, asi que esta bien
#ahora se puede predecir si alguien clicara en el anuncio a partir de los datos entrenados
input <- data.frame(Daily.Time.Spent.on.Site = 65, Age=25, Area.Income = 15000, Daily.Internet.Usage = 120)
#input= tiempo en sitio en la media, edad 25, Area INcome bajo, Uso internet medio-bajo. Lo saco de summary
predict(model, input, type="response") #Si hay click

predicted <- predict(model, test, type="response")
#pasemos a 0 y 1 la respuesta, para ver cuanto acertamos
confusionMatrix(as.factor(test$Clicked.on.Ad), as.factor(round(predicted)))
#La prediccion es bastante buena
library(pROC)
roc_obj<-roc(as.numeric(test$Clicked.on.Ad), predicted)
ggroc(roc_obj) #Area under the curve: 0.9861
