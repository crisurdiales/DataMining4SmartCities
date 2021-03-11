#Algoritmo de clasificacion por decision tree y random forest. Usaremos el 
#dataset hotel_bookings.csv con 25 variables sobre reserva de hoteles, para
#intentar predecir cuando alguien va a cancelar

df = read.csv("hotel_bookings.csv", header = TRUE)
#Tenemos la columna is_canceled y podemos quitar reservation status y reservation_status_date
#quito tambien company, porque mayoritariamente es NULL: summary(df$company=="NULL") 112593 son NULL
df<- within(df, rm(reservation_status_date, reservation_status, company))

df<-df[!(df$agent=="NULL"),] #Quito los que tienen NULL en agente, son solo 16340
df$agent<- factor(df$agent) #con esto elimino el nivel

#Las columnas que son factores numericos las paso a numero, por la legibilidad del arbol, que si no
#escribe todos los niveles al corte

df$agent <- as.numeric(as.character(df$agent))

#Los paises tienen demasiados factores, tengo que reducirlos, 
#Los que aparacen mas de 1000 veces los preservo. Son los 13 primeros: PRT, GBR, FRA, ESP, DEU, ITA,...
#El resto los paso a Others
tableFrq <- data.frame(table(df$country ))
tableFrq <- tableFrq[order(-tableFrq$Freq),]
Change <- tableFrq$Var1[14:178] #he comprobado que despues del 13 son menos
levels(df$country) <- c(levels(df$country), "Other")
df$country[which(df$country %in% Change)] = "Other"
df$country<- factor(df$country) #con esto elimino los niveles que sobran


df$is_canceled<-as.factor(df$is_canceled) #paso a factor la decision

set.seed(101)
train=sample(1:nrow(df), 70000) #70% aprox

library(rpart)
res_tree<- rpart(is_canceled~. , df, subset=train, cp=0.02)

rpart.rules(res_tree) #presentar como regla el % de cancelacion

library(rattle)
#ver parametros ?rpart.plot::prp
#par(mar=c(0.1,1,1,1))
fancyRpartPlot(res_tree, cex=0.8, space=1.5)
#el grafico se lee asi. YES iqda, NO derecha
#Root. De los que cancelaron el 39% perdieron deposito 
#El 
#a la izqda, de los del 31% con cancelacion, un 2% tenia cancelaciones previas
#de los que tenian menos de 0.5, el 54% habian reservado online o no definido

#predecir
tree.pred = predict(res_tree, df[-train,], type="class")
with(df[-train,], table(tree.pred, is_canceled)) #acierta un 88%

#Podemos probar ahora un Random Forest a ver si mejora
library(randomForest)
rf.res = randomForest(is_canceled~., data = df, subset = train, na.action=na.exclude)
forest.pred = predict(rf.res, df[-train,], type="class")
with(df[-train,], table(forest.pred, is_canceled)) #acierta un 88.9%

