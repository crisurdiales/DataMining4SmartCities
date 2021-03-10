#Ejemplo de regresion lineal con el dataset Ecommerce Customers.csv (GitHub)
#El dataset incluye una serie de parámetros sobre clientes de una tienda online. 
#El objetivo será predecir cuánto gastará anualmente un cliente en dicha tienda en 
#función de los parámetros de que disponemos. 
library(ggplot2)

df = read.csv("Ecommerce Customers.csv", header = TRUE)
#de estos datos, las columnas con texto no son interesantes a priori y tienen 500 niveles
#(aunque se podria clusterizar la direccion para ver zonas), asi que las quitamos.
df<-df[,-c(1:2)] #el color del avatar podria ser relevante, 
#pero casi seguro con relacion no lineal con ninguna variable. Aun asi lo voy a conservar por ahora, pero en numero
df$AvatarNum<-as.numeric(df$Avatar) #guardo la columna para ver equivalencias, pero en prediccion uso el numero

#veamos ahora las relaciones entre variables con GGpairs


library(GGally) #soporte a ggplot para crear matrices de plots
ggpairs(data=df, columns=c(2:7), title="Relacion entre variables numericas ECommerce")
#a priori, se observa que solo hay correlacion significativa entre el tiempo de membresia y el gasto anual (0.809)
#y, mucho mas baja, entre el tiempo de sesion  o tiempo en la app y el gasto anual (0.355 y 0.499 respectivamente). 
#Vamos a comprobar si la relacion es mas o menos lineal.
scatter.smooth(df$Avg..Session.Length, df$Yearly.Amount.Spent, main='Gasto vs Tiempo Medio de Sesion')
scatter.smooth(df$Time.on.App, df$Yearly.Amount.Spent, main='Gasto vs tiempo en la App')
scatter.smooth(df$Length.of.Membership, df$Yearly.Amount.Spent, main='Gasto vs tiempo de membresia')
#La relacion con tiempo de membresia tiene cierta linealidad y podria merecer la pena probar con el tiempo en la app
#aunque el ajuste no sera muy bueno

#miremos tambien relacion con el color del avatar como contraejemplo. Se ve que el ajuste lineal no es bueno
scatter.smooth(df$AvatarNum, df$Yearly.Amount.Spent, main='Gasto vs Color del Avatar')


#probemos unas cuantas variaciones con una sola variable

fit_membresia <- lm(Yearly.Amount.Spent ~ Length.of.Membership, data = df)
fit_app <- lm(Yearly.Amount.Spent ~ Time.on.App, data = df)
fit_avatar <- lm(Yearly.Amount.Spent ~ AvatarNum, data = df)

#y comparemos los tres (esperamos que el de membresia sea el mejor)
summary(fit_membresia)
summary(fit_app)
summary(fit_avatar)

#Interpretemos los resultados del summary:

#Residuals (diferencia entre el valor real y el predicho; cuanto mas baja, mejor. Ojo a los maximos y minimos)
#coeficientes: la aproximacion seria y= (Intercept) + varind1*Estimate1 + ...
#(intercept)->valor de la variable dependiente si la independiente es cero
#nombre de variable independiente: pendiente de la recta para la variable,i.e por cada incremento de xi cuanto varia y
#para cada coeficiente, tenemos el error estandar y una fiabilidad
#standard errors: variacion de los coeficientes estimados frente a la variacion de la variable de respuesta
#Hipotesis nula: no hay relacion entre las variables
#t-value: (estimate/error estandar) i.e. cuantas desviaciones estandar tiene el coeficiente estimado respecto a 0. Si es pequeño, la hipotesis nula es cierta
#p-value: lo contrario: si p-value es menor de 0.05, hipotesis nula es falsa. Debe ser menor que 0.05 usualmente.
#si cualquier coeficiente tiene p-value alto o t-value bajo, habria que eliminarlo de la prediccion

#model fit
#Residual estandar error: como de lejos caen las observaciones de la regresion. Debería ser bajo
#los grados de libertad son numero de observaciones - numero de predictores - 1
#Multiple R-Squared- coeficiente de determinacion. Indica la proporcion de varianza 
#de la variable dependiente que explican las variables independientes. Mejor cuanto mas cercano a 1, 
#R2 siempre crece con el numero de variables, por eso se ajusta a dicho numero con
#Adjusted R-Square: Este sera menor que R-Squared cuantas mas variables predictoras haya
#F-statistic: indica si la regresion se ajusta mejor a los datos que un modelo sin variables independientes 
#es decir, si vale para algo. F debe ser alto y va asociado al p-value comentado antes (<0.05)

#En nuestro caso solo la variable membresia funciona pasablemente. app no ajusta bien
#el intercept y devuelve un R2 muy bajo, aunque su p-value sea razonable. En avatar el p-value es muy, muy alto

#Con esto sacamos nuestros resultados en formato mas compacto
coef(summary(fit_membresia))
coef(summary(fit_app))
coef(summary(fit_avatar))


#Podemos ver los residuos, que deben seguir una distribucion NORMAL, o la prediccion no es buena

ggplot(data=df, aes(fit_membresia$residuals)) +
  geom_histogram(binwidth = 10, color = "black", fill = "blue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histograma de Residuos del modelo basado en tiempo de membresia")
#Se comporta pasablemente bien en torno al cero

ggplot(data=df, aes(fit_app$residuals)) +
  geom_histogram(binwidth = 10, color = "black", fill = "red4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histograma de Residuos del modelo basado en tiempo en app")
#tambien es aceptable

ggplot(data=df, aes(fit_avatar$residuals)) +
  geom_histogram(binwidth = 10, color = "black", fill = "green4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histograma de Residuos del modelo basado en color del avatar")
#los residuos encajan, dentro de lo razonable, pero se ajustan peor a una normal

#veamos los modelos ahora
ggplot(data = df, aes(x = Length.of.Membership, y = Yearly.Amount.Spent)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Modelo lineal ajustado al tiempo de membresia") #El ajuste es razonable

ggplot(data = df, aes(x = Time.on.App, y = Yearly.Amount.Spent)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Modelo lineal ajustado al tiempo en la app") #Este tiene mucha dispersion, aunque sea algo lineal


ggplot(data = df, aes(x = AvatarNum, y = Yearly.Amount.Spent)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Modelo lineal ajustado al color de avatar") #Este no tiene mucho sentido, es solo una media
#es decir, independientemente del color del avatar, siempre se gasta lo mismo: 500

#Es decir, el unico modelo aceptable en todo se basa en el tiempo de membresia, que de hecho era el que tenia
#correlacion mayor. Con eso podemos predecir el gasto aproximado anual del usuario. 
#Ojo que va en años y solo se ha ajustado a 6.9 max

predict(fit_membresia, data.frame(Length.of.Membership = 4))  #4 años->529 de gasto

#la pregunta final es, si el tiempo en app no era tan malo, ¿mejoraria predecir con membresia y tiempo en app a la vez?
fit_total <- lm(Yearly.Amount.Spent ~ Length.of.Membership + Time.on.App, data = df)
summary(fit_total) #parece que si, si nos fijamos en el R2. 

#Podriamos juntar todos los modelos para verlo mejor

total<- list()
total$membresia <- fit_membresia
total$app <- fit_app
total$avatar <- fit_avatar
total$completo <- fit_total
do.call(cbind, lapply(total, function(z) summary(z)$adj.r.squared)) #uso el ajustado porque son 2 variables
#ver aqui para otros parametros 
#https://statisticsglobe.com/r-extract-f-statistic-predictors-degrees-of-freedom-regression-model


#ahora podemos predecir (mejor) con los dos parametros. Time on App va entre 8.5 y 15.1 en nuestro df
#por ejemplo para ver si gastan mas los que echan mas tiempo
predict(fit_total, data.frame(Length.of.Membership = 4, Time.on.App = 8))  #374.824
predict(fit_total, data.frame(Length.of.Membership = 4, Time.on.App = 15))  #640.72

library(scatterplot3d) #visualizacion avanzada

#probemos a representar el plano de prediccion ahora (con 3d no se va a poder ver)

Membresia <- seq(0.5,8.5, by=0.1) 
TiempoApp <- seq(8.5,15.5, by=0.1) ## ojo. Comprobar los rangos
pred_grid <- expand.grid(Length.of.Membership = Membresia , Time.on.App = TiempoApp)
pred_grid$gasto <-predict(fit_total, new = pred_grid)

fit_2_sp <- scatterplot3d(pred_grid$Length.of.Membership, pred_grid$Time.on.App, pred_grid$gasto, angle = 80, 
                          color = "dodgerblue", pch = 1, ylab = "Tiempo en app", xlab = "Tiempo membresia", zlab = "gasto" )

fit_2_sp$points3d(df$Length.of.Membership, df$Time.on.App, df$Yearly.Amount.Spent, pch=16)  #ajusta bien :)
