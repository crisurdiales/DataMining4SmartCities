#ejemplo de aplicación de SVM usando el dataframe heart.csv
library(e1071)  #esta libreria (u otras como caret) nos implementa el SVM

heart <- read.csv("heart.csv", sep = ',', header = TRUE)  #cargamos datos, asegurando que la primera fila sea el nombre de las columnas
str(heart) #lo suyo es comprobar que los datos sean numericos o, todo lo mas, factores, que se pueden pasar facilmente a numero
#de hecho, factorizamos el resultado esperado (columna target), que es un numero
heart[["target"]] = factor(heart[["target"]]) #dos clases

#Lo que viene a continuacion es comun a todos los metodos supervisados de aprendizaje
#Dividimos entre datos de entrenamiento y de test, para probar que el modelo funciona con datos que no ha sido entrenado
intrain <- createDataPartition(y = heart$target, p= 0.7, list = FALSE) #esto saca un 70% de indices para entrenamiento
training <- heart[intrain,]  #los indices de intrain se guardan en un subset de entrenamiento
testing <- heart[-intrain,] #con el resto hacemos un subset de testeo


#vamos a entrenar un clasificador con kernel lineal. Normalmente, deberiamos probar varios con varios parametros a ver cual es mejor 
classifier = svm(formula = target ~ .,   #queremos que target se clasifique frente a TODAS las demas columnas, esa es nuestra formula
                 data = training,                                       #entrenamos SOLO con training
                 type = 'C-classification',                        #este tipo es valido para variables dependientes discretas. C va entre 0 e Inf. Puede usarse nu (0-1)
                 kernel = 'linear')                                     #usaremos un kernel lineal

#lsvm devuelve un objeto clase svm, que junto a parametros del modelo contiene la lista de vectores de soporte ($SV) y sus indices ($index)

y_pred = predict(classifier, newdata = testing[-14]) #con el clasificador, ahora predecimos los datos que el modelo no conocecm = confusionMatrix(table(testing$target, y_pred)) #comparamos variable target de los datos test con prediccion, para evaluar modelo

#Si mirais cm, el porcentaje de aciertos lo da accuracy y la fiabilidad de los resultados el p-value
#sensitivity y specificity son los true positives y true negatives, es decir, cuantos enfermos acertaria y cuantos sanos descartaria
#podeis ver que acierta mas cuando indica que alguien esta enfermo que al decir que esta sano

