#Ejemplo tomado de https://www.analytics-tuts.com/association-rules-in-r/
df= read.csv("groceries.csv", header=FALSE)
str(df)
#Cada fila es simplemente una compra
#Los datos no sirven asi. Necesitamos una matriz. Cada fila es una compra y en cada columna un 0 o 1 si lleva el elemento
library(arules) #para ello usamos esta libreria
library(arulesViz)
gr <-read.transactions(file.choose(), sep = ",")
str(gr)
#En concreto, hay 9835 compras, pero solo 169 objetos distintos
#Los mas frecuentes son leche, vegetales, bollos, refrescos, yogur y "otros"
inspect(gr[1:15]) #podemos leer la estructura con inspect
#y saber cuantas veces se ha comprado cada cosa (suelta) con itemFrequency
itemFrequency(gr[,8:10])
itemFrequencyPlot(gr, support = .10) #o pintando el histograma (de los de mas del 10% de frecuencia)
itemFrequencyPlot(gr,topN=10,type="relative") #o de los 10 mayores

#Creemos reglas ahora. Lo primero es definir el minimo support y confidence levels
#support = numero transacciones con A y B entre numero total de transacciones, i.e. cuanto se asocian A y B en total 
#confidence = probabilidad de comprar B cuando compras A = transacciones con A y B entre transacciones con A
#lift probabilidad de asociar A y B entre probabilidad de A por probabilidad de B.
rules <- apriori(gr, parameter = list(supp = 0.005, conf = 0.20, minlen =2))

#Nos salen 872 reglas, los minimos y maximos de los parametros y el tamaño de las reglas
#veamos algunas
options(digits=2)
inspect(rules[1:10]) #la regla tiene dos partes lhs (si sale esto...) y rhs (saldra esto tambien)
#ej. quien compra pastel, compra leche (confidence 0.4)
#ordenemos por confidence
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:10]) #ej. quien compra fruta, vegetales y yougur, compra leche (confidence 0.7)
#ordenemos ahora por lift; lift normaliza la probabilidad a las frecuencias de A y B
rules<-sort(rules, by="lift", decreasing=TRUE) #a mayor lift, mayor probabilidad de que
inspect(rules[1:10]) #rhs se asocie a elementos de lhs.Ej. la probabilidad de comprar mantequilla es
#3.8 veces mayor si ademas compras crema y leche

#Podemos usar las reglas para hacer recomendaciones. Por ejemplo, fijemos lhs como soda (refrescos)

rules <- apriori(gr, parameter = list(supp = 0.005, conf = 0.20, minlen =2), appearance = list(default="rhs",lhs="soda"))
rules<-sort(rules, by="lift", decreasing=TRUE)
inspect(rules)
#esto indica que si has comprado refresco, te puede interesar bollos o leche.