#Ejemplo knn con el dataset adult.csv (salarios)
#dataset aqui: https://www.kaggle.com/wenruliu/adult-income-dataset

# age: continuous.
# workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
# fnlwgt: continuous.
# education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
# education-num: continuous.
# marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
# occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
# relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
# race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
# sex: Female, Male.
# capital-gain: continuous.
# capital-loss: continuous.
# hours-per-week: continuous.
# native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.
# class: >50K, <=50K (gana mas o menos de 50000)

# Load packages
library('ggplot2') # visualization
library('dplyr') # data manipulation

knn.df <- read.csv('adult.csv', stringsAsFactors = T)
dim(knn.df)
#Dividimos al azar el set entre entrenamiento y test, pero solo numsamples muestras -si son muchas es muy lento-
numsamples<-50
set.seed(2)
knn.small <- knn.df[sample(nrow(knn.df), numsamples), ]
train.knn <- knn.small[1:as.integer(0.7*numsamples),] #70% train
test.knn <- knn.small[as.integer(0.7*numsamples +1):numsamples,] #30% test

#para fijar un valor de k, usamos la estimacion habitual de la raiz cuadrada del numero de elementos de entrenamiento
chosen_k<- trunc(sqrt(nrow(train.knn)))

#ojo con el dataset, porque hay valores que NO son NUMERICOS, asi que la distancia Euclidea no nos vale. Intentemos Gower, que admite variables categoricas
library(cluster) #no vamos a implementar de 0 el knn
d_gower<- daisy(knn.small,metric= "gower")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

knn_gower <- function (train, test, k) 
{
  p = dim(train)[2]  #numero variables
  ntest = dim(test)[1]  #numero de muestras de test
  ntrain = dim(train)[1] #numero de muestras de entrenamiento
  classes = rep(0, ntest)  #inicializar clases a vector de 0 longutud num_test
  if (ntest == ntrain) {
    for (i in 1:ntest) {
      tempo = order(gower.dist(test[i, -p], train[-i, 
                                                  -p]))[1:k]
      classes[i] = getmode(train[tempo, p])[1]
    }
  }
  else {
    for (i in 1:ntest) {
      tempo = order(StatMatch::gower.dist(test[i, -p], 
                                          train[, -p]))[1:k]
      classes[i] = getmode(train[tempo, p])[1]
    }
  }
  classes
}

results<-knn_gower(train.knn, test.knn,chosen_k) #nos da las clases de cada observacion en test
#Como ya sabemos, hay dos clases, los que ganan mas de 50K y los que menos. Con los datos, podemos mirar la ultima columna de test

test.knn[,15]
#se reconoce la clase 1 como los que ganan menos de 50k y la 2 como los que ganan mas de 50k
#Como se ve, hay algunos errores, por ejemplo el elemento 1, que se cree que es clase 1 y es 2 y el 15, que cree que es clase 2 y es 1 (gana menos). Veamos quienes son...

test.knn[c(1,15),]
#parece ser que el varón blanco de 50 años ejecutivo con un master que echa 48 horas semanales gana mas de lo esperado y la mujer blanca de 29 con un bachelor gana menos

#¿y si testeamos solo con partes del grupo muestral? Por ejemplo, varones frente a mujeres con el mismo nivel de educacion, todos en el margen de 30 a 40 años
#uso dplyr para seleccionar
levels(knn.df$education)
my_df <- filter(knn.df, age %in% c(30,40) & education=="Masters")

results<-knn_gower(train.knn, my_df,chosen_k)
error<- abs(as.numeric(my_df$income) - results)
porcentaje_error<- sum(error)/length(error)  #30%

composite<- cbind(my_df,error)
my_df$gender[error==1]  #13 mujeres mal clasificadas frente a 43 

#Si se miran los errores, son hombres que se cree que ganan menos o mujeres que se cree que ganan mas
