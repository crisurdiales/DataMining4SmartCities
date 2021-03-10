#Ejemplo de aplicar EFA para determinar si hay un conjunto más reducido de factores que expliquen la variación de los datos. 
#Vamos a usar el mismo archivo que en el ejemplo de PCA.
#Cargamos el archivo y ajustamos los nombres de las columnas de acuerdo a la descripcion
houses <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data",header = F, na.string = "?")
colnames(houses) <- c("CRIM", "ZN", "INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIO","B","LSTAT","MEDV")
library(psych)  #paquetes para el CFA
library(GPArotation)

#https://www.kaggle.com/prasadperera/the-boston-housing-dataset
#CRIM per capita crime rate by town
#ZN proportion of residential land zoned for lots over 25,000 sq.ft.
#INDUS proportion of non-retail business acres per town.
#CHAS Charles River dummy variable (1 if tract bounds river; 0 otherwise)
#NOX nitric oxides concentration (parts per 10 million)
#RM average number of rooms per dwelling
#AGE proportion of owner-occupied units built prior to 1940
#DIS weighted distances to five Boston employment centres
#RAD index of accessibility to radial highways
#TAX full-value property-tax rate per €8489.87
#PTRATIO pupil-teacher ratio by town
#B - 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#LSTAT - % lower status of the population
#MEDV - Median value of owner-occupied homes in $1000's

#quitemos el precio, que es lo que suponemos que depende de todo..
houses<-houses[,-14]

#EXPLORATORY FA (EFA)
cor_mat <- cor(houses)   #Creating Correlation Matrix 

#Prueba esto para ver cuantos factores. Con 4 simuacion y datos se juntan-de 3 a 6 vamos bien
fa.parallel(houses, fm = 'minres', fa = 'fa')

Two_FactorLoading <- fa(r = cor_mat, nfactors = 2, rotate = "oblimin", fm="minres") #3 factores
Three_FactorLoading <- fa(r = cor_mat, nfactors = 3, rotate = "oblimin", fm="minres") #3 factores
Four_FactorLoading <- fa(r = cor_mat, nfactors = 4, rotate = "oblimin", fm="minres") #3 factores
#fa toma como parametris r->matriz correlacion de datos (numericos), nfactors-> numero de factores
#(si se quiere) rotate: Varimax` and `Oblimin` are the most popular
#(si se quiere) fm: factor extraction technique-> `Minimum Residual (OLS)`, `Maximum Liklihood`, `Principal Axis` etc.
#(rotate = “oblimin”) as we believe that there is a correlation in the factors. Note that Varimax rotation is used under the assumption that the factors are completely uncorrelated.
#`Ordinary Least Squared/Minres` factoring (fm = “minres”), as it is known to provide results similar to `Maximum Likelihood` without assuming a multivariate normal distribution

Three_FactorLoading

#we need to consider the loadings more than 0.3 and not loading on more than one factor. Note that negative values are acceptable here. 
#progressively, we increase the cutoff threshold until we have at least a factor with a single load

print(Two_FactorLoading$loading,cutoff = 0.3) #No nos vale LSTAT contribuye a dos factores, hay que subir
print(Three_FactorLoading$loading,cutoff = 0.3) #ahora ya si. CHAS no importa. Las otras 13 se agrupan asi:

fa.diagram(Three_FactorLoading) #esto pinta que incluye cada factor
#ahora hay que darle sentido: MR1 (DIS, AGE, NOX, ZN e INDUS) viene a ser como de industrializada está la zona
#MR2 (RAD, TAX,CRIM, B) es si es un "buen barrio" con criterio yanqui: poco negro, poco crimen, autovias accesibles..
#MR3 (RM,LSTAT,PTRATIO) representa el numero de habitaciones, status del vecindario y ratio de profesores. Tal vez numero de hijos?.
#hay que notar que no todas las variables cargan igual: el crimen (por suerte) pesa más que el porcentaje de negros, el status, no sorprendentemente, mas que el numero de profesores

print(Three_FactorLoading)
#Ahora miramos el output del analisis. RMSR vale 0.04 (cuanto mas bajo mejor)
#RMSEA no me ha salido?? pero debería ser menor de 0.05. Tucker, que tampoco me sale, debería ser mayor de 0.9 

############ EJEMPLO ORIGINAL
dataset_bfi = bfi             #Loading the Dataset
dataset_bfi = dataset_bfi[complete.cases(dataset_bfi),] #Removing the rows with Missing Values
cor_mat <- cor(dataset_bfi)   #Creating Correlation Matrix 
FactorLoading <- fa(r = cor_mat, nfactors = 6)
FactorLoading
