#Esta practica es una adaptacion del trabajo de Shane Lynch: https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/
#Partimos del set preprocesado: https://mop.cv.uma.es/pluginfile.php/810165/mod_resource/content/1/Ireland_data_in_percentages.Rda
#El set incluye un conjunto de datos de cada distrito en torno a Dublin (14 en total)
#El objetivo es reducir el número de variables que caracterizan a cada zona sólo 
#a dos para dibujarlas sobre un mapa. Supongamos, por ejemplo, que deseamos saber en qué zonas es 
#más probable que la población adquiera un vehículo para poder repartir publicidad en dicha zona 


#Carguemos las librerías que necesitamos para empezar

library(kohonen)  #esta es la RNA que se encarga de hacer el SOM
library(ggplot2) #libreria para dibujar

load(file = "Ireland_data_in_percentages.Rda") #cargamos los datos preprocesados.

#escogemos qué variables son relevantes para agrupar en el plano en nuestro problema
#por ejemplo, edad media, educacion, numero de coches, porcentaje de desempleo
#podriamos probar otras variables, segun nuestro criterio o estudios previos
data_train <- data[, c(2,4,5,8)]

# preparamos los datos para aplicar la red de Kohonen. Para ello debemos convertirlos en matriz (no dataframe)
data_train_matrix <- as.matrix(scale(data_train)) #la matriz no lleva nombres
names(data_train_matrix) <- names(data_train) #asi que se le añaden

require(kohonen)
som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")  #configuramos el grid sobre el que dibujaremos
#hemos escogido un grid de 20x20 celdas hexagonales
#aplicamos ahora la red. Esto puede llevar un tiempo
#véase que la mayoría de los parametros de la funcion son los que hemos explicado en la teoria
system.time(som_model <- som(data_train_matrix,
                             grid=som_grid,
                             rlen=100,
                             alpha=c(0.05,0.01),
                             # n.hood = "circular",
                             keep.data = TRUE ))

rm(som_grid, data_train_matrix) #limpiamos los datos que ya no usamos

# SOM VISUALIZACION: Una vez terminados, vamos a visualizar resultados
source('coolBlueHotRed.R') #definir paleta alternativa, si se desea. No es necesario

#Si se desea ver como converge el modelo, se puede descomentar la linea a continuacion, pero seguramente habra que cambiar
#los margenes de la figura. De hacerlo, se puede observar que se reduce con las iteraciones hasta alcanzar un valle.
#plot(som_model, type = "changes") 

#Lo primero que vamos a pintar es el numero de muestras (en este caso zonas) por nodo del SOM (counts).
#El resultado deberia ser mas o menos homogeneo. Si hay nodos con muchas muestras,  igual hay que hacer el SOM mas grande (grid).
#Si hay muchos nodos casi vacios, hariamos lo contrario. Deberia haber al menos de 5 a 10 samples por nodo

plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)

#Ahora pintaremos las distancias entre nodos. Aunque vamos a usar color, suele hacerse en grises (palette.name=grey.colors)
#la matriz de distancias se denomina U-Matrix. Distancias grandes delimitan zonas naturales para una futura particion
#es decir, que los nodos que tengan distancias pequeñas entre ellos deberian agruparse y las fronteras de unos a otros grupos
#se marcan en la matriz con colores diferentes. Hay que recordar, no obstante, que el SOM no agrupa, solo distribuye

plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=coolBlueHotRed)

#El siguiente plot solo es facil de ver cuando tenemos pocos nodos. En el, se dibujan los denominados "codigos" del SOM, es decir,
#los pesos de las variables que hemos reducido para obtener el plano para cada nodo. Estos pesos representan una normalizacion
#de todos los elementos asociados al nodo
plot(som_model, type = "codes")

#Cuando tenemos muchos nodos, para visualizar mejor es necesario colorear de acuerdo a una variable en particular.
#Los codigos son una estructura de tantas columnas como variables y tantas filas como nodos
#Es interesante ver su estructura de datos. Para ello podemos usar la funcion getCodes(som_model)
#Representemos por ejemplo, el color en funcion del peso de la componente nivel de educacion

var <- 2
plot(som_model, type = "property", property = getCodes(som_model)[,var], main=colnames(getCodes(som_model))[var], palette.name=coolBlueHotRed)
#Observese que el mapa dibujado no preserva la informacion geografica -esta se encuentra codificada en los enlaces a los datos 
#originales- sino que agrupa los lugares por parecido, pero por parecido en LAS CUATRO VARIABLES empleadas. 
#no obstante, se ve que el desempleo influye bastante en la distribucion -se ven regiones claras-

#probemos ahora con el porcentaje de desempleo y comparemos ambos plots

var <- 4
plot(som_model, type = "property", property = getCodes(som_model)[,var], main=colnames(getCodes(som_model))[var], palette.name=coolBlueHotRed)

#puede observarse una asimetria visual inmediata -que habria que analizar en mas profundidad- entre ambas representaciones
#notese que los nodos son los mismos, por lo que esto podria indicar que el nivel de educacion es inverso al porcentaje de empleo

#Cabe indicar que estamos representando los codigos normalizados, pero podriamos revertir para presentar sus valores originales
#si lo deseamos porque tenemos esa informacion almacenada en unit.classif (var sigue siendo la columna a representar)

var_unscaled <- aggregate(as.numeric(data_train[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(som_model, type = "property", property=var_unscaled, main=colnames(getCodes(som_model))[var], palette.name=coolBlueHotRed)

