#El objetivo de esta práctica es comprobar cómo la información visual que devuelve t-SNE (no convexa)  
#a veces supera la que devuelve PCA. Para ello, emplearemos la base de datos USArrest, que 
#ofrece el número de delitos de 3 tipos (asalto, homicidio y violación) más el porcentaje de suelo 
#urbano en todos los estados de norteamérica.
#https://www.kaggle.com/deepakg/usarrests

library("ggplot2")
usa.df <- read.csv('USArrests.csv', stringsAsFactors = T)
row.names(usa.df)<-usa.df$X

#El promedio de los datos muestra que hay tres veces más secuestros que asesinatos y 8 veces más asaltos que secuestros.
apply(X = USArrests, MARGIN = 2, FUN = mean)
apply(X = USArrests, MARGIN = 2, FUN = var) #la varianza es enorme
#Hay que normalizar, por tanto, o cualquier operación de reducción de dimensionalidad se inclinara a asaltos

pca <- prcomp(USArrests, scale = TRUE)
names(pca)
#se puede ver que el objeto pca tiene center, sdev (pre-escala) rotacion, escala y x
#x son las componentes de todos los elementos, rotacion, las cargas
#ej. PC1=−0.5358995 Murder−0.5831836 Assault−0.2781909 UrbanPop−0.5434321 Rape --> delitos
#PC2->tiene un 0.8 en UrbanPop, es decir poblacion en ciudad. Va mas de estructura

biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

prop_varianza <- pca$sdev^2 / sum(pca$sdev^2) #la mayor cantidad de info esta en los delitos, claro

ggplot(data = data.frame(prop_varianza, pc = 1:4),
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")

prop_varianza_acum <- cumsum(prop_varianza)

ggplot(data = data.frame(prop_varianza_acum, pc = 1:4),
       aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")

library(ggfortify) # for autplot support for the clustering
library(cluster) # for pam
cl <- pam(usa.df[,-1], 3)
# learn about the interpretation of the principal componenets:
autoplot(cl, frame = TRUE, frame.type = 'norm', loadings.label = TRUE,
         loadings.colour = "black", loadings.label.colour = "black")
autoplot(cl, frame = TRUE, frame.type = 'norm', label.repel = TRUE,
         label = TRUE, label.colour = "black", shape = FALSE)

library(Rtsne)
library(ggrepel)
set.seed(1234) # reproducibility
tsne <- Rtsne(usa.df[,-1], dims = 2, perplexity = 5)
t.df <- as.data.frame(tsne$Y)
colnames(t.df) <- c("V1", "V2")
t.df <- cbind(t.df, Cluster = factor(cl$clustering))
t.df$Ciudad <- rownames(t.df)
ggplot(t.df, aes(x = V1, y = V2, color = Cluster)) +
  geom_point() + geom_text_repel(aes(label=Ciudad),hjust=0, vjust=0) 

ggplot(t.df, aes(x = V1, y = V2, color = Cluster)) +
  geom_point() + geom_text_repel(aes(label=Ciudad),hjust=0, vjust=0) 

#ggplot(nba, aes(x= MIN, y= PTS, colour="green", label=Name))+
#  geom_point() +
#  geom_text(aes(label=ifelse(valor>24,as.character(Name),'')),hjust=0,vjust=0)
#pintar solo etiquetas cuya columna valor sea mayor que ...
