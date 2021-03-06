# setwd("./")
library("ggpubr")

data<-read.csv("./Data/train.csv",stringsAsFactors = FALSE)

#Resumen de datos
summary(data)
str(data)

#Preguntas

rowNumbers <- c()
varNames <- c()

# Obtener vector de columnas num?ricas
for(name in colnames(data)){
  if(is.numeric(data[1,name]) && name != "Id")
  {
    rowNumbers <- c(rowNumbers, which(colnames(data)==name))
    varNames <- c(varNames, name)
  }
}

# Crear y ordenar tabla
corrTable <- data.frame(rowNumbers, varNames, correlation)
corrTableDesc <- corrTable[order(-correlation),]

# Obtener correlaci?n de cada columna en el dataframe
correlation <- cor(data[,rowNumbers],data$SalePrice,method = c("pearson", "kendall", "spearman"))
View(corrTableDesc)

#?Cu?l es el promedio de chimeneas que tienen las casas m?s caras?
mean(head(data[order(data$SalePrice,decreasing = TRUE),c("Fireplaces")], n = 20))


#?En qu? intervalo de a?os han sido construidas las casas m?s caras?
table(head(data[order(data$SalePrice,decreasing = TRUE),c("YearBuilt")], n = 20))

#?Cu?l es la cualidad general de las cocinas que tienen las casas m?s caras?
table(head(data[order(data$SalePrice,decreasing = TRUE),c("KitchenQual")], n = 20))


#?Cual es el promedio de cuartos que tienen las 20 casas m?s caras?
mean(head(data[order(data$SalePrice,decreasing = TRUE),c("BedroomAbvGr")], n = 20))


#?El tama?o del garajes de una casa influye en el precio?
cor(data$GarageArea,data$SalePrice,method = c("pearson", "kendall", "spearman"))

#?Cu?ntas casas tienen piscina?
nrow(data[data$PoolArea>0,])

# ¿Cuál es la capacidad promedio en carros de los garajes de las casas más caras?
carrosCasasDesc <- data[order(data$SalePrice, decreasing = TRUE),c("GarageCars","SalePrice")]
mean(carrosCasasDesc[1:50,1])

# ¿Se asocian ciertas calles a las casas más grandes?
lotAreaDesc <- data[order(data$LotArea, decreasing = TRUE),c("Neighborhood")]
table(lotAreaDesc[1:200])

#Agrupamiento de datos

#Usando Metodo k-medias
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el numero de clusters optimo
library(factoextra) #Para hacer gr?ficos bonitos de clustering

#View(data[,c(18,47,62,63,81)])
data2<-data[,c(18,47,62,63)]
dataCluster<-data[,c(18,47,62,63,81)]
#Para saber la cantidad de grupos 
wss <- (nrow(na.omit(dataCluster))-1)*sum(apply(na.omit(dataCluster),2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(na.omit(dataCluster), centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")


#Clustering jerarquico
hc<-hclust(dist(na.omit(dataCluster))) #Genera el clustering jerárquico de los datos
plot(hc) #Genera el dendograma
rect.hclust(hc,k=3) #Dibuja el corte de los grupos en el gráfico
groups<-cutree(hc,k=3) #corta el dendograma, determinando el grupo de cada fila
data2$gruposHC<-groups

fviz_cluster(list(data = na.omit(dataCluster), cluster = groups))#Grafica

#Metodo de la silueta para clustering jerárquico
silch<-silhouette(groups,dist(na.omit(dataCluster)))
mean(silch[,3])


#Analisis
summary(data2[data2$gruposHC==1,])
hist(data2[data2$gruposHC==1,"OverallQual"])
hist(data2[data2$gruposHC==1,"GrLivArea"])
hist(data2[data2$gruposHC==1,"GarageCars"])

summary(data2[data2$gruposHC==2,])
hist(data2[data2$gruposHC==2,"OverallQual"])
hist(data2[data2$gruposHC==2,"GrLivArea"])
hist(data2[data2$gruposHC==2,"GarageCars"])

summary(data2[data2$gruposHC==3,])
hist(data2[data2$gruposHC==3,"OverallQual"])
hist(data2[data2$gruposHC==3,"GrLivArea"])
hist(data2[data2$gruposHC==3,"GarageCars"])


# Se obtienen los cuartiles de la data
quantile(data[order(data$SalePrice),"SalePrice"])

  # Del total de los precios, las casas promedio se encuentran entre el 25% y el 75%
  # del valor más alto. Por ello, Las casas "caras" se toman a partir de 214000 dólares y las
  # Baratas de 13000 dólares hacia abajo.

# Dataframe del precio de las casas descendiente
casasPrecioDesc <- data[order(-data$SalePrice),]
hist(casasPrecioDesc$SalePrice)

 # Casas más caras...
topCasas <- casasPrecioDesc[1:225,"SalePrice"]
mean(topCasas)
hist(topCasas)

  # El promedio de las casas más caras es de 324000 dólares, lo que cumple con el tercer
  # cuartil obtenido previamente


# Casas más baratas...
bottomCasas <- casasPrecioDesc[1096:1460, "SalePrice"]
mean(bottomCasas)
hist(bottomCasas)

  # El promedio de las casas más baratas es de 105831 dólares, lo que cumple con el primer
  # cuartil obtenido previamente

library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)


# Conjunto de entrenamiento
trainingSet <- data2[1:876,]
# Conjunto de test
testSet <- data2[1168:1460,]

# MODELO CON EL CONJUNTO DE PRUEBA
test<-read.csv("./Data/test.csv",stringsAsFactors = FALSE)
test <- test[,c(18,47,62,63)]
answer <- read.csv("./Data/sample_submission.csv",stringsAsFactors = FALSE)

grupoRespuesta <- c()
testCompleto<-test
vector <- answer[,2]

for (value in vector) {
  if (value <= 260400) {
    grupoRespuesta <- c(grupoRespuesta, 1)
  } else if (value >= 410000) {
    grupoRespuesta <- c(grupoRespuesta, 3) 
  } else {
    grupoRespuesta <- c(grupoRespuesta, 2)
  }
}

answer$grupoRespuesta <- grupoRespuesta


# Creacion del árbol de clasificación en base a los grupos generados por el cluster
dt_model<-rpart(trainingSet$gruposHC~., trainingSet, method = "class")
plot(dt_model);text(dt_model)
prp(dt_model)
rpart.plot(dt_model)

# PREDICCION AD SOBRE CV
prediccion <- predict(dt_model, newdata = testSet[1:4])
columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
testCompleto <- testSet
testCompleto$prediccion<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción

cfm<-table(testCompleto$prediccion,testCompleto$gruposHC)
cfm

# PREDICCION AD SOBRE CSV
prediccion <- predict(dt_model, newdata = test[1:4])
columnaMasAlta<-apply(prediccion, 1, function(x) colnames(prediccion)[which.max(x)])
testCompleto <- test
testCompleto$prediccion<-columnaMasAlta #Se le añade al grupo de prueba el valor de la predicción

cfm<-table(testCompleto$prediccion,answer$grupoRespuesta)
cfm



# Creacion del árbol de regresión en base a los grupos generados por el cluster
trainingSetConPrecios <- trainingSet[1:4]
trainingSetConPrecios$SalePrice <- data[1:876,81]

# Entrenar a los datos en base al precio
dt_model2<-rpart(trainingSetConPrecios$SalePrice~., trainingSetConPrecios, method = "anova")
plot(dt_model2);text(dt_model2)
prp(dt_model2)
rpart.plot(dt_model2)


# PREDICCIÓN SOBRE CV


prediccion <- predict(dt_model2, newdata = testSet[1:4])
testCompleto <- testSet[1:4]
testCompleto$prediccion<-prediccion #Se le añade al grupo de prueba el valor de la predicción
testCompleto$ValorReal<-data[1168:1460,81]

#Verificamos el error promedio para ver el rendimiento
errorProm<-0
for (i in 1:length(testCompleto) ) {
  errorProm<-errorProm + ((abs(testCompleto$prediccion[i]-testCompleto$ValorReal[i])*100)/testCompleto$ValorReal[i])
}
errorProm<-errorProm/length(testCompleto)


# PREDICCION SOBRE CSV
prediccion <- predict(dt_model2, newdata = test[1:4])
testCompleto <- test
testCompleto$prediccion<-prediccion #Se le añade al grupo de prueba el valor de la predicción
testCompleto$respuesta<-answer$SalePrice

#Verificamos el error promedio para ver el rendimiento
errorProm<-0
for (i in 1:length(testCompleto) ) {
  errorProm<-errorProm + ((abs(testCompleto$prediccion[i]-testCompleto$respuesta[i])*100)/testCompleto$respuesta[i])
}
errorProm<-errorProm/length(testCompleto)



test <- test[,c(18,47,62,63)]
#############################################################################################################################
# PREDICCION DE RF SOBRE CV
testSet <- data2[1168:1460,]

# Random forest
modeloRF1<-randomForest(trainingSetConPrecios$SalePrice~.,data=trainingSetConPrecios)
prediccionRF1<-predict(modeloRF1, newdata = testSet[1:4])
testCompleto$ValorReal<-data[1168:1460,81]
testCompleto$predRF<-prediccionRF1


errorProm<-0
for (i in 1:length(testCompleto) ) {
  errorProm<-errorProm + ((abs(testCompleto$predRF[i]-testCompleto$ValorReal[i])*100)/testCompleto$ValorReal[i])
}
errorProm<-errorProm/length(testCompleto)

# POR HACER..!
# testCompleto$respuesta <-



# PREDICCION RF SOBRE EL TEST CSV
prediccionRF1<-predict(modeloRF1, newdata = test[1:4])
testCompleto <- test
testCompleto$prediccion2<-prediccionRF1 
testCompleto$respuesta2<-answer$SalePrice

errorProm<-0
for (i in 1:length(testCompleto) ) {
  errorProm<-errorProm + ((abs(testCompleto$prediccion2[i]-testCompleto$respuesta2[i])*100)/testCompleto$respuesta2[i])
}
errorProm<-errorProm/length(testCompleto)

# Resultado final
# Comparar precio real vs prediccion
################################################################################################################################
