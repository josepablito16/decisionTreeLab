# setwd("./")
library("ggpubr")

data<-read.csv("./Data/train.csv",stringsAsFactors = FALSE)

#Resumen de datos
summary(data)
str(data)

# numerics <- c(2,4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)
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

# Obtener correlaci?n de cada columna en el dataframe
correlation <- cor(data[,rowNumbers],data$SalePrice,method = c("pearson", "kendall", "spearman"))

# Crear y ordenar tabla
corrTable <- data.frame(rowNumbers, varNames, correlation)
corrTableDesc <- corrTable[order(-correlation),]

#Preguntas
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