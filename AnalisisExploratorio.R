# setwd("./")
data <- read.csv("./Data/train.csv",stringsAsFactors = FALSE)


# Analisis de variables cuantitativas

# Distribucion normal de OverallQual
qqnorm(data$OverallQual)
qqline(data$OverallQual)
hist(data$OverallQual)

# Distribucion normal de GrLivArea
qqnorm(data$GrLivArea)
qqline(data$GrLivArea)
hist(data$GrLivArea)

# Distribucion normal de GarageCars
qqnorm(data$GarageCars)
qqline(data$GarageCars)
hist(data$GarageCars)

# Distribucion normal de GarageArea
qqnorm(data$GarageArea)
qqline(data$GarageArea)
hist(data$GarageArea)

# Distribucion normal de TotalBsmtSF
qqnorm(data$TotalBsmtSF)
qqline(data$TotalBsmtSF)
hist(data$TotalBsmtSF)

# Distribucion normal de X1stFlrSF
qqnorm(data$X1stFlrSF)
qqline(data$X1stFlrSF)
hist(data$X1stFlrSF)

# Distribucion normal de FullBath
qqnorm(data$FullBath)
qqline(data$FullBath)
hist(data$FullBath)

# Distribucion normal de TotRmsAbvGrd
qqnorm(data$TotRmsAbvGrd)
qqline(data$TotRmsAbvGrd)
hist(data$TotRmsAbvGrd)

# Distribucion normal de YearBuilt
qqnorm(data$YearBuilt)
qqline(data$YearBuilt)
hist(data$YearBuilt)

# Distribucion normal de YearRemodAdd
qqnorm(data$YearRemodAdd)
qqline(data$YearRemodAdd)
hist(data$YearRemodAdd)

# Distribucion normal de Fireplaces
qqnorm(data$Fireplaces)
qqline(data$Fireplaces)
hist(data$Fireplaces)

# Distribucion normal de BsmtFinSF1
qqnorm(data$BsmtFinSF1)
qqline(data$BsmtFinSF1)
hist(data$BsmtFinSF1)

# Distribucion normal de WoodDeckSF
qqnorm(data$WoodDeckSF)
qqline(data$WoodDeckSF)
hist(data$WoodDeckSF)

# Distribucion normal de X2ndFlrSF
qqnorm(data$X2ndFlrSF)
qqline(data$X2ndFlrSF)
hist(data$X2ndFlrSF)

# Distribucion normal de OpenPorchSF
qqnorm(data$OpenPorchSF)
qqline(data$OpenPorchSF)
hist(data$OpenPorchSF)

# Distribucion normal de HalfBath
qqnorm(data$HalfBath)
qqline(data$HalfBath)
hist(data$HalfBath)

# Distribucion normal de LotArea
qqnorm(data$LotArea)
qqline(data$LotArea)
hist(data$LotArea)

# Distribucion normal de BsmtFullBath
qqnorm(data$BsmtFullBath)
qqline(data$BsmtFullBath)
hist(data$BsmtFullBath)

# Distribucion normal de BsmtUnfSF
qqnorm(data$BsmtUnfSF)
qqline(data$BsmtUnfSF)
hist(data$BsmtUnfSF)

# Distribucion normal de BedroomAbvGr
qqnorm(data$BedroomAbvGr)
qqline(data$BedroomAbvGr)
hist(data$BedroomAbvGr)

# Distribucion normal de ScreenPorch
qqnorm(data$ScreenPorch)
qqline(data$ScreenPorch)
hist(data$ScreenPorch)

# Distribucion normal de PoolArea
qqnorm(data$PoolArea)
qqline(data$PoolArea)
hist(data$PoolArea)

# Distribucion normal de MoSold
qqnorm(data$MoSold)
qqline(data$MoSold)
hist(data$MoSold)

# Distribucion normal de X3SsnPorch
qqnorm(data$X3SsnPorch)
qqline(data$X3SsnPorch)
hist(data$X3SsnPorch)

# Distribucion normal de BsmtFinSF2
qqnorm(data$BsmtFinSF2)
qqline(data$BsmtFinSF2)
hist(data$BsmtFinSF2)

# Distribucion normal de BsmtHalfBath
qqnorm(data$BsmtHalfBath)
qqline(data$BsmtHalfBath)
hist(data$BsmtHalfBath)

# Distribucion normal de MiscVal
qqnorm(data$MiscVal)
qqline(data$MiscVal)
hist(data$MiscVal)

# Distribucion normal de LowQualFinSF
qqnorm(data$LowQualFinSF)
qqline(data$LowQualFinSF)
hist(data$LowQualFinSF)

# Distribucion normal de YrSold
qqnorm(data$YrSold)
qqline(data$YrSold)
hist(data$YrSold)

# Distribucion normal de OverallCond
qqnorm(data$OverallCond)
qqline(data$OverallCond)
hist(data$OverallCond)

# Distribucion normal de MSSubClass
qqnorm(data$MSSubClass)
qqline(data$MSSubClass)
hist(data$MSSubClass)

# Distribucion normal de EnclosedPorc
qqnorm(data$EnclosedPorch)
qqline(data$EnclosedPorch)
hist(data$EnclosedPorch)

# Distribucion normal de KitchenAbvGr
qqnorm(data$KitchenAbvGr)
qqline(data$KitchenAbvGr)
hist(data$KitchenAbvGr)

# Distribucion normal de LotFrontage
qqnorm(data$LotFrontage)
qqline(data$LotFrontage)
hist(data$LotFrontage)

# Distribucion normal de MasVnrArea
qqnorm(data$MasVnrArea)
qqline(data$MasVnrArea)
hist(data$MasVnrArea)

# Distribucion normal de GarageYrBlt
qqnorm(data$GarageYrBlt)
qqline(data$GarageYrBlt)
hist(data$GarageYrBlt)


# Tablas de frecuencias de variables cualitativas

# Tabla de MSZoning
table(data$MSZoning)

# Tabla de Street
table(data$Street)

# Tabla de Alley
table(data$Alley)

# Tabla de LotShape
table(data$LotShape)

# Tabla de LandContour
table(data$LandContour)

# Tabla de Utilities
table(data$Utilities)

# Tabla de LotConfig
table(data$LotConfig)

# Tabla de LandSlope
table(data$LandSlope)

# Tabla de Neighborhood
table(data$Neighborhood)

# Tabla de Condition1
table(data$Condition1)

# Tabla de Condition2
table(data$Condition2)

# Tabla de BldgType
table(data$BldgType)

# Tabla de HouseStyle
table(data$HouseStyle)

# Tabla de RoofStyle
table(data$RoofStyle)

# Tabla de RoofMatl
table(data$RoofMatl)

# Tabla de Exterior1st
table(data$Exterior1st)

# Tabla de Exterior2nd
table(data$Exterior2nd)

# Tabla de MasVnrType
table(data$MasVnrType)

# Tabla de ExterQual
table(data$ExterQual)

# Tabla de ExterCond
table(data$ExterCond)

# Tabla de Foundation
table(data$Foundation)

# Tabla de BsmtQual
table(data$BsmtQual)

# Tabla de BsmtCond
table(data$BsmtCond)

# Tabla de BsmtExposure
table(data$BsmtExposure)

# Tabla de BsmtFinType1
table(data$BsmtFinType1)

# Tabla de BsmtFinType2
table(data$BsmtFinType2)

# Tabla de Heating
table(data$Heating)

# Tabla de HeatingQC
table(data$HeatingQC)

# Tabla de CentralAir
table(data$CentralAir)

# Tabla de Electrical
table(data$Electrical)

# Tabla de KitchenQual
table(data$KitchenQual)

# Tabla de Functional
table(data$Functional)

# Tabla de FireplaceQu
table(data$FireplaceQu)

# Tabla de GarageType
table(data$GarageType)

# Tabla de GarageFinish
table(data$GarageFinish)

# Tabla de GarageQual
table(data$GarageQual)

# Tabla de GarageCond
table(data$GarageCond)

# Tabla de PavedDrive
table(data$PavedDrive)

# Tabla de PoolQC
table(data$PoolQC)

# Tabla de Fence
table(data$Fence)

# Tabla de MiscFeature
table(data$MiscFeature)

# Tabla de SaleType
table(data$SaleType)

# Tabla de SaleCondition
table(data$SaleCondition)
