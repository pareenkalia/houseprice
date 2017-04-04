install.packages(Amelia)
install.packages(corrplot)
library(corrplot)
library(Amelia)

# Loading Dataset
train <- read.csv("train_houseprice.csv")
head(train)
str(train)

# MSSubClass should be categorical
train$MSSubClass <- factor(train$MSSubClass)
str(train)

# Alley shows 2 levels, grvl and pave. Want NA to be a factor too
str(train)
train$Alley<-addNA(train$Alley)
str(train)

#Utilities data has only two levels, but there are 4 possible levels
levels_Utilities<- c("AllPub","NoSeWa","NoSewr", "ELO")
train$Utilities<- factor(train$Utilities, levels=levels_Utilities)
str(train$Utilities)
levels(train$Utilities)

#Condition2 data has only 8 levels, but there are 9 possible levels
str(train)
levels(train$Condition2)
levels_Condition2<-c("Artery", "Feedr", "Norm", "PosA", "PosN", "RRAe",  "RRAn", "RRNn", "RRNe")
train$Condition2 <- factor(train$Condition2, levels=levels_Condition2)
str(train$Condition2)

#OverallQual should be an ordered factor
train$OverallQual <- factor(train$OverallQual, levels=1:10, ordered = TRUE)
str(train$OverallQual)

#OverallCond should be an ordered factor
train$OverallCond <- factor(train$OverallCond, levels=1:10, ordered = TRUE)
train$OverallCond

str(train)

#YearBuilt should be an ordered factor
train$YearBuilt <- as.integer(train$YearBuilt)
str(train$YearBuilt)

#YearRemodAdd should be an ordered factor
train$YearRemodAdd <- as.integer(train$YearRemodAdd)
str(train$YearRemodAdd)

str(train)

#Exterior1st shows 15 levels but has 17
anyNA(train$Exterior1st) # Checking if NA shoulw be a level
levels_Exterior <- c("AsbShng", "AsphShn","Brk Cmn", "BrkFace", "CBlock", "CmentBd", "HdBoard","ImStucc", "MetalSd","Other", "Stone", "Stucco","VinylSd","Wd Sdng","Wd Shng")
train$Exterior1st<- factor(train$Exterior1st, levels=c(levels(train$Exterior1st),"PreCast", "Other"))
levels(train$Exterior1st)

#Exterior2nd shos 16 levels but has 17
anyNA(train$Exterior2nd) #Checking if NA should be a level
train$Exterior2nd<- factor(train$Exterior2nd, levels=c(levels(train$Exterior2nd),"PreCast"))
levels(train$Exterior2nd)


str(train)

# ExterQual should be ordered factors with 5 levels
levels(train$ExterQual)
train$ExterQual <- factor(train$ExterQual, levels=c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
levels(train$ExterQual)

str(train)
# ExterCond should be ordered factors with 5 levels
train$ExterCond <- factor(train$ExterCond, levels=c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
levels(train$ExterCond)

str(train)

#BsmtQual should have 5 ordered levels and NA
train$BsmtQual<-factor(train$BsmtQual, levels= c(NA,"Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered = TRUE)
train$BsmtQual

str(train)

#BsmtCond should have 5 ordered levels and NA
train$BsmtCond<-factor(train$BsmtCond, levels= c(NA,"Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered = TRUE)
train$BsmtCond

str(train)
train$BsmtExposure

#BsmtExposure should have 5 ordered levels, including NA : NA, No, Mn,Av, Gd
train$BsmtExposure<-factor(train$BsmtExposure, levels= c(NA,"No", "Mn", "Av", "Gd"), exclude = NULL, ordered = TRUE)
train$BsmtExposure

str(train)

#train$BsmtFinType1 should include NA and have 7 levels
train$BsmtFinType1<-factor(train$BsmtFinType1, levels= c(NA,"Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), exclude = NULL, ordered = TRUE)
train$BsmtFinType1

str(train)

#train$BsmtFinType2 should include NA and have 7 levels
train$BsmtFinType2<-factor(train$BsmtFinType2, levels= c(NA,"Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"), exclude = NULL, ordered = TRUE)
train$BsmtFinType2

str(train)

# KitchenQual should be ordered factors with 5 levels
levels(train$KitchenQual)
train$KitchenQual <- factor(train$KitchenQual, levels=c("Po", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
levels(train$KitchenQual)

str(train)

# Functional should have 8 orderedlevels
levels(train$Functional) # (Sal missing)
train$Functional <- factor(train$Functional, levels=c("Typ", "Min1", "Min2", "Mod", "Maj1", "Maj2", "Sev", "Sal"), ordered = TRUE)
levels(train$Functional)

str(train)

# FireplaceQu should have 6 ordered levels
train$FireplaceQu <- factor(train$FireplaceQu, levels = c(NA, "Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered=TRUE)
train$FireplaceQu

str(train)

# GarageType should include NA as a level
levels(train$GarageType)
train$GarageType<- addNA(train$GarageType)
levels(train$GarageType)

str(train)

# Will see later if this is categorical
train$GarageYrBlt

# GarageFinish should include NA as a level
levels(train$GarageFinish)
train$GarageFinish<-addNA(train$GarageFinish)
levels(train$GarageFinish)

str(train)

# GarageQual should include NA as a level
levels(train$GarageQual)
train$GarageQual <- factor(train$GarageQual, levels = c(NA, "Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered=TRUE)
train$GarageQual

str(train)

# GarageCond should include NA and should have ordered factors
levels(train$GarageQual)
train$GarageCond <- factor(train$GarageCond, levels = c(NA, "Po", "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered=TRUE)
train$GarageCond

str(train)

# PoolQC should include NA and have ordered factors
train$PoolQC <- train$PoolQC<- factor(train$PoolQC, levels = c(NA, "Fa", "TA", "Gd", "Ex"), exclude = NULL, ordered=TRUE)
train$PoolQC

str(train)

# Fence should include NA
levels(train$Fence)
train$Fence<-addNA(train$Fence)
levels(train$Fence)

str(train)

# MiscFeature should include NA
levels(train$MiscFeature)
train$MiscFeature<-addNA(train$MiscFeature)
levels(train$MiscFeature)

str(train)

# SaleType should include VWD
train$SaleType <- factor(train$SaleType, levels= c(levels(train$SaleType), "VWD"))
levels(train$SaleType) 

str(train)
head(train)

###################################

###### ----------------------------------Missing values------------------------------------------###########


#listing rows of data with missing values
train[!complete.cases(train),]

summary(train)
colSums(sapply(train, is.na))
head(train)

#Get the number of missing values in each column

number.of.mv <-function(a){
  mv.list <- c()
for(i in 1:ncol(a))
{
  mv.list[i] <- sum(is.na(a[,i]))
}
  return(mv.list)
}
number.of.mv(train)

# names of columns with missing values and the number of missing values 

get.mv <- function(a){
  missing.values<-cbind(colnames(a[which(number.of.mv(a)!=0)]),number.of.mv(a)[!(number.of.mv(a)==0)])
  return(missing.values)
}

get.mv(train)

colnumbers.mv<-function(a)
{
  list.col <- number.of.mv(a)
  return(which(list.col!=0))
}
colnumbers.mv(train)
# Mapping columns with missing values to look for a pattern

#---------------------------------- Graph of Missing Values----

missmap(train[c(colnumbers.mv(train))])


# LotFrontage is Linear feet of street connected to property
# Maybe Lot Frontage is larger for larger houses? Is there a relationship between 
# Lot Frontage and Lot Area?

cor(train$LotFrontage[!(is.na(train$LotFrontage))], train$LotArea[!(is.na(train$LotFrontage))])


# correlation coefficient is 0.426095. Yikes. Not that strong

#data.frame(table(train$Neighborhood[is.na(train$LotFrontage)]))

# 17% mv in LotFrontage. Can't discard

# performing a simple random imputation of LotFrontage MV

data.frame(table(train$Neighborhood[is.na(train$LotFrontage)]))

random.imputation <- function (a){
  missing <- is.na(a)
  no.of.missing <- sum(missing)
  notmissing.data <- a[!missing]
  impute <- a
  impute[missing] <- sample(notmissing.data, no.of.missing, replace=TRUE)
  return(impute)
}

train$LotFrontage <-random.imputation(train$LotFrontage)

#--- Missing value map after imputing LotFrontage
missmap(train[c(colnumbers.mv(train))])

# GarageYrBlt has 81 missing values
# Want to check if the missing values are of records which don't have a garage,
# i.e., GarageType is NA

# Setting GarageYrBlt = 0 for values that don't have a garage

if (all(is.na(as.character(train$GarageType[is.na(train$GarageYrBlt)]))) == TRUE)
{
  train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0 
}
  
missmap(train[c(colnumbers.mv(train))])

# If MasVnrType is NA, then MasVnrType does not exist, can be none

train$MasVnrType[is.na(train$MasVnrType)] <- "None"

missmap(train[c(colnumbers.mv(train))])

# MasVnrArea should be 0 for MasVnrType = none

if (all(train$MasVnrType[is.na(train$MasVnrArea)]== "None")==TRUE)
{
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
}


missmap(train[c(colnumbers.mv(train))])

# Electrical has 1 missing value, impute it with the most frequent

freq.impute <- function(a)
{
  mv<- a[is.na(a)]
  imp<- names(which(table(a) == max(table(a))))
  a[mv] <- imp
  
}

train$Electrical <- freq.impute(train$Electrical)
sum(is.na(train$Electrical))


#---- GRAPHS ----

numeric.columns <- train[,sapply(train, is.numeric)]
factor.columns <- train[, sapply(train, is.factor)]

par(mfrow=c(1,1))

correlation.matrix <- cor(numeric.columns[,2:ncol(numeric.columns)])
#correlation.matrix
corrplot(correlation.matrix, method="square")


