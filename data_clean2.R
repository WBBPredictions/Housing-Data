library(readr)
train <- read_csv("GitHub/Housing-Data/train.csv")
View(train)

#excluding the variable ID
my.data<-train[,-1]
View(my.data)

#NO CHANGES REQUIRED FOR (A-Z)- MS Zoning, Lot Area, Street, Lot Shape, Land contour, Utilities,
#Land config, Land slope, Neighbourhood, Condition1, Condition2, Bldgtype, HouseStyle,
#OverallQuality, OverallCondition, RoofStyle, RoofMAterial, Exterior1, Exterior2, MasVnrType

#WHAT TO DO WITH -Year built, Year Remodelled?


#MSSubClass: Converting the numbers to characters
typeof(my.data[,1])
MSSubCClass<-as.character(my.data$MSSubClass)
my.data[,1]<-MSSubCClass #replace with the converted col.
MSSubCClass
typeof(my.data$MSSubClass)


#Lot Frontage:convert "NA", if any , to "None"
my.data$LotFrontage[is.na(my.data$LotFrontage)]<- "None"

#Alley:convert "NA", if any , to "None"
my.data$Alley[is.na(my.data$Alley)]<- "None"


#NO CHANGES REQUIRED FOR (AA-AZ):MasVnrArea, foundation, BsmtFinSF1

#External quality
my.data[my.data$ExterQual=="Ex",]$ExterQual=5
my.data[my.data$ExterQual=="Gd",]$ExterQual=4
my.data[my.data$ExterQual=="TA",]$ExterQual=3
my.data[my.data$ExterQual=="Fa",]$ExterQual=2
my.data[my.data$ExterQual=="Po",]$ExterQual=1

#External Condition
my.data[my.data$ExterCond=="Ex",]$ExterCond=5
my.data[my.data$ExterCond=="Gd",]$ExterCond=4
my.data[my.data$ExterCond=="TA",]$ExterCond=3
my.data[my.data$ExterCond=="Fa",]$ExterCond=2
my.data[my.data$ExterCond=="Po",]$ExterCond=1

#BsmtQuaility
my.data$BsmtQual[is.na(my.data$BsmtQual)]<- 0
my.data[my.data$BsmtQual=="Ex",]$BsmtQual=5
my.data[my.data$BsmtQual=="Gd",]$BsmtQual=4
my.data[my.data$BsmtQual=="TA",]$BsmtQual=3
my.data[my.data$BsmtQual=="Fa",]$BsmtQual=2
my.data[my.data$BsmtQual=="Po",]$BsmtQual=1

#Basement Conditon
my.data$BsmtCond[is.na(my.data$BsmtCond)]<- 0 #should i put "NA" as 0 for rating
my.data[my.data$BsmtCond=="Ex",]$BsmtCond=5
my.data[my.data$BsmtCond=="Gd",]$BsmtCond=4
my.data[my.data$BsmtCond=="TA",]$BsmtCond=3
my.data[my.data$BsmtCond=="Fa",]$BsmtCond=2
my.data[my.data$BsmtCond=="Po",]$BsmtCond=1

#Basement Exposure:  Gd	,Av, Mn, No, NA
my.data$BsmtExposure[is.na(my.data$BsmtExposure)]<- 0
my.data[my.data$BsmtExposure=="Gd",]$BsmtExposure=4
my.data[my.data$BsmtExposure=="Av",]$BsmtExposure=3
my.data[my.data$BsmtExposure=="Mn",]$BsmtExposure=2
my.data[my.data$BsmtExposure=="No",]$BsmtExposure=1

#basement Fintype1:  GLQ,,ALQ,BLQ	,Rec	,LwQ	,Unf	,NA
my.data$BsmtFinType1[is.na(my.data$BsmtFinType1)]<- 0
my.data[my.data$BsmtFinType1=="GLQ",]$BsmtFinType1=6
my.data[my.data$BsmtFinType1=="ALQ",]$BsmtFinType1=5
my.data[my.data$BsmtFinType1=="BLQ",]$BsmtFinType1=4
my.data[my.data$BsmtFinType1=="Rec",]$BsmtFinType1=3
my.data[my.data$BsmtFinType1=="LwQ",]$BsmtFinType1=2
my.data[my.data$BsmtFinType1=="Unf",]$BsmtFinType1=1

#basement Fintype2:  GLQ,,ALQ,BLQ	,Rec	,LwQ	,Unf	,NA
my.data$BsmtFinType2[is.na(my.data$BsmtFinType2)]<- 0
my.data[my.data$BsmtFinType2=="GLQ",]$BsmtFinType2=6
my.data[my.data$BsmtFinType2=="ALQ",]$BsmtFinType2=5
my.data[my.data$BsmtFinType2=="BLQ",]$BsmtFinType2=4
my.data[my.data$BsmtFinType2=="Rec",]$BsmtFinType2=3
my.data[my.data$BsmtFinType2=="LwQ",]$BsmtFinType2=2
my.data[my.data$BsmtFinType2=="Unf",]$BsmtFinType2=1

#HeatingQC 
my.data$HeatingQC[is.na(my.data$HeatingQC)]<- 0 
my.data[my.data$HeatingQC=="Ex",]$HeatingQC=5
my.data[my.data$HeatingQC=="Gd",]$HeatingQC=4
my.data[my.data$HeatingQC=="TA",]$HeatingQC=3
my.data[my.data$HeatingQC=="Fa",]$HeatingQC=2
my.data[my.data$HeatingQC=="Po",]$HeatingQC=1

#CentralAir
my.data[my.data$CentralAir=="Y",]$CentralAir=1  
my.data[my.data$CentralAir=="N",]$CentralAir=0 


#KitchenQual
my.data[my.data$KitchenQual=="Ex",]$KitchenQual=5
my.data[my.data$KitchenQual=="Gd",]$KitchenQual=4
my.data[my.data$KitchenQual=="TA",]$KitchenQual=3
my.data[my.data$KitchenQual=="Fa",]$KitchenQual=2
my.data[my.data$KitchenQual=="Po",]$KitchenQual=1

#WHY THIS ERROR- Error in `$<-.data.frame`(`*tmp*`, KitchenQual, value = 4) : 
#replacement has 1 row, data has 0

#Fireplace
my.data$FireplaceQu[is.na(my.data$FireplaceQu)]<- 0 
my.data[my.data$FireplaceQu=="Ex",]$FireplaceQu=5
my.data[my.data$FireplaceQu=="Gd",]$FireplaceQu=4
my.data[my.data$FireplaceQu=="TA",]$FireplaceQu=3
my.data[my.data$FireplaceQu=="Fa",]$FireplaceQu=2
my.data[my.data$FireplaceQu=="Po",]$FireplaceQu=1

#Garage Type
my.data$GarageType[is.na(my.data$GarageType)]<- "None"

#GarageQual
my.data$GarageQual[is.na(my.data$GarageQual)]<- 0 
my.data[my.data$GarageQual=="Ex",]$GarageQual=5
my.data[my.data$GarageQual=="Gd",]$GarageQual=4
my.data[my.data$GarageQual=="TA",]$GarageQual=3
my.data[my.data$GarageQual=="Fa",]$GarageQual=2
my.data[my.data$GarageQual=="Po",]$GarageQual=1

#GarageCond
my.data$GarageCond[is.na(my.data$GarageCond)]<- 0 
my.data[my.data$GarageCond=="Ex",]$GarageCond=5
my.data[my.data$GarageCond=="Gd",]$GarageCond=4
my.data[my.data$GarageCond=="TA",]$GarageCond=3
my.data[my.data$GarageCond=="Fa",]$GarageCond=2
my.data[my.data$GarageCond=="Po",]$GarageCond=1

#PoolQC: Ex, Gd, TA, Fa, NA
my.data$PoolQC[is.na(my.data$PoolQC)]<- 0 
my.data[my.data$PoolQC=="Ex",]$PoolQC=4
my.data[my.data$PoolQC=="Gd",]$PoolQC=3
my.data[my.data$PoolQC=="TA",]$PoolQC=2
my.data[my.data$PoolQC=="Fa",]$PoolQC=1
#should we get rid of PoolArea and PoolQC????

#Fence
my.data$Fence[is.na(my.data$Fence)]<- "None"

#MiscFeature
my.data$MiscFeature[is.na(my.data$MiscFeature)]<- "None"








