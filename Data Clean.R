library(readr)
library(PCAmixdata)
library(FactoMineR)
train <- read.csv("~/Desktop/Projects/Trav + Keven/train.csv")
SP <- train$SalePrice
dat <- train[,-c(1)]
dat <- dat[, -80]
#View(dat)



#Clean Alley, Pool QC, fence, fireplace quality
for(i in 1:nrow(dat))
{
  if(is.na(dat[i, 6]) == T)
    dat[i, 6] = "None"
  if(is.na(dat[i, 72]) == T)
    dat[i, 72] = "None"
  if(is.na(dat[i, 73]) == T)
    dat[i, 73] = "None"
  if(is.na(dat[i, 57]) == T)
    dat[i, 57] = "None"
} 
# Eliminate the "over NA'd column" misc feature and misc val 
dat <- dat[, -c(74, 75)]

# make all catagoricals letters, and all continuous numerics
## 1st col
temp <- rep("A", nrow(dat))
for(i in 1:nrow(dat))
{
  if(dat[i, 1] == 20)
    temp[i] = "A"
  if(dat[i, 1] == 30)
    temp[i] = "B"
  if(dat[i, 1] == 40)
    temp[i] = "C"
  if(dat[i, 1] == 45)
    temp[i] = "D"
  if(dat[i, 1] == 50)
    temp[i] = "E"
  if(dat[i, 1] == 60)
    temp[i] = "F"
  if(dat[i, 1] == 70)
    temp[i] = "G"
  if(dat[i, 1] == 75)
    temp[i] = "H"
  if(dat[i, 1] == 80)
    temp[i] = "I"
  if(dat[i, 1] == 85)
    temp[i] = "J"
  if(dat[i, 1] == 90)
    temp[i] = "K"
  if(dat[i, 1] == 120)
    temp[i] = "L"
  if(dat[i, 1] == 150)
    temp[i] = "M"
  if(dat[i, 1] == 160)
    temp[i] = "N"
  if(dat[i, 1] == 180)
    temp[i] = "O"
  if(dat[i, 1] == 190)
    temp[i] = "P"
  
}
dat[,1] = temp
View(dat)

## exterqual, extercond, bsmtqual
index = 0
dat[18, 31]
for(i in 1:nrow(dat))
{
  #print(index)
  if(is.na(dat[i, 31]) == T)
    dat[i, 31] = as.integer(0)
  else if(dat[i, 31] == "Ex")
    dat[i, 31] = as.integer(5)
  else if(dat[i, 31] == "Gd")
    dat[i, 31] = as.integer(4)
  else if(dat[i, 31] == "TA")
    dat[i, 31] = as.integer(3)
  else if(dat[i, 31] == "Fa")
    dat[i, 31] = as.integer(2)
  else if(dat[i, 31] == "Po")
    dat[i, 31] = as.integer(1)
  
  
  
  
  if(dat[i, 28] == "Ex")
    dat[i, 28] = 5
  if(dat[i, 28] == "Gd")
    dat[i, 28] = 4
  if(dat[i, 28] == "TA")
    dat[i, 28] = 3
  if(dat[i, 28] == "Fa")
    dat[i, 28] = 2
  if(dat[i, 28] == "Po")
    dat[i, 28] = 1
  
  if(dat[i, 27] == "Ex")
    dat[i, 27] = 5
  if(dat[i, 27] == "Gd")
    dat[i, 27] = 4
  if(dat[i, 27] == "TA")
    dat[i, 27] = 3
  if(dat[i, 27] == "Fa")
    dat[i, 27] = 2
  if(dat[i, 27] == "Po")
    dat[i, 27] = 1
  
  if(dat[i, 40] == "Ex")
    dat[i, 40] = 5
  if(dat[i, 40] == "Gd")
    dat[i, 40] = 4
  if(dat[i, 40] == "TA")
    dat[i, 40] = 3
  if(dat[i, 40] == "Fa")
    dat[i, 40] = 2
  if(dat[i, 40] == "Po")
    dat[i, 40] = 1
  
  if(dat[i, 53] == "Ex")
    dat[i, 53] = 5
  if(dat[i, 53] == "Gd")
    dat[i, 53] = 4
  if(dat[i, 53] == "TA")
    dat[i, 53] = 3
  if(dat[i, 53] == "Fa")
    dat[i, 53] = 2
  if(dat[i, 53] == "Po")
    dat[i, 53] = 1
  
  if(is.na(dat[i, 57]) == T)
    dat[i, 57] = 0
  if(dat[i, 57] == "Ex")
    dat[i, 57] = 5
  if(dat[i, 57] == "Gd")
    dat[i, 57] = 4
  if(dat[i, 57] == "TA")
    dat[i, 57] = 3
  if(dat[i, 57] == "Fa")
    dat[i, 57] = 2
  if(dat[i, 57] == "Po")
    dat[i, 57] = 1
  
  
  if(is.na(dat[i, 63]) == T)
    dat[i, 63] = 0
  if(dat[i, 63] == "Ex")
    dat[i, 63] = 5
  if(dat[i, 63] == "Gd")
    dat[i, 63] = 4
  if(dat[i, 63] == "TA")
    dat[i, 63] = 3
  if(dat[i, 63] == "Fa")
    dat[i, 63] = 2
  if(dat[i, 63] == "Po")
    dat[i, 63] = 1
  
  
  #index = index + 1
}
dat$ExterQual <- as.integer(dat$ExterQual)
dat$ExterCond <- as.integer(dat$ExterCond)
dat$BsmtCond <- as.integer(dat$BsmtCond)
dat$HeatingQC <- as.integer(dat$HeatingQC)
dat$KitchenQual <- as.integer(dat$KitchenQual)
dat$FireplaceQu <- as.integer(dat$FireplaceQu)
dat$GarageQual <- as.integer(dat$GarageQual)

View(dat)
X.quanti <- splitmix(dat)$X.quanti
View(X.quanti)

X.quali <- splitmix(dat)$X.quali
View(X.quali)
for(i in 1:ncol(X.quali))
{
  X.quali[,i] <- as.factor(X.quali[,i])
  
}
