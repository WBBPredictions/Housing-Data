#HOUSING DATA
#AGE OF THE HOUSES 
my.data.copy<-my.data

current.year<- 2018

#Age from year built
age.YBuilt<- current.year- (my.data.copy$YearBuilt)

#Age from year remodelled
age.YRemod<- current.year- (my.data.copy$YearRemodAdd)

#Adding the age columns in my.data.copy
my.data.copy$Age.YearBuilt<- age.YBuilt
my.data.copy$Age.YearRemod<- age.YRemod
#Should we filter the houses that were never remodelled and the onces that were
#remodlled.  Also, should we consider that a remodelled a old house is 
#equivalent to a newly contructed house? 