#HOUSING DATA
#AGE OF THE HOUSES 

my.data<-read.csv(file.choose())


current.year<- 2018

#Age from year built
age.YBuilt<- current.year- (my.data$YearBuilt)

#Age from year remodelled
age.YRemod<- current.year- (my.data$YearRemodAdd)

#Adding the age columns in my.data.copy
my.data$Age.YearBuilt<- age.YBuilt
my.data$YearRemod<- age.YRemod

#Should we filter the houses that were never remodelled and the onces that were
#remodlled.  Also, should we consider that a remodelled a old house is 
#equivalent to a newly contructed house? 
if(age.YBuilt==age.YRemod){
  youth<-age.YRemod
} else{ youth<- age.YBuilt}

my.data$Youth<-youth
