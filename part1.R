library(descr, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library( ROCR , warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(foreach)
library(RCurl)
library(mlbench)
library(pbkrtest)
library(caret)
setwd("C:\\Users\\pc\\Dropbox\\Raz&Livnat\\lastYear\\b\\ofrit")

x<- read.csv('Accidents_2014.csv',header = TRUE)
head(x)

#boxplot(x$Number_of_Casualties,col="blue")
barplot(table(x$Urban_or_Rural_Area),col="pink",main="Number of Accidents per Area")

hist(x$Day_of_Week,col="green")
abline(v=12,lwd=2,col="red")
abline(v=median(x$Day_of_Week),lwd=4,col="yellow")

boxplot(Number_of_Vehicles~Number_of_Casualties,data=x,col="red")
#hist(x$Number_of_Casualties,col="green",vreaks=100)

x$Local_Authority_.Highway.<-NULL
x$LSOA_of_Accident_Location<-NULL
x$Accident_Index<-NULL
x$Date<-NULL
x$Time<-NULL
## build a correlation matrix based on the first 100000 rows ##
corr.matrix = cor(x, use = "pairwise.complete.obs")
corr.matrix[is.na(corr.matrix)] = 0
write.csv(corr.matrix,'corrMatrix.csv')

corr.list = foreach(i = 1:nrow(corr.matrix)) %do% {
  rownames(corr.matrix[corr.matrix[,i] > 0.996,])
}
## remove empty sets ##
corr.list = corr.list[sapply(corr.list, function(x) length(x) > 0 )]
## remove duplicated sets ##
corr.list = unique(corr.list)
corr.list

varX<-apply(x, 2, var)
write.csv(varX,'variance.csv')

require(ggmap)
map<-get_map(location='england', zoom=7, maptype = "terrain",
             source='google',color='color')

require("ggplot2")
require("RColorBrewer")
ggmap(map) + geom_point(
  aes(x=Longitude, y=Latitude, show_guide = TRUE, 
      colour=Number_of_Casualties), data=x, 
  alpha=.6, na.rm = T)  + 
  scale_color_gradient(low="green", high="red")