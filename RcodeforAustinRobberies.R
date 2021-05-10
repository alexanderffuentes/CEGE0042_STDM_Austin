library(maptools)
library(lattice)
library(spdep)
library(sp)
library(rgdal)
library(tmap)
library(ggplot2)
library(gridExtra)
library(gstat)
library(OpenStreetMap)
library(spacetime)
library(randomForest)
library(wesanderson)
library(dplyr)

setwd("F:\\UCL\\CEGE0042_STDM\\Coursework\\Data") #set to your local data folder


AustinRobberyIncCountGenMY <- readOGR(dsn="ARICGMY.shp", layer="ARICGMY")
RobDat <- read.csv('AustinRobberies.csv')

#####
#Exploratory Analysis#
#####

#Below code adapted from (https://towardsdatascience.com/analyzing-london-crimes-data-in-r-aee4658f79fe)

RobYear <- RobDat %>% group_by(Year) %>% tally() %>% arrange(desc(n))  



#pct_change_yr <- mutate(RobYear,(n-lag(n))/lag(n))

ggplot(RobYear, aes(x = Year, y =n)) +
  geom_bar(stat='identity', fill="#00AFBB", color="black")+
  geom_line(color="firebrick", size=1, linetype="dashed")+
  #  geom_text(aes(label=percent(pct_change)),vjust=-1,fontface="bold", color="black")+  
  geom_text(aes(label=comma(n)),vjust=1,fontface="bold",color="white")+ 
  theme_classic()+                                          
  labs(title = "Total Robberies over years")



RobDay <- RobDat %>% group_by(DayN) %>% tally() %>% arrange(desc(n))  


ggplot(RobDay, aes(x = DayN, y =n)) +
  geom_bar(stat='identity', fill="#00AFBB", color="black")+
  geom_line(color="firebrick", size=1, linetype="dashed")+
  geom_text(aes(label =c("Sunday", "Saturday", "Monday", "Friday", "Wednesdsay", "Thursday","Tuesday"),vjust=-1))+ 
  geom_text(aes(label=comma(n)),vjust=1,fontface="bold",color="white")+ 
  theme_classic()+                                          
  labs(title = "Total Robberies over the days of the week", y = "Count", x = "Days")


#####
# Random Forests
####

ARICGMY_DF <- data.frame(AustinRobberyIncCountGenMY)

# Below code from Haworth, J. (2020). Spatio-temporal Analytics in R, 7.3.3 Random Forests.

y <- ARICGMY_DF$TOTAL_CNT
yMean <- mean(y>0)
yClass <- y
yClass[which(y>=mean(y))] <- 1
yClass[which(y<mean(y))] <- -1
# Plot the corn yield high low classes
brks <- c(-1,1)
lbls <- findInterval(yClass, brks)
cols <- c("blue", "red")
plot(AustinRobberyIncCountGenMY, col=cols[lbls])
legend("bottomleft", legend=c("Low", "High"), fill=cols, title="Class")
title(main="Robbery Class")


X <- ARICGMY_DF[,c(
  58:72
)]


set.seed(101)
n <- nrow(X)
trainInd <- sort(sample(1:nrow(X), n*.8))# Select 80% of samples at random
XTrain <- X[trainInd,]
XTest <- X[-trainInd,]                   # Select the remainder for testing

yClassTrain <- as.factor(yClass[trainInd]) #if you don't make y a factor, RF tries to do regression
yClassTest <- as.factor(yClass[-trainInd])





#Below section of code adapted from https://rpubs.com/ksakwa/489687

m2 <- tuneRF(
  x          = XTrain,
  y          = as.factor(yClassTrain),
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)
m2

plot(m2)

# mtry=7

modelClass <- randomForest(
  x = XTrain, 
  y = as.factor(yClassTrain),
  ntree=500,
  mtry=7,
  importance=TRUE 
)

predClass <- predict(modelClass, XTest)

predClass<-as.matrix(as.numeric(predClass))
yClassTest<-as.matrix(as.numeric(yClassTest)) # convert y back to a number to calculate error
predErr <- length(which((predClass-yClassTest)>0))/length(predClass)
predErr




par(bg='grey')
predRes <- yClassTest-predClass
brks <- c(-2,0,2)
lbls <- findInterval(predRes, brks)
#cols <- col=wes_palette(n=2, name="Darjeeling")
cols <- c("blue4", "grey", "red")
plot(AustinRobberyIncCountGenMY[-trainInd,], col=cols[lbls])
legend("topright", legend=c("-ve classed +ve", "Correct", "+ve classed -ve"), fill=cols, title="Class")
title(main="RF Classification Errors, Robberies in Austin, Tx")

#mtry=5

modelClass <- randomForest(
  x = XTrain, 
  y = as.factor(yClassTrain),
  ntree=500,
  mtry=5,
  importance=TRUE 
)

predClass <- predict(modelClass, XTest)

predClass<-as.matrix(as.numeric(predClass))
yClassTest<-as.matrix(as.numeric(yClassTest)) # convert y back to a number to calculate error
predErr <- length(which((predClass-yClassTest)>0))/length(predClass)
predErr




par(bg='grey')
predRes <- yClassTest-predClass
brks <- c(-2,0,2)
lbls <- findInterval(predRes, brks)
#cols <- col=wes_palette(n=2, name="Darjeeling")
cols <- c("blue4", "grey", "red")
plot(AustinRobberyIncCountGenMY[-trainInd,], col=cols[lbls])
legend("topright", legend=c("-ve classed +ve", "Correct", "+ve classed -ve"), fill=cols, title="Class")
title(main="RF Classification Errors, Robberies in Austin, Tx")

# mtry=5 has less errors

varImpPlot(modelClass)


