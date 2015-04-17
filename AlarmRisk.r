#
# AlarmRisk.R
#
#===================
#
# To make a model that predicts which houses are missing smoke alarms, we first read in the AHS data (this is a very large file, and takes a while to load).
# The data can be accessed from the Census Bureau's AHS page at http://www.census.gov/programs-surveys/ahs/data/2011/ahs-national-and-metropolitan-puf-microdata.html (the file newhouse contains structure-specific data).
# We then select the variables of interest, as well as key geographic variables. We chose variables for our model that are available at discrete geographies and best predict whether or not a house is missing a smoke alarm: the age of the structure, the occupants' poverty level income, and the length of time the occupants have been in their home. Due to differing characteristics of different regions, it is recommended that different variables are tested for relevance if this model is being applied in other cities.
#
#===================
#
#


setwd("YOUR_WORKING_DIRECTORY")
packages = "YOUR_PACKAGE_DIRECTORY"
.libPaths(packages)

require(reshape)
require(plyr)
require(dplyr)
require(ROCR)

AHS <- read.csv("AHSnewHouse.csv",na.strings=c(-9,-8,-7,-6,"-9","-8","-7","-6","'-9'","'-8'","'-7'","'-6'"))
geo.vars <- subset(AHS,select=c(SMSA,COUNTY,METRO3,SMOKE,BUILT,POOR,HHMOVE))

#Then we clean the data and filter for New Orleans. Because ACS data is reported as the percent of population in bins rather than exact values, we convert the independent variables from continuous to binary comparisons to a threshhold.
 to.num <- function(vec){
	if(is.numeric(vec)==FALSE){
		vec <- as.character(vec)
		return(as.numeric(substr(vec,2,nchar(vec)-1)))}
	else{
    return(vec)
  }
}
for(j in 3:ncol(geo.vars)){
	if(is.numeric(geo.vars[,j])==FALSE){geo.vars[,j] <- to.num(geo.vars[,j])}
}
geo.vars$SMOKE <- to.num(geo.vars$SMOKE)
geo.vars$SMOKE <- as.numeric(gsub(pattern=1,replacement=0,x=geo.vars$SMOKE))
geo.vars$SMOKE <- as.numeric(gsub(pattern=2,replacement=1,x=geo.vars$SMOKE))
geo.vars$B1950 <- (geo.vars$BUILT<1950)+0
geo.vars$B200P <- (geo.vars$POOR<200)+0
geo.vars$MOVEB00 <- (geo.vars$HHMOVE<2000)+0

nola.met <- geo.vars[which(as.character(geo.vars$SMSA)=="'5560'"),] #The New Orleans Metro is SMSA code 5560
nola <- nola.met[which(as.character(nola.met$COUNTY)=="'071'"),]  #Orleans Parish is County Code 071

#By looking at the cross tabs, we can see the effect that each variable has on the likelihood of needing a smoke alarm
vars <- subset(nola,select=c(SMOKE,B1950,B200P,MOVEB00))
sum.stats <- ddply(vars, names(vars)[2:ncol(vars)], summarise,mean = mean(SMOKE,na.rm=TRUE),num = length(SMOKE))
sum.stats[complete.cases(sum.stats),]

#Now we can make a logistic regression model and find the weights:
md <- glm(SMOKE~., data=vars, family=binomial("logit"))
coef <- summary(md)$coefficients
coef

#We test the model to find how efficient it is at identifying homes in need of smoke alarms compared to a random search:
split.data <- function(data, split=0.6) {
  n <- nrow(data)
  samp <- sample(1:n, n*split)
  train <- data[samp, ]
  test <- data[-samp,]
  list(train=train, test=test)
}
vars.split <- split.data(vars)
vars.train <- vars.split$train
vars.test <- vars.split$test
md.train <- glm(SMOKE~., data=vars.train, family=binomial("logit"))
predicted <- predict(md.train, vars.test, type="response")
pred.df <- data.frame(cbind(pred=predicted,actual=vars.test$SMOKE))
pred.df <- pred.df[complete.cases(pred.df),]
pred.df <- pred.df[order(-pred.df$pred),]
pred <- prediction(pred.df$pred,pred.df$actual)
perf <- performance(pred,"tpr","rpp")
plot(perf,,xlab="Percent of All Houses Visited",ylab="Percent of Houses Needing Alarms Found", main="Estimated Smoke Alarm Distribution by Total Coverage")
abline(0,1,col="grey",lty=2)

#Now we can save our regression weights and use our model to target specific areas.
save(coef,file="weights.RData")
