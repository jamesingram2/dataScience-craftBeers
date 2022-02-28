#####################################
# Install Packages and Load Libraries
#####################################

packages <- c("tidyverse", "RWeka","rJava","caret","e1071","kernlab", "randomForest", "class", "cluster", "ggplot2", "maps", "mapdata", "maptools", "rstudioapi")
package.check <- lapply(packages, FUN = function(x){
  if(!require(x, character.only=TRUE)){
    install.packages(x, dependencies=TRUE)
    library(x, character.only=TRUE)
  }
})

##############################
# Data Load and Pre-Processing
##############################

setwd(dirname(getActiveDocumentContext()$path))
beers <- read.csv("beers.csv")
breweries <- read.csv("breweries.csv")

# examine datasets
str(beers)
sum(is.na(beers)) # find total NAs in beers data set
str(breweries)
sum(is.na(breweries)) # find total NAs in breweries data set

# omit NAs
beers <- na.omit(beers)
rownames(beers) <- NULL # resets row counts after NAs are removed

# remove beers$X and beers$id
beers <- beers[, -1,]
beers <- beers[, -3]
head(beers) # examine structure of beers dataframe

# rename beers$name to beers$beer_name
cnames <- colnames(beers)
cnames[3] <- "beer_name"
colnames(beers) <- cnames
colnames(beers)

# rename breweries$X to breweries$brewery_id and breweries$name to breweries$brewery_name
cnames <- colnames(breweries)
cnames[1] <- "brewery_id"
cnames[2] <- "brewery_name"
colnames(breweries) <- cnames
colnames(breweries)

# merge beers and breweries data sets on brewery_id
df <- merge(x = beers, y = breweries, by = "brewery_id")

# re-order column headings and remove brewery_id from dataframe
df <- df[,c(4,5,2,3,6,7,8,9)]
head(df) # examine new dataframe
View(head(df,25))

# standardize ounces
df$ounces[df$ounces==8.4]<-8
df$ounces[df$ounces==16.9]<-16
df$ounces[df$ounces==19.2]<-20

###########################
# Exploratory Data Analysis
###########################

# display frequency distribution of abv
g.abv <- ggplot(df, aes(x=abv))
g.abv <- g.abv + geom_histogram(bins=10, color = "black", fill="white")
g.abv <- g.abv + ggtitle("Histogram of Alcohol By Volume")
g.abv
# examine central tendency and range of abv
mean(df$abv)
g.abv.box <- ggplot(df, aes(x=factor(0),abv)) + geom_boxplot() + ggtitle("Alcohol Content")
g.abv.box
# sort beers with highest alcohol by volume (display top 10)
sortedAbv <- df[order(-df$abv), ]
View(head(sortedAbv,10))

# display frequency distribution of ibu
g.ibu <- ggplot(df, aes(x=ibu))
g.ibu <- g.ibu + geom_histogram(bins=10, color = "black", fill="white")
g.ibu <- g.ibu + ggtitle("Histogram of International Bittering Units")
g.ibu
# examine central tendency and range of ibu
mean(df$ibu)
g.ibu.box <- ggplot(df, aes(x=factor(0),ibu)) + geom_boxplot() + ggtitle("International Bittering Units")
g.ibu.box
# sort beers with highest international bitter units (display top 10)
sortedIbu <- df[order(-df$ibu), ]
View(head(sortedIbu, 10))

# display frequency distribution of ounces
g.ounces <- ggplot(df, aes(x=ounces))
g.ounces <- g.ounces + geom_histogram(binwidth = 4, color = "black", fill="white")
g.ounces <- g.ounces + ggtitle("Histogram of Beer Volume (in Ounces)") + xlim(0,32)
g.ounces
# examine central tendency and range of ounces
mean(df$ounces)
g.ounces.box <- ggplot(df, aes(x=factor(0),ounces)) + geom_boxplot() + ggtitle("Beer Ounces")
g.ounces.box
# sort beers with largest volume in ounces (display top 10)
sortedOunces <- df[order(-df$ounces), ]
View(head(sortedOunces, 10))

# Frequency of beers by state
stateBeers <- barplot(table(df$state), main="Beers by State", las=2)

# Discretize variables

# discretization of "abv"
df$abv <- cut(df$abv, breaks = c(0,0.025, 0.0375, 0.050, 0.0625, 0.075, 0.0875, 0.1000, 0.1125, 0.1250, Inf), labels = c(1,2,3,4,5,6,7,8,9,10))
plot(df$abv, main="Discretized Alcohol By Volume")

# discretization of ibu
df$ibu <- cut(df$ibu, breaks = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,Inf), labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
plot(df$ibu, main="Discretized International Bittering Units")

# discretization of ounces
df$ounces <- cut(df$ounces, breaks = c(0,8,12,16,20,24,30,Inf), labels=c(1,2,3,4,5,6,7))
plot(df$ounces, main="Discretized Beer Volume")

#######################
# Classification Models
#######################

# create data set with only style, abv, ibu, and ounces
trainData <- df[,2:5]

#########################################
# Decision Tree Modeling (J48 from RWeka)
#########################################

# J48 decision tree model with default values
model_dt <- J48(style~.,data=trainData)
model_dt

# use 10-fold cross-validation to evaluate model_dt
e <- evaluate_Weka_classifier(model_dt, numFolds=10, seed=1, class=TRUE)
e$details

# improve model with increased confidence
model_dt2 <- J48(style~.,data=trainData, control=Weka_control(U=FALSE, M=2, C=0.5))
model_dt2

# use 10-fold cross-validation to evaulate model_dt2
e2 <- evaluate_Weka_classifier(model_dt2, numFolds=10, seed=1, class=TRUE)
e2$details

# examine the effect of pruning by creating unpruned tree
model_dt3 <- J48(style~.,data=trainData, control=Weka_control(U=TRUE, M=2))
model_dt3

# use 10-fold cross-validation to evaulate model_dt3
e3 <- evaluate_Weka_classifier(model_dt3, numFolds=10, seed=1, class=TRUE)
DT_results <- e3$details
DT_results

# evaluate information gain
InfoGainAttributeEval(style~.,data=trainData)

##################################
# Naive Bayes Classification Model
##################################

# RWeka NB Model with defualt values
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
model_nb <- NB(style~.,data=trainData)
model_nb

# use 10-fold cross-validation to evaulate the model_nb
e_nb <- evaluate_Weka_classifier(model_nb, numFolds=10, seed=1, class=TRUE)
NB_results <- e_nb$details
NB_results

