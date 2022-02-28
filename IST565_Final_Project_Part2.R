#####################################
# Install Packages and Load Libraries
#####################################

packages <- c("tidyverse", "RWeka","rJava","caret","e1071","kernlab", "randomForest", "class", "cluster", "ggplot2", "ggmap", "maps", "mapdata", "maptools", "rstudioapi")
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
beers <- read.csv("beers_new.csv")
breweries <- read.csv("breweries.csv")

# examine datasets
str(beers)
sum(is.na(beers)) # find total NAs in beers data set
str(breweries)
sum(is.na(breweries)) # find total NAs in breweries data set

# omit NAs
beers <- na.omit(beers)
rownames(beers) <- NULL # resets row counts after NAs are removed

# remove beers$Column1 and beers$id
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
View(head(df,25))

# standardize ounces
df$ounces[df$ounces==8.4]<-8
df$ounces[df$ounces==16.9]<-16
df$ounces[df$ounces==19.2]<-20

# create new csv file containing cleaned data set
write.csv(df, file="clean_beer_data.csv")

###########################
# Exploratory Data Analysis
###########################

# frequency of each beer style
barplot(table(df$style), main="Beer Style Frequency", xlab="Styles", ylab="Frequency", cex.axis=0.5)

# ABV
# display frequency distribution of abv
g.abv <- ggplot(df, aes(x=abv))
g.abv <- g.abv + geom_histogram(bins=10, color = "blue", fill="light blue")
g.abv <- g.abv + ggtitle("Histogram of Alcohol By Volume")
g.abv <- g.abv + theme(plot.title = element_text(hjust = 0.5))
g.abv
# sort beers with highest alcohol by volume (display top 10)
sortedAbv <- df[order(-df$abv), ]
View(head(sortedAbv,10))
# Look at ABV by Beer Style
plot(x=df$style, y=df$abv, xlab="Beer Styles", ylab="ABV", cex.axis=1, main="Alcohol By Volume")

# IBU
# display frequency distribution of ibu
g.ibu <- ggplot(df, aes(x=ibu))
g.ibu <- g.ibu + geom_histogram(bins=10, color = "red", fill="pink")
g.ibu <- g.ibu + ggtitle("Histogram of International Bittering Units")
g.ibu <- g.ibu + theme(plot.title = element_text(hjust = 0.5))
g.ibu
# sort beers with highest international bitter units (display top 10)
sortedIbu <- df[order(-df$ibu), ]
View(head(sortedIbu, 10))
# Look at IBU by Beer Style
plot(x=df$style, y=df$ibu, xlab="Beer Styles", ylab="IBU", cex.axis=1, main="International Bittering Units")

# display frequency distribution of ounces
g.ounces <- ggplot(df, aes(x=ounces))
g.ounces <- g.ounces + geom_histogram(binwidth = 4, color = "black", fill="light yellow")
g.ounces <- g.ounces + ggtitle("Histogram of Beer Volume (in Ounces)") + xlim(0,32)
g.ounces <- g.ounces + theme(plot.title = element_text(hjust = 0.5))
g.ounces
# sort beers with largest volume in ounces (display top 10)
sortedOunces <- df[order(-df$ounces), ]
View(head(sortedOunces, 10))
# Look at ounces by Beer Style
plot(x=df$style, y=df$ounces, xlab="Beer Styles", ylab="ounces", cex.axis=0.5, main="Beer Volume (Ounces)")

# relationship between abv and ibu
volume <- as.factor(df$ounces)
qplot(data=df, x=abv, y=ibu, color=style, size=volume, main="ABV and IBU by Style") + theme(plot.title = element_text(hjust = 0.5))

# Frequency of beers by state
stateBeers <- barplot(table(df$state), main="Beers by State", las=2)

# relationship between style and state
qplot(data=df, x=state, y=style, main="Beer Styles in each State", color=df$style, size=3) + theme(legend.position="none") + theme(plot.title = element_text(hjust = 0.5))

# map visualization of styles per state
# function from: https://favorableoutcomes.wordpress.com/2012/10/19/create-an-r-function-to-convert-state-codes-to-full-state-name/
stateFromLower <-function(x) {
st.codes<-data.frame(
  state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  st.x<-data.frame(state=x)
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  return(refac.x)
}
states<-map_data("state")
df$state<-gsub("[[:space:]]", "", df$state)
df$region<-stateFromLower(df$state)

# text data for maps
counts<-as.data.frame(table(df$state)) # no. of observations per state
colnames(counts)<-c("state.abb","count")
txt <- data.frame(state.center, state.abb)
d1<-txt
d2<-counts
lab<-merge(d1,d2, by = "state.abb", all=FALSE)
rm(counts,txt,d1,d2)

plot.data <- inner_join(states, agg, by = "region")

df$styles<-as.character(df$style)
df.new<-within(df,{no.styles<-ave(styles,region,FUN=function(x) length(unique(x)))})
agg<-subset(df.new,select=c("region","no.styles"))
agg<-unique(agg)
agg$no.styles<-as.numeric(paste(agg$no.styles))
plot.data <- inner_join(states, agg, by = "region")

ggplot(data = plot.data, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +   geom_polygon(data = plot.data, aes(fill = no.styles), color = "white") +
  geom_polygon(color = "black", fill = NA) +theme_bw() +labs( title="Styles Per State") +
  scale_fill_gradientn("no. of styles",colors=c("pink","blue" )) +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())+
  geom_text(data = lab, aes(x = x, y = y, label = count, group = NULL), size = 2)+theme_bw()

#######################
# Classification Models
#######################

# create data set with only style, abv, and ibu
trainData <- df[,-1] #remove beer name
trainData <- trainData[,-4:-9] #remove ounces, brewery name, city, and state, and added columns

# hold-out method to measure performance
randIndex <- sample(1:dim(trainData)[1])
cutpoint2_3 <- floor(2*dim(trainData)[1]/3)
new_train <- trainData[randIndex[1:cutpoint2_3], ]
new_validation <- trainData[randIndex[(cutpoint2_3+1):dim(trainData)[1]],]
train_labels <- new_train$style

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
e3$details

# evaluate information gain
ig <- InfoGainAttributeEval(style~.,data=trainData)
cat("Information Gain:\n\n'abv' 'ibu' \n", ig)

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

# Naive Bayes using e1071
NB_model <- naiveBayes(style~.,data=trainData)
NB_model
NB_pred <- predict(NB_model, trainData)
table(NB_pred, trainData$style)

NB_model2 <- naiveBayes(style~.,data=new_train)
NB_model2
NB_pred2 <- predict(NB_model2, new_validation)
table(NB_pred2, new_validation$style)

###############################
# Support Vector Machines Model
###############################

# SVM linear model using caret package
set.seed(1818)
train_control <- trainControl(method="cv", number=10)
fit_svm_linear <- train(style~.,data=new_train,
                        method="svmLinear",
                        traControl = train_control,
                        na.action=na.pass)
# cross-validation performance
fit_svm_linear

pred_svm_linear <- predict(fit_svm_linear, newdata=new_validation)
table(pred_svm_linear)

confusionMatrix(pred_svm_linear, new_validation$style)

# SVM RBF model using caret package
fit_svm_rbf <- train(style~.,data=new_train,
                     method="svmRadial",
                     traControl = train_control,
                     na.action=na.pass)
# cross-validation performance
fit_svm_rbf

pred_svm_rbf <- predict(fit_svm_rbf, newdata=new_validation)
table(pred_svm_rbf)

confusionMatrix(pred_svm_rbf, new_validation$style)
