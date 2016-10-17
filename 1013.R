#### set, load data - pre-processing ####
setwd("~/Documents/[1]Rstudio/INTELOP/LTE")

# Load package

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("ggplot2", "dplyr", "data.table", "Amelia", "caret", "Boruta","randomForest","doSNOW","rpart","rpart.plot","infotheo","Rtsne")
ipak(packages)



library(data.table)

# Load Data
data <- fread("LTEMenus.csv", sep="auto", header = TRUE, verbose = TRUE, na.strings = c("","NA"))

dim(data); str(data); summary(data)

dataa <- data

dataa$RestaurantID <- ifelse(data$RestaurantID == "", " ", substr(data$RestaurantID,4,6))

dataa$RestaurantID <- as.integer(dataa$RestaurantID)

dataa$OrderID <- ifelse(data$OrderID == "", " ", substr(data$OrderID,7,13))

dataa$OrderID <- as.integer(dataa$OrderID)

# R00001 <- leadtime[which(leadtime$RestaurantID == "R00001"),]
# Reorder
# data <- subset(data, select = c(1:3,10,4:9))
# 
# dataa <- subset(data, select = c(1,2,4:10))

#impute missing value with median value in DeliveryLeadTime
dataa$DeliveryLeadTime[is.na(dataa$DeliveryLeadTime)] <- median(dataa$DeliveryLeadTime, na.rm = TRUE)

#impute 0 in DeliveryLeadTime
dataa$DeliveryLeadTime <- ifelse(dataa$DeliveryLeadTime == 0,
                               median(dataa$DeliveryLeadTime), dataa$DeliveryLeadTime)

# remove outlier in DLT and create dataframe
(datab <- dataa[with(dataa, DeliveryLeadTime < 100), ])



#impute missing value with median value in foodtotalprice
datab$FoodTotalPrice[is.na(datab$FoodTotalPrice)] <- median(datab$FoodTotalPrice, na.rm = TRUE)

#impute 0 in ftp
datab$FoodTotalPrice <- ifelse(datab$FoodTotalPrice == 0,
                                 median(datab$FoodTotalPrice), datab$FoodTotalPrice)

# remove outlier in ftp and create dataframe
datab <- datab[with(datab, FoodTotalPrice < 1000), ]


datab$DeliveryType[is.na(datab$DeliveryType)] <- median(datab$DeliveryType, na.rm = TRUE)

# plot the missing value map
library(Amelia)
missmap(datab, main = "Missing Map")
str(datab); summary(datab)
#





#### SplitDataIntoTrainingAndTesting ####


#sample index
index = sample(1:nrow(datab), size =0.2*nrow(datab))

#split data
test = datab[index, ]
dim(test)
train = datab[-index,]
dim(train)



train$dlt <- train$DeliveryLeadTime
train$dlt <- ifelse(train$DeliveryLeadTime < 16, 1, 0)

library(dplyr)
train <- select(train, -c(1))



####Data pre-processing ####
library(caret)
train.pp = preProcess(train, method = c("range"))
train.transformed = predict(train.pp, newdata = train)
train.transformed$dlt <- as.factor(train.transformed$dlt)
summary(train.transformed)
train.transformed <- train.transformed[complete.cases(train.transformed),]
set.seed(123)


#### FeatureSelection ####
library(Boruta)
boruta.train <- Boruta(dlt~., data = train.transformed, doTrace = 2)

print(boruta.train)

plot(boruta.train, xlab="", xaxt = "n")
lz <- lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1, las=2, labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)

print(final.boruta)

plot(final.boruta, xlab="", xaxt = "n")
lz <- lapply(1:ncol(final.boruta$ImpHistory),function(i)
  final.boruta$ImpHistory[is.finite(final.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(final.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1, las=2, labels = names(Labels),
     at = 1:ncol(final.boruta$ImpHistory), cex.axis = 0.7)

getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)






#### RandomForest ####




# OrderID and restaurantID
rf.train.11 <- select(train, c(1:2))

rf.label <- as.factor(train$dlt)

library(randomForest)
set.seed(1234)
rf.11 <- randomForest(x = rf.train.11, y = rf.label, importance = TRUE, ntree = 1000)
rf.11
varImpPlot(rf.11)

# RestaurantID and cuisineType
rf.train.22 <- select(train, c(2,5))

set.seed(1234)
rf.22 <- randomForest(x = rf.train.22, y = rf.label, importance = TRUE, ntree = 1000)
rf.22
varImpPlot(rf.22)


# RestaurantID and DeliveryType
rf.train.33 <- select(train, c(2,6))

set.seed(1234)
rf.33 <- randomForest(x = rf.train.33, y = rf.label, importance = TRUE, ntree = 1000)
rf.33
varImpPlot(rf.33)

# RestaurantID, cuisineType, and DeliveryType 
rf.train.44 <- select(train, c(2,5,6))

set.seed(1234)
rf.44 <- randomForest(x = rf.train.44, y = rf.label, importance = TRUE, ntree = 1000)
rf.44
varImpPlot(rf.44)

# RestaurantID, cuisineType, and FoodtotalPrice
rf.train.55 <- select(train, c(2,5,7))

set.seed(1234)
rf.55 <- randomForest(x = rf.train.55, y = rf.label, importance = TRUE, ntree = 1000)
rf.55
varImpPlot(rf.55)

# RestaurantID, cuisineType, and menuID
rf.train.66 <- select(train, c(1,2,5))

set.seed(1234)
rf.66 <- randomForest(x = rf.train.66, y = rf.label, importance = TRUE, ntree = 1000)
rf.66
varImpPlot(rf.66)

# all
rf.train.77 <- select(train, c(1:2,5,7))

set.seed(1234)
rf.77 <- randomForest(x = rf.train.77, y = rf.label, importance = TRUE, ntree = 1000)
rf.77
varImpPlot(rf.77)



# #### RandomForest1 ####
# 
# 
# rf <- train.transformed
# 
# # OrderID and restaurantID
# rf.train.1 <- select(rf, c(1:2))
# 
# rf.label <- as.factor(rf$dlt)
# 
# library(randomForest)
# set.seed(1234)
# rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
# rf.1
# varImpPlot(rf.1)
# 
# # RestaurantID and cuisineType
# rf.train.2 <- select(rf, c(2,5))
# 
# set.seed(1234)
# rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
# rf.2
# varImpPlot(rf.2)
# 
# 
# # RestaurantID and DeliveryType
# rf.train.3 <- select(rf, c(2,6))
# 
# set.seed(1234)
# rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
# rf.3
# varImpPlot(rf.3)
# 
# # RestaurantID, cuisineType, and DeliveryType 
# rf.train.4 <- select(rf, c(2,5,6))
# 
# set.seed(1234)
# rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
# rf.4
# varImpPlot(rf.4)
# 
# # RestaurantID, cuisineType, and FoodtotalPrice
# rf.train.5 <- select(rf, c(2,5,7))
# 
# set.seed(1234)
# rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
# rf.5
# varImpPlot(rf.5)
# 
# # RestaurantID, cuisineType, and menuID
# rf.train.6 <- select(rf, c(1,2,5))
# 
# set.seed(1234)
# rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
# rf.6
# varImpPlot(rf.6)
# 
# # all
# rf.train.7 <- select(rf, c(1:2,5,7))
# 
# set.seed(1234)
# rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
# rf.7
# varImpPlot(rf.7)


#### Cross Validation ####
library(doSNOW)

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

# Check stratification
table(rf.label)


table(rf.label[cv.10.folds[[33]]])
308 / 494
14805/22324

# Set up caret's trainControl object per above.
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                       index = cv.10.folds)


# Set up doSNOW package for multi-core training. This is helpful as we're going
# to be training a lot of trees.
# NOTE - This works on Windows and Mac, unlike doMC
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)


# Set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.55, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.1


# The above is only slightly more pessimistic than the rf.5 OOB prediction, but 
# not pessimistic enough. Let's try 5-fold CV repeated 10 times.
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10,
                       index = cv.5.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.55, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.2


# 5-fold CV isn't better. Move to 3-fold CV repeated 10 times. 
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10,
                       index = cv.3.folds)

cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.55, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 64, trControl = ctrl.3)

#Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.3


#### exploratory modeling ####

library(rpart)
library(rpart.plot)


rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  # Leverage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, 
                    trControl = ctrl)
  
  #Shutdown cluster
  stopCluster(cl)
  
  return (rpart.cv)
}


features <- c("RestaurantID", "CuisineType","FoodTotalPrice")

rpart.train.1 <- select(train, c(2,5,7))

rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.1)
rpart.1.cv.1

#Plot
prp(rpart.1.cv.1$finalModel, type=0, extra = 1, under = TRUE)




# Grab features
features <- c("RestaurantID","CuisineType")

rpart.train.2 <- select(train, c(2,5))

# Run CV and check out results
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.1)
rpart.2.cv.1

# Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


test.submit <- select(test, c(3,6,8))

rpart.1.preds <- predict(rpart.1.cv.1$finalModel, test.submit, type = "class")
table(rpart.1.preds)

test$pred <- rpart.1.preds
test$dlt <- test$DeliveryLeadTime
test$dlt <- ifelse(test$DeliveryLeadTime < 16, 1, 0)



