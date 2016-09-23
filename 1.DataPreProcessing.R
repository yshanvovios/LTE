#### Set WD, load Data and packages ####

# set WorkingDirectory
setwd("~/Documents/[1]Rstudio/INTELOP/LTE")

# Load package
library(data.table)

# Load Data
res <- fread("order_restaurant.csv", sep="auto", header = TRUE, verbose = TRUE, na.strings = c("","NA"))


# plot the missing value map
library(Amelia)
missmap(res, main = "Missing Map")
str(res)
summary(res)
#

# load package
library(dplyr)

# remove outlier in DLT and create dataframe
(dt <- res[with(res, DeliveryLeadTime < 100), ])

summary(dt$DeliveryLeadTime)

#remove variables
dt <- select(dt, -c(4:6,11,24:38,40:50))

#### wrangling DeliveryLeadTime Variable ####
# impute missing value in DeliveryLeadTime
dt$DeliveryLeadTime[is.na(dt$DeliveryLeadTime)] <- median(dt$DeliveryLeadTime, na.rm = TRUE)

#impute 0 with mean value in DLT
dt$DeliveryLeadTime <- ifelse(dt$DeliveryLeadTime == 0, median(dt$DeliveryLeadTime), dt$DeliveryLeadTime)

summary(dt$DeliveryLeadTime)

### plot the missing value map
missmap(dt, main = "Missing Map")
str(dt)
summary(dt)
#

#take care of orderstatus
str(dt$OrderStatus)
summary(dt$OrderStatus)
table(dt$OrderStatus)

dt$OrderStatus[is.na(dt$OrderStatus)] <- 81

dt$dlt <- dt$DeliveryLeadTime
dt$dlt <- ifelse(dt$DeliveryLeadTime < 16, 1, 0)

# set factor
dt$DeliveryLeadTime <- as.factor(dt$DeliveryLeadTime)

# ggplot leadtime
library(ggplot2)
ggplot(dt, aes(x = DeliveryLeadTime)) +
  geom_bar() +
  xlab("leadtime") +
  ylab("Total Count") +
  labs(fill = "deliverytype") 


#### Pre-Processing for Machine Learning ####
# “BoxCox“: apply a Box–Cox transform, values must be non-zero and positive.
# “YeoJohnson“: apply a Yeo-Johnson transform, like a BoxCox, but values can be negative.
# “expoTrans“: apply a power transform like BoxCox and YeoJohnson.
# “zv“: remove attributes with a zero variance (all the same value).
# “nzv“: remove attributes with a near zero variance (close to the same value).
# “center“: subtract mean from values.
# “scale“: divide values by standard deviation.
# “range“: normalize values.
# “pca“: transform data to the principal components.
# “ica“: transform data to the independent components.
# “spatialSign“: project data onto a unit circle.

library(caret)

#### 1. Scale ####

# calculate the pre-process parameters from the dataset
prep.scale <- preProcess(dt, method = c("scale"))

print(prep.scale)

# transform the dataset using the parameters
trans.scale <- predict(prep.scale, dt)

summary(trans.scale)





#### 2. Center ####

# calculate the pre-process parameters from the dataset
prep.center <- preProcess(dt, method = c("center"))

print(prep.center)

#transform the dataset using the parameters
trans.center <- predict(prep.center, dt)

summary(trans.center)

#### 3. Standardize ####

# calculate the pre-process parameters from the dataset
prep.stand <- preProcess(dt, method = c("center", "scale"))

print(prep.stand)

#transform the dataset using the parameters
trans.stand <- predict(prep.stand, dt)

summary(trans.stand)

#### 4. Normalize ####

# calculate the pre-process parameters from the dataset
prep.normal <- preProcess(dt, method = c("range"))

print(prep.normal)

#transform the dataset using the parameters
trans.normal <- predict(prep.normal, dt)

trans.normal$DeliveryLeadTime <- as.factor(trans.normal$DeliveryLeadTime)

trans.normal <- trans.normal[complete.cases(trans.normal),]
summary(trans.normal)

#### 5. Box-Cox Transform
library(mlbench)

# calculate the pre-process parameters from the dataset
prep.boxcox <- preProcess(dt, method = c("BoxCox"))

print(prep.boxcox)

#transform the dataset using the parameters
trans.boxcox <- predict(prep.boxcox, dt)

summary(trans.boxcox)

#### 6. Yeo-Johnson Transform ####

# calculate the pre-process parameters from the dataset
prep.yeoj <- preProcess(dt, method = c("YeoJohnson"))

print(prep.yeoj)

#transform the dataset using the parameters
trans.yeoj <- predict(prep.yeoj, dt)

summary(trans.yeoj)


#### 7. Principal Component Analysis ####

# calculate the pre-process parameters from the dataset
prep.pca <- preProcess(dt, method = c("center","scale","pca"))

print(prep.pca)

#transform the dataset using the parameters
trans.pca <- predict(prep.pca, dt)

summary(trans.center)

#### 8. Independent Component Analysis ####
library(fastICA)

# calculate the pre-process parameters from the dataset
prep.ica <- preProcess(dt, method = c("center","scale","ica"), n.comp=5)

print(prep.ica)

#transform the dataset using the parameters
trans.ica <- predict(prep.ica, dt)

summary(trans.ica)









#### Applied Machine Learning ####

# using randomforest
library(randomForest)
# trans.normal$OrderID <- as.factor(trans.normal$OrderID)
# trans.normal$RestaurantID <- as.factor(trans.normal$RestaurantID)
# trans.normal$RestaurantName <- as.factor(trans.normal$RestaurantName)
# rf.train.1 <- select(dt, c(1:2))
# 
# rf.label <- as.factor(train$dlt)
# 
# set.seed(1234)
# rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)

#### 4. Normalize ####


