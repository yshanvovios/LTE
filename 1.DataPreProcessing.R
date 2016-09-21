#### Set WD, load Data and packages ####

# set WorkingDirectory
setwd("~/Documents/[1]Rstudio/INTELOP/LTE")

# Load package
library(data.table)

# Load Data
res <- fread("order_restaurant.csv", sep="auto", header = TRUE, verbose = TRUE, na.strings = c("","NA"))

# remove variables
library(dplyr)

res <- select(res, -c(4:5,11,24:26,29:38,40,45:50))

# impute missing value in DeliveryLeadTime
res$DeliveryLeadTime[is.na(res$DeliveryLeadTime)] <- median(res$DeliveryLeadTime, na.rm = TRUE)

#impute 0 with mean value in DLT
res$DeliveryLeadTime <- ifelse(res$DeliveryLeadTime == 0, median(res$DeliveryLeadTime), res$DeliveryLeadTime)

summary(res$DeliveryLeadTime)



# remove outlier in DLT
(dt <- res[with(res, DeliveryLeadTime < 100), ])

summary(dt$DeliveryLeadTime)

#

