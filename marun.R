# ========================================================================================================
# Purpose:      Rscript for Computer Based Assignment CBA
# Author:       Anthony Koh Hong Ji
# Updated:      29-10-2023
# Topics:       Data Exploration; Data Cleaning; Linear Regression; Train vs Test set; CART
# Data Source:  marun_sample2.csv
# Packages:     data.Table, corrplot, caTools, rpart
#=========================================================================================================
# LIBRARY ------------------------------------------------------------------------------------------------
library(data.table)
library(corrplot)
library(caTools)
library(rpart)
library(rpart.plot)

# IMPORTING OF DATASET -----------------------------------------------------------------------------------
setwd("C:/Users/Anthony/Documents/NTU/Y2S1/BC2406 Course Materials/AY23 BC2406 CBA")
marun <- fread("marun_sample2.csv")

# DATA EXPLORATION AND CLEANING (I) ----------------------------------------------------------------------
# Finding datatype for each variable
sapply(marun, class)

# Changing formation type from numeric to factor
marun$Formation <- factor(marun$Formation) 

summary(marun)

# Finding NA's - Creating a dataframe to store missing value information
missing_data <- data.frame(
  FAN600 = which(is.na(marun$FAN600)),
  FAN300 = which(is.na(marun$FAN300)),
  MIN10GEL = which(is.na(marun$MIN10GEL)),
  MUDLOSSU = which(is.na(marun$MUDLOSSU))
)
print(missing_data)

# Regression imputation to predict missing MUDLOSSU value
no_na <- marun[!is.na(marun$MUDLOSSU), ]
with_na <- marun[is.na(marun$MUDLOSSU), ]
predict_na_model <- lm(MUDLOSSU ~ ., data = no_na)
predicted_mudlossu <- predict(predict_na_model, newdata = with_na)
marun$MUDLOSSU[which(is.na(marun$MUDLOSSU))] <- predicted_mudlossu

# Omitting row with 3 missing variables
marun <- na.omit(marun)

# DATA EXPLORATION AND CLEANING (II) ---------------------------------------------------------------------
# Boxplot
par(mfrow = c(3, 3))  # This sets up a 2x2 grid, adjust as needed
# Create a boxplot for each variable
for (variable in names(marun)) {
  boxplot(marun[[variable]], main = variable, xlab = variable)
}
par(mfrow = c(1, 1))

# Removing outliers from METERAGE and RPM
marun <- marun[-which(marun$METERAGE == 650 | marun$RPM == 394 | marun$RPM == 375.5),]

# Histogram
h <- hist(marun$MUDLOSSU, main = "Histogram of Severity of Lost Circulation", ylim = c(0, 2000), breaks=10)
text(h$mids, h$counts, labels=h$counts, adj=c(0.5, -0.5))

# DATA EXPLORATION (III) ---------------------------------------------------------------------------------
# Correlation Matrix
c <- round(cor(marun[, .SD, .SDcols = names(marun)[sapply(marun, is.numeric)]]),2)
corrplot(c, type="upper")

# DATA CLEANING ------------------------------------------------------------------------------------------
# Regression imputation to predict missing MUDLOSSU value
no_na <- marun[!is.na(marun$MUDLOSSU), ]
with_na <- marun[is.na(marun$MUDLOSSU), ]
predict_na_model <- lm(MUDLOSSU ~ ., data = no_na)
predicted_mudlossu <- predict(model, newdata = with_na)
marun$MUDLOSSU[which(is.na(marun$MUDLOSSU))] <- predicted_mudlossu

# Omitting row with 3 missing variables
marun <- na.omit(marun)

# TRAIN-TEST SPLIT  --------------------------------------------------------------------------------------
set.seed(10)
train <- sample.split(Y = marun$MUDLOSSU, SplitRatio = 0.7)
trainset <- subset(marun, train == T)
testset <- subset(marun, train == F)

# LINEAR REGRESSION --------------------------------------------------------------------------------------
m1 <- lm(MUDLOSSU ~ . -Northing -Easting, data = trainset)
summary(m1)
m2 <- lm(MUDLOSSU ~ . -Northing -Easting -`Mud pressure (psi)` -DRLTIME -WOB -`Pump pressure` -RPM, data = trainset)
summary(m2)
residuals(m2)

# RMSE based on Trainset
RMSE.m2.train <- sqrt(mean(residuals(m2)^2))

# Apply model from trainset to predict on testset
predict.m2.test <- predict(m2, newdata = testset)
testset.error <- testset$MUDLOSSU - predict.m2.test

# Testset Errors
RMSE.m2.test <- sqrt(mean(testset.error^2))

# RMSE Results
RMSE.m2.train 
RMSE.m2.test

# CART ---------------------------------------------------------------------------------------------------
cart1 <- rpart(MUDLOSSU ~ ., data = trainset, method = 'anova',
               control = rpart.control(minsplit = 2, cp = 0))

# Plots the maximal tree and results
rpart.plot(cart1, nn= T, main = "Maximal Tree in Trainset")
printcp(cart1)
plotcp(cart1)
print(cart1)

# Compute min CVerror + 1SE in maximal tree cart1
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + 
  cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]
CVerror.cap

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
cp.opt

# Prune the maximal tree using the optimal cp value
cart2 <- prune(cart1, cp = cp.opt)
printcp(cart2, digits = 3)
# Trainset Error
# Root node error: 50526804/1882 = 26847
# cart2 trainset MSE = 0.556 * 26847 = 14,926.932

plotcp(cart2)
print(cart2)

# Printing optimal tree
rpart.plot(cart2, nn = T, main = "Optimal Tree")

# Variable Importance
cart2$variable.importance
scaledVarImpt <- round(100*cart2$variable.importance/sum(cart2$variable.importance))
scaledVarImpt[scaledVarImpt > 0] 

# Trainset RMSE
RMSE.cart2.train <- sqrt(14926.932)

# Predicting testset
cart3 <- predict(cart2, newdata = testset)
RMSE.cart2.test <- sqrt(mean((testset$MUDLOSSU - cart3)^2))

# CART RMSE Results
RMSE.cart2.train
RMSE.cart2.test


