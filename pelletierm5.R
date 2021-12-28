library(readr)
library(dplyr)
library(zoo)
library(psych)
library(ROCR)
library(corrplot)
library(car)
library(InformationValue)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(corrplot)
library(glm2)
library(aod)
library(caret)
library(kernlab)

# Data Import and Variable Type Changes
data <- read.csv("C:/Users/18046/Downloads/MachineLearning1/buad5122-m3-insurance-training.csv")

# Changing data types to categorical and numeric (training)  
data$INDEX <- as.factor(data$INDEX)
data$TARGET_FLAG <- as.factor(data$TARGET_FLAG)
data$SEX <- as.factor(data$SEX)
data$EDUCATION <- as.factor(data$EDUCATION)
data$PARENT1 <- as.factor(data$PARENT1)
data$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$INCOME)))

data$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$HOME_VAL)))
data$MSTATUS <- as.factor(data$MSTATUS)
data$REVOKED <- as.factor(data$REVOKED)
data$RED_CAR <- as.factor(ifelse(data$RED_CAR=="yes", 1, 0))
data$URBANICITY <- ifelse(data$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
data$URBANICITY <- as.factor(data$URBANICITY)
data$JOB <- as.factor(data$JOB)
data$CAR_USE <- as.factor(data$CAR_USE)
data$CAR_TYPE <- as.factor(data$CAR_TYPE)
data$DO_KIDS_DRIVE <- as.factor(ifelse(data$KIDSDRIV > 0, 1, 0 ))
data$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$HOME_VAL)))
data$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$BLUEBOOK)))

# Fix NA's in training
data$AGE[is.na(data$AGE)] <- mean(data$AGE, na.rm = "TRUE")
data$YOJ <- na.aggregate(data$YOJ, data$JOB, mean, na.rm = TRUE)
data$INCOME <- na.aggregate(data$INCOME, data$JOB, mean, na.rm = TRUE)
data$HOME_VAL <- na.aggregate(data$HOME_VAL, data$JOB, mean, na.rm = TRUE )
data$CAR_AGE <- na.aggregate(data$CAR_AGE, data$CAR_TYPE, mean, na.rm = TRUE)
data$CAR_AGE[data$CAR_AGE < 0 ] <- 0 #change negative ages to zero
data$OLDCLAIM <- ifelse(data$CAR_AGE < 5 & !is.na(data$CAR_AGE),0,data$OLDCLAIM)
data$OLDCLAIM <- na.aggregate(data$OLDCLAIM, data$CAR_AGE, mean, na.rm = TRUE )
data$HOME_OWNER <- ifelse(data$HOME_VAL == 0, 0, 1)
data$SQRT_TRAVTIME <- sqrt(data$TRAVTIME)
data$SQRT_BLUEBOOK <- sqrt(data$BLUEBOOK)

# Bin Income (training)
data$INCOME_bin[data$INCOME == 0] <- "Zero"
data$INCOME_bin[data$INCOME >= 1 & data$INCOME < 30000] <- "Low"
data$INCOME_bin[data$INCOME >= 30000 & data$INCOME < 80000] <- "Medium"
data$INCOME_bin[data$INCOME >= 80000] <- "High"
data$INCOME_bin <- factor(data$INCOME_bin)
data$INCOME_bin <- factor(data$INCOME_bin, levels=c("Zero","Low","Medium","High"))

# Bin Car Age 
data$CAR_AGE_bin[data$CAR_AGE <= 3] <- "New"
data$CAR_AGE_bin[data$CAR_AGE >= 4 & data$CAR_AGE < 8] <- "Medium"
data$CAR_AGE_bin[data$CAR_AGE >= 9] <- "Old"
data$CAR_AGE_bin <- factor(data$CAR_AGE_bin)
data$CAR_AGE_bin <- factor(data$CAR_AGE_bin, levels=c("New","Medium","Old"))
summary(data)

#Studies show highest rate of drinking and driving occurs within ages 21-34, lets make a new dimension to our model
data$DUI <- as.factor(ifelse(data$AGE >= 21 & data$AGE < 34, 1, 0))
data$DUI <- as.factor(data$DUI)
#Looking at our data
# Histograms for Numeric Variables, looking for outliers
par(mfrow=c(2,2))
hist(data$AGE, col = "red", xlab = "Age", main = "AGE Hist")
data0<- subset(data, TARGET_FLAG == 1 )
bp_Age = boxplot(data$AGE, col = "red", main = "AGE BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(data$TRAVTIME, col = "green", xlab = "TRAVTIME", main = "TRAVTIME Hist")
hist(data$YOJ, col = "blue", xlab = "YOJ", main = "YOJ Hist")
bp_Trav = boxplot(data$TRAVTIME, col = "green", main = "TRAVTIME BoxPlot")
bp_Yoj = boxplot(data$YOJ, col = "blue", main = "YOJ BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(data$BLUEBOOK, col = "green", xlab = "BLUEBOOK", main = "BLUEBOOK Hist")
hist((data$TIF), col = "blue", xlab = "TIF", main = "TIF Hist")
bp_blue = boxplot(data$BLUEBOOK, col = "green", main = "BLUEBOOK BoxPlot")
bp_tif = boxplot(data$TIF, col = "blue", main = "TIF BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(data$MVR_PTS, col = "red", xlab = "MVR_PTS", main = "MVR_PTS Hist")
hist(data$CAR_AGE, col = "blue", xlab = "CAR_AGE", main = "CAR_AGE Hist")
bp_mvr = boxplot(data$MVR_PTS, col = "red", main = "MVR_PTS BoxPlot")
bp_Carage = boxplot(data$CAR_AGE, col = "blue", xlab = "CAR_AGE", main = "CAR_AGE BoxPlot")
par(mfrow=c(1,1))

#Setting outlier boundaries for numeric data
ubAge = bp_Age$stats[5,]
lbAge = bp_Age$stats[1,]

data$AGE.Flag <- as.factor(ifelse(data$AGE  < lbAge, 0, 1)) 
data$AGE <- as.numeric(ifelse(data$AGE.Flag == 0, lbAge, data$AGE ))

data$AGE.max <- as.factor(ifelse(data$AGE > ubAge, 0, 1))
data$AGE <- as.numeric(ifelse(data$AGE.max == 0, ubAge, data$AGE))

ubTrav = bp_Trav$stats[5,]
lbTrav = bp_Trav$stats[1,]

data$TRAVTIME.Flag <- as.factor(ifelse(data$TRAVTIME  < lbTrav, 0, 1)) 
data$TRAVTIME <- as.numeric(ifelse(data$TRAVTIME.Flag == 0, lbTrav, data$TRAVTIME ))

data$TRAVTIME.max <- as.factor(ifelse(data$TRAVTIME > ubTrav, 0, 1))
data$TRAVTIME <- as.numeric(ifelse(data$TRAVTIME.max == 0, ubTrav, data$TRAVTIME))

ubYoj = bp_Yoj$stats[5,]
lbYoj = bp_Yoj$stats[1,]

data$YOJ.Flag <- as.factor(ifelse(data$YOJ  < lbYoj, 0, 1)) 
data$YOJ <- as.numeric(ifelse(data$YOJ.Flag == 0, lbYoj, data$YOJ))

data$YOJ.max <- as.factor(ifelse(data$YOJ > ubYoj, 0, 1))
data$YOJ <- as.numeric(ifelse(data$YOJ.max == 0, ubYoj, data$YOJ))

ubBlue = bp_blue$stats[5,]
lbBlue = bp_blue$stats[1,]

data$BLUEBOOK.Flag <- as.factor(ifelse(data$BLUEBOOK  < lbBlue, 0, 1)) 
data$BLUEBOOK <- as.numeric(ifelse(data$BLUEBOOK == 0, lbBlue, data$BLUEBOOK))

data$BLUEBOOK.max <- as.factor(ifelse(data$BLUEBOOK > ubBlue, 0, 1))
data$BLUEBOOK <- as.numeric(ifelse(data$BLUEBOOK.max == 0, ubBlue, data$BLUEBOOK))

ubTif = bp_tif$stats[5,]
lbTif = bp_tif$stats[1,]

data$TIF.Flag <- as.factor(ifelse(data$TIF  < lbTif, 0, 1)) 
data$TIF <- as.numeric(ifelse(data$TIF == 0, lbTif, data$TIF))

data$TIF.max <- as.factor(ifelse(data$TIF > ubTif, 0, 1))
data$TIF <- as.numeric(ifelse(data$TIF.max == 0, ubTif, data$TIF))

ubMvr = bp_mvr$stats[5,]
lbMvr = bp_mvr$stats[1,]

data$MVR_PTS.Flag <- as.factor(ifelse(data$MVR_PTS  < lbMvr, 0, 1)) 
data$MVR_PTS <- as.numeric(ifelse(data$MVR_PTS == 0, lbMvr, data$MVR_PTS))

data$MVR_PTS.max <- as.factor(ifelse(data$MVR_PTS > ubMvr, 0, 1))
data$MVR_PTS <- as.numeric(ifelse(data$MVR_PTS.max == 0, ubMvr, data$MVR_PTS))

ubCar = bp_Carage$stats[5,]
lbCar = bp_Carage$stats[1,]

data$CAR_AGE.Flag <- as.factor(ifelse(data$CAR_AGE  < lbCar, 0, 1)) 
data$CAR_AGE <- as.numeric(ifelse(data$CAR_AGE == 0, lbCar, data$CAR_AGE))

data$CAR_AGE.max <- as.factor(ifelse(data$CAR_AGE > ubCar, 0, 1))
data$CAR_AGE <- as.numeric(ifelse(data$CAR_AGE.max == 0, ubCar, data$CAR_AGE))

#seems like there are barely any outliers flagged, so I see little need to change them for test data

############# Part 3: Model Development ######################
#Model Development for TARGET_FLAG
set.seed(100)
index <- createDataPartition(data$TARGET_FLAG, p=.8, list = FALSE, times =1)
train_set = data[index,]
test_set = data[-index,]

#Let's drop our columns with less than 2 factors (Flag columns)
train_set[,c(32:50)] <- NULL
test_set[,c(32:50)] <- NULL

#Training model... took way too long to run.
#model.train <- train(TARGET_FLAG ~ ., 
                     #data = train_set, 
                     #method = "svmPoly", 
                     #na.action = na.omit, 
                     #preProcess = c("scale","center"), 
                     #trControl = trainControl(method = "none"), 
                     #tuneGrid = data.frame(degree=10,scale=10,C=10))

#cv.train <- train(TARGET_FLAG ~ ., 
                  #data = train_set, 
                  #method = "svmPoly", 
                  #na.action = na.omit, 
                  #preProcess = c("scale","center"), 
                  #trControl = trainControl(method="cv", number=10), 
                  #tuneGrid = data.frame(degree=1,scale=1,C=1))


#Going with Step-wise
Model1 <- glm(TARGET_FLAG ~ AGE + BLUEBOOK + TRAVTIME + KIDSDRIV + SEX + URBANICITY + HOMEKIDS + 
                CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + TIF + EDUCATION + MSTATUS + PARENT1 + RED_CAR + 
                CAR_USE + CAR_TYPE + YOJ + JOB + INCOME_bin + HOME_VAL, 
              data = data, family = binomial())
summary(Model1)

Model2 <- glm(TARGET_FLAG ~ AGE + BLUEBOOK + TRAVTIME + KIDSDRIV + SEX +  URBANICITY +
                CLM_FREQ + REVOKED + MVR_PTS + TIF + EDUCATION + MSTATUS + PARENT1 + CAR_USE + CAR_TYPE + YOJ + JOB + 
                HOME_VAL,
              data = data, family = binomial())
summary(Model2)

Model3 <- glm(TARGET_FLAG ~ AGE + SQRT_TRAVTIME + SQRT_BLUEBOOK + DO_KIDS_DRIVE + URBANICITY +
                CLM_FREQ + REVOKED + MVR_PTS + TIF + EDUCATION + MSTATUS + PARENT1 + CAR_USE + CAR_TYPE + JOB + 
                HOME_OWNER,
              data = data, family = binomial())
summary(Model3)

Model4 <- glm(TARGET_FLAG ~ TRAVTIME + KIDSDRIV + URBANICITY + 
                CLM_FREQ + REVOKED + MVR_PTS + TIF + MSTATUS + PARENT1 + 
                CAR_USE + CAR_TYPE + JOB + INCOME_bin + HOME_VAL + DUI,
              data = data, family = binomial())
summary(Model4)

#Comparing classification results for each model
data$predict1 <- predict(Model1, type = "response")

data$Claim1 <- ifelse(data$predict1 > 0.5,1,0)
data$Claim1 <- as.factor(data$Claim1)
confusionMatrix(data$Claim1, data$TARGET_FLAG)

data$predict2 <- predict(Model2, type = "response")

data$Claim2 <- ifelse(data$predict2 > 0.5,1,0)
data$Claim2 <- as.factor(data$Claim2)
confusionMatrix(data$Claim2, data$TARGET_FLAG)

data$predict3 <- predict(Model3, type = "response")

data$Claim3 <- ifelse(data$predict3 > 0.5,1,0)
data$Claim3 <- as.factor(data$Claim3)
confusionMatrix(data$Claim3, data$TARGET_FLAG)

data$predict4 <- predict(Model4, type = "response")

data$Claim4 <- ifelse(data$predict4 > 0.5,1,0)
data$Claim4 <- as.factor(data$Claim4)
confusionMatrix(data$Claim4, data$TARGET_FLAG)

##Model 2 has best accuracy (79.12%)
#let's see importance
importance <- varImp(Model2)
importance

#Importing test data
test <- read.csv("C:/Users/18046/Downloads/MachineLearning1/buad5122-m3-insurance-test.csv")

#Change data types to numerical and categorical (test)
test$INDEX <- as.factor(test$INDEX)
test$TARGET_FLAG <- as.factor(test$TARGET_FLAG)
test$SEX <- as.factor(test$SEX)
test$EDUCATION <- as.factor(test$EDUCATION)
test$PARENT1 <- as.factor(test$PARENT1)
test$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$INCOME)))
test$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$HOME_VAL)))
test$MSTATUS <- as.factor(test$MSTATUS)
test$REVOKED <- as.factor(test$REVOKED)
test$RED_CAR <- as.factor(ifelse(test$RED_CAR=="yes", 1, 0))
test$URBANICITY <- ifelse(test$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
test$URBANICITY <- as.factor(test$URBANICITY)
test$JOB <- as.factor(test$JOB)
test$CAR_USE <- as.factor(test$CAR_USE)
test$CAR_TYPE <- as.factor(test$CAR_TYPE)
test$DO_KIDS_DRIVE <- as.factor(ifelse(test$KIDSDRIV > 0, 1, 0 ))
test$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$HOME_VAL)))
test$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$BLUEBOOK)))

#Fix NA's for training
test$AGE[is.na(test$AGE)] <- mean(test$AGE, na.rm = "TRUE")
test$YOJ <- na.aggregate(test$YOJ, test$JOB, mean, na.rm = TRUE)
test$INCOME <- na.aggregate(test$INCOME, test$JOB, mean, na.rm = TRUE)
test$HOME_VAL <- na.aggregate(test$HOME_VAL, test$JOB, mean, na.rm = TRUE )
test$CAR_AGE <- na.aggregate(test$CAR_AGE, test$CAR_TYPE, mean, na.rm = TRUE)
test$CAR_AGE[test$CAR_AGE < 0 ] <- 0 
test$OLDCLAIM <- ifelse(test$CAR_AGE < 5 & !is.na(test$CAR_AGE),0,test$OLDCLAIM)
test$OLDCLAIM <- na.aggregate(test$OLDCLAIM, test$CAR_AGE, mean, na.rm = TRUE )
test$HOME_OWNER <- ifelse(test$HOME_VAL == 0, 0, 1)
test$SQRT_TRAVTIME <- sqrt(test$TRAVTIME)
test$SQRT_BLUEBOOK <- sqrt(test$BLUEBOOK)

# Bin Income (test)
test$INCOME_bin[test$INCOME == 0] <- "Zero"
test$INCOME_bin[test$INCOME >= 1 & test$INCOME < 30000] <- "Low"
test$INCOME_bin[test$INCOME >= 30000 & test$INCOME < 80000] <- "Medium"
test$INCOME_bin[test$INCOME >= 80000] <- "High"
test$INCOME_bin <- factor(test$INCOME_bin)
test$INCOME_bin <- factor(test$INCOME_bin, levels=c("NA","Zero","Low","Medium","High"))

# Bin Car Age (test)
test$CAR_AGE_bin[test$CAR_AGE <= 3] <- "New"
test$CAR_AGE_bin[test$CAR_AGE >= 4 & test$CAR_AGE < 8] <- "Medium"
test$CAR_AGE_bin[test$CAR_AGE >= 9] <- "Old"
test$CAR_AGE_bin <- factor(test$CAR_AGE_bin)
test$CAR_AGE_bin <- factor(test$CAR_AGE_bin, levels=c("New","Medium","Old"))

summary(test)

#use model on test data
test$prediction <- predict(Model2, newdata = test, type = "response")
test$claim <- ifelse(test$prediction > 0.5,1,0)
test$claim <- as.factor(test$claim)
results <- test[c("INDEX","claim")]

#write file
write.csv(results, file = "C:/Users/18046/Downloads/MachineLearning1/M5results.csv")


