data <- read.csv(file.choose(),header=T)

install.packages("olsrr")
library(dummies)            # dummy variables
library(tidyverse)          # pipe oerator and plots
library(car)                # VIF
library(caret)              # splitting
library(MASS)               # boxcox
library(broom)              # 
library(leaps)              # 
library(olsrr)
library(lmtest)

# MISSING VALUE IMPUTATION
names(data)

# table(is.na(data$VAR7))

# table(data$VAR4)

data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = TRUE)
data$Time_of_service[is.na(data$Time_of_service)] <- mean(data$Time_of_service, na.rm = TRUE)

data$Pay_Scale[is.na(data$Pay_Scale)] <- 5
data$Work_Life_balance[is.na(data$Work_Life_balance)] <- 3
data$VAR2[is.na(data$VAR2)] <- -0.1048
data$VAR4[is.na(data$VAR4)] <- 2

#write.csv(data, "datawithoutdummycomp.csv",row.names =F)

# table(is.na(data))

data1 <- dummy.data.frame(data, names=c("Gender","Relationship_Status","Hometown","Unit",
                            "Decision_skill_possess","Compensation_and_Benefits"), sep="_")

# write.csv(data1, "datawithdummycomp.csv",row.names =F)

# Splitting data via caret and tidyverse
set.seed(123)
training.samples <- data1$Attrition_rate %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data1[training.samples, ]
test.data <- data1[-training.samples, ]

# write.csv(test.data, "test_data_with_dummy_comp.csv",row.names =F)

# Removing extra dummies
train.data <- train.data[ , c(-1,-3,-7,-12,-24,-28,-39)]
test.data <- test.data[ , c(-1,-3,-7,-12,-24,-28,-39)]

#write.csv(train.data, "train_data_with_dummy_comp.csv",row.names =F)

###################################################################################################################
# Load train and test data from diectory
# train.data <- read.csv("train_data_with_dummy_comp.csv" , header = T)
 test.data <- read.csv("test_data_with_dummy_comp.csv" , header = T)


# Linear regression with all variables
model <- lm(Attrition_rate ~ . +0  , data = train.data)
summary(model)

# Making predictions
pred <- model %>% predict(test.data)
data.frame( R2 = R2(pred, test.data$Attrition_rate),
            RMSE = RMSE(pred, test.data$Attrition_rate),
            MAE = MAE(pred, test.data$Attrition_rate)) 


# Apply boxcox transformation
# train.data <- train.data[c(-2728,-4146), ]     # removing 0 from target
BC = boxcox(Attrition_rate ~ . , data = train.data)
best.lm = BC$x[which(BC$y==max(BC$y))]
hist(train.data$Attrition_rate^0.26)

# Transforming target in train data
train.data$Attrition_rate <- train.data$Attrition_rate^0.26

# Again Linear regression with all variables
model <- lm(Attrition_rate ~ . +0  , data = train.data)
summary(model)

# Making predictions
pred <- model %>% predict(test.data)
pred <- pred^(1/0.26)
data.frame( R2 = R2(pred, test.data$Attrition_rate),
            RMSE = RMSE(pred, test.data$Attrition_rate),
            MAE = MAE(pred, test.data$Attrition_rate)) 

par(mfrow = c(2,2))
plot(model,4)
#ols_test_score(model)
#ad.test(model$residuals)
hist(model$residuals)

model <- lm(Attrition_rate ~ . -Gender_F -Hometown_Springfield - Hometown_Franklin -Education_Level- Hometown_Lebanon- Decision_skill_possess_Analytical - VAR3 - VAR2  - Travel_Rate - Decision_skill_possess_Conceptual -Relationship_Status_Married -Hometown_Clinton -Decision_skill_possess_Behavioral -VAR1 +0  , data = train.data)
summary(model)

plot(model)

##########################################################################################################

test <- read.csv(file.choose(), header = T)
test_data1 <- dummy.data.frame(test, names=c("Gender","Relationship_Status","Hometown","Unit",
                                         "Decision_skill_possess","Compensation_and_Benefits"), sep="_")

test23 <- test_data1[ , c(-1,-3,-7,-12,-24,-28,-39)]

write.csv(test23, "test submit.csv",row.names =F)
test23 <- read.csv(file.choose(),header = T)

pred <- model %>% predict(test23)
pred <- pred^(1/0.26)
data.frame( R2 = R2(pred, test.data$Attrition_rate),
            RMSE = RMSE(pred, test.data$Attrition_rate),
            MAE = MAE(pred, test.data$Attrition_rate)) 

ad <- data.frame(test_data1[1] , pred)

write.csv(ad, "test submit_1.csv",row.names =F)


##########################
test$Age[is.na(test$Age)] <- mean(data$Age, na.rm = TRUE)
test$Time_of_service[is.na(test$Time_of_service)] <- mean(data$Time_of_service, na.rm = TRUE)

test$Pay_Scale[is.na(test$Pay_Scale)] <- 5
test$Work_Life_balance[is.na(test$Work_Life_balance)] <- 3
test$VAR2[is.na(test$VAR2)] <- -0.1048
test$VAR4[is.na(test$VAR4)] <- 2
##########################################################################################################




