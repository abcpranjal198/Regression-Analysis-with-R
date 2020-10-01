
# Loading dataset
data = read.csv(file.choose(),header = T)


# Randomly Split the data into training and test set
library(tidyverse)
# install.packages("caret")
library(caret)      # training and testing split
set.seed(123)
training.samples <- data$PE %>%
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]



# Take intro of your dataset

dim(train.data)                    #  =>      6699 rows and 5 columns
colnames(train.data)               #  =>      Predictors : "AT" "V"  "AP" "RH"  ;   Target is "PE"
summary(train.data)
str(train.data)                          # => All numeric


# Range is different for different features, hence need for normalization (min max scaling)
for (i in 1:ncol(data)){ 
  
                        train.data[ , i] <- (train.data[ , i] - min(train.data[i] , na.rm = T))/
                                            (max(train.data[i] , na.rm = T) - min(train.data[i] , na.rm = T))
}



# Checking and removing outliers
boxplot(train.data)

D3 <- train.data[-which(train.data$AP %in% boxplot.stats(train.data$AP)$out), ]
boxplot(D3)
D4 <- D3[-which(D3$RH %in% boxplot.stats(D3$RH)$out), ]
boxplot(D4)

# renaming D4
of_data <- D4

# Checking missing value
table(is.na(of_data))          # no missing value



# Further split the of_data into train and validation data
set.seed(123)
training.samples <- of_data$PE %>%
  createDataPartition(p = 0.7, list = FALSE)
traindata  <- of_data[training.samples, ]
valid.data <- of_data[-training.samples, ]



plot(of_data$AT , of_data$PE)
abline(lm(PE ~ AT, data = of_data), col = "blue")

plot(of_data$V , of_data$PE)
abline(lm(PE ~ V, data = of_data), col = "blue")

plot(of_data$AP , of_data$PE)
abline(lm(PE ~ AP, data = of_data), col = "blue")

plot(of_data$RH , of_data$PE)
abline(lm(PE ~ RH, data = of_data), col = "blue")


# Creating multiple linear regression model.

model = lm(PE ~ . , data = traindata)
summary(model)


# Model adequacy cheking
par(mfrow = c(2,2))
plot(model)

# detecting multicollinearity
install.packages("car")
library(car)
car::vif(model)                                      #   multicollinearity
cor(traindata)
plot(traindata$V,traindata$AT)



hist(model$residuals)                                #   errors are not normally distributed
# checking normality assumptions
library(nortest)
ad.test(model$residuals)                             #   voilated
# shapiro.test(model$residuals)                      #   voilated


# library(tseries)
# jarque.bera.test(model)
# library(normtest)
# ajb.norm.test(traindata)


# Checking homoscedasticity
library(lmtest)
bptest(model)
library(olsrr)
ols_test_f(model)                                    #    Variance is not homogenous

# checking independency of errors
durbinWatsonTest(model)                              #    errors are independent (Errors are uncorrelated)

# Checking mean of errors
mean(model$residuals)                                #    mean zero


# influential point detection
# Diagnostic metrics
library(broom)
dm <- augment(model)                                 #    No influential observation


# Making predictions
pred <- model %>% predict(valid.data)
data.frame( R2 = R2(pred, valid.data$PE),
            RMSE = RMSE(pred, valid.data$PE),
            MAE = MAE(pred, valid.data$PE))                     # R2         RMSE       MAE
                                                                # 0.9436412  1.386676   1.074112

x = as.matrix(traindata[-5])
class(x)
y = t(x)
class(y)
z= t(x) %*% x
b = eigen(z)
a = b$values
class(a)
c = max(a)/min(a)
c                                                    #    166.6071 danger mc


#########################################################################################################################
# install.packages("BBmisc")
# library(BBmisc)
# data_norm <- normalize(data, method = "normalize", range = c(0, 1))


model1 = lm(log(PE+1) ~ .  , data = traindata )
summary(model1)
plot(model1)

hist(data$AT)
hist(data$V)
hist(data$AP)
hist(data$RH)
hist(log(data$PE))

####################################################################################################
install.packages("randomForest")
library(randomForest)
rf <- randomForest(PE ~ .  , data = traindata)

print(rf)
rf$importance
plot(rf)

pred_valid = predict(rf, newdata=valid.data[-5] )
data.frame( R2 = R2(pred_valid, valid.data$PE),
            RMSE = RMSE(pred_valid, valid.data$PE),
            MAE = MAE(pred_valid, valid.data$PE))









