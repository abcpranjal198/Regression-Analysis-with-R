
getwd()
setwd("D:\\Regression Model")

## Loading train data directly
train.data <- read.csv("123.csv",header=T)

attach(train.data)

par(mfrow = c(2,2))


############################# Simple Linear Regression ##############################

plot(youtube , sales)
cor(youtube , sales)               # r = 0.7877721
cor.test(youtube , sales)          # significant correlation  

# Build model
model_1 <- lm(sales ~ youtube , data = train.data)

# Summarize Model
summary(model_1)                   #  MSE = 3.943,  ADJ R2 = 0.6182,  All coefficients and over all model is significant.

# Diagnostic metrics
library(broom)
dm_1 <- augment(model_1)

# Diagnostic plots
plot(model_1)                      # Shows hetrosadsticity
plot(model_1 , 4)

durbinWatsonTest(model_1)


# Weighted Simple linear regression (shape of megaphone in residual vs fitted plot and absence of horizontal line in scale location plot)
sd.func <- lm(abs(model_1$residuals) ~ youtube)
summary(sd.func)

weigts <- 1/((sd.func$fitted.values)^2)

wlsfit <- lm(sales ~ youtube , weights = weigts)
summary(wlsfit)                    #  MSE = 1.268,  ADJ R2 = 0.7345,  All coefficients and over all model is significant.

# Diagnostic metrics
wdm_1 <- augment(wlsfit)

# Diagnostic plots
plot(wlsfit)
plot(wlsfit , 4)


detach(train.data)

# Influential point detection
## First removal
ro105_data <- subset(wdm_1 , select = c(1,2,3) , .cooksd < 0.5 ) 
sales <- ro105_data$sales
youtube <-  ro105_data$youtube

wls_re_fit <- lm(sales ~ youtube , weights = ro105_data$X.weights.)
summary(wls_re_fit)                 #  MSE = 1.202,  ADJ R2 = 0.7376,  All coefficients and over all model is significant.

# Diagnostic metrics
wdm_re_1 <- augment(wls_re_fit)

# Diagnostic plots
par(mfrow=c(2,2))
plot(wls_re_fit)
plot(wls_re_fit , 4)

#----------------------------------------------------- Visualization of models ------------------------------------------------------------------------------
library(tidyverse)
## see the ols regression line with residual error
ggplot(dm_1, aes(youtube, sales)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = youtube, yend = .fitted), color = "red", size = 0.3) +
  labs(title = "Scatterplot of sales ~ youtube")
#-----------------------------------------------------------------------------------------------

### plot of ols and weighted regression line
#library(dplyr)
#ggplot(dm_1, aes(youtube, sales)) +
#  geom_point() +
#  geom_smooth(method = lm, se = FALSE,
#              color = "black", 
#              size = 0.8, 
#              linetype = "dashed") +
#  geom_smooth(method = lm, se = FALSE, 
#              aes(weight = weigts),
#              color = "red", 
#              size = 0.8,
#              linetype = "dashed") +
#  labs(title = "Scatterplot of sales ~ youtube")
#------------------------------------------------------------------------------------------------
### plot of ols regression line
ggplot(dm_1, aes(youtube , sales)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, 
              
              color = "blue", 
              size = 0.8,
              linetype = "dashed") +
  labs(title = "Scatterplot of sales ~ youtube")


###### plot of weighted regression line

ggplot(wdm_1, aes(youtube , sales)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, 
              aes(weight = X.weights.),
              color = "blue", 
              size = 0.8,
              linetype = "dashed") +
  labs(title = "Scatterplot of sales ~ youtube")


###### plot of weighted regression line after removal of first influential point

ggplot(wdm_re_1, aes( ro105_data.youtube , ro105_data.sales)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, 
              aes(weight = ro105_data$X.weights.),
              color = "blue", 
              size = 0.8,
              linetype = "dashed") +
  labs(title = "Scatterplot of sales ~ youtube")

  
#--------------------------------------------- NOT USEFUL ----------------------------------------------------------------------------

## Second removal

ro145_data <- subset(wdm_re_1 , select = c(1,2,3) , .cooksd < 0.2 ) 

wls_re_fit_2 <- lm(ro145_data$ro105_data.sales ~ ro145_data$ro105_data.youtube , weights = ro145_data$X.weights.)
summary(wls_re_fit_2)                 #  MSE = 1.169,  ADJ R2 = 0.7353,  All coefficients and over all model is significant.

# Diagnostic metrics
wdm_re_2 <- augment(wls_re_fit_2)

# Diagnostic plots
plot(wls_re_fit_2)
plot(wls_re_fit_2 , 4)
plot(wls_re_fit_2 , 4 , id.n = 7)


## Third removal

ro144_data <- subset(wdm_re_2 , select = c(1,2,3) , .cooksd < 0.09 ) 

wls_re_fit_3 <- lm(ro144_data$ro145_data.ro105_data.sales ~ ro144_data$ro145_data.ro105_data.youtube , weights = ro144_data$X.weights.)
summary(wls_re_fit_3)                 #  MSE = 1.158,  ADJ R2 = 0.7303,  All coefficients and over all model is significant.

# Diagnostic metrics
wdm_re_3 <- augment(wls_re_fit_3)

# Diagnostic plots
plot(wls_re_fit_3)
plot(wls_re_fit_3 , 4)
plot(wls_re_fit_3 , 4 , id.n = 7)



## Fourth removal

ro143_data <- subset(wdm_re_3 , select = c(1,2,3) , .cooksd < 0.066 ) 

wls_re_fit_4 <- lm(ro143_data$ro144_data.ro145_data.ro105_data.sales ~ ro143_data$ro144_data.ro145_data.ro105_data.youtube , weights = ro143_data$X.weights.)
summary(wls_re_fit_4)                 #  MSE = 1.152,  ADJ R2 = 0.7226,  All coefficients and over all model is significant.

# Diagnostic metrics
wdm_re_4 <- augment(wls_re_fit_4)

# Diagnostic plots
plot(wls_re_fit_4)
plot(wls_re_fit_4 , 4)
plot(wls_re_fit_4 , 4 , id.n = 7)

## Fifth removal

ro142_data <- subset(wdm_re_4 , select = c(1,2,3) , .cooksd < 0.07 ) 

wls_re_fit_5 <- lm(ro142_data$ro143_data.ro144_data.ro145_data.ro105_data.sales ~ ro142_data$ro143_data.ro144_data.ro145_data.ro105_data.youtube , weights = ro142_data$X.weights.)
summary(wls_re_fit_5)                 #  MSE = 1.144,  ADJ R2 = 0.7174,  All coefficients and over all model is significant.

# Diagnostic metrics
wdm_re_5 <- augment(wls_re_fit_5)

# Diagnostic plots
plot(wls_re_fit_5)
plot(wls_re_fit_5 , 4)
plot(wls_re_fit_5 , 4 , id.n = 7)


#sort(cooks.distance(wls_re_fit_3))

#------------------------------------------------------------------------------------------------------------------------------------

########### Simple Linear Regression in sales vs facebook
attach(train.data)
par(mfrow=c(2,2))
plot(facebook , sales)
cor(facebook , sales)          # r = 0.5899
cor.test(facebook , sales)     # significant

# Build model
model_2 <- lm(sales ~ facebook)
summary(model_2)                    #  MSE = 5.169,  ADJ R2 = 0.344,  All coefficients and over all model is significant.
plot(model_2)
# hist(newspaper)
# summary(sales)


# Weighted Simple linear regression (shape of megaphone in residual vs fitted plot and absence of horizontal line in scale location plot)
sd.func1 <- lm(abs(model_2$residuals) ~ facebook)
summary(sd.func1)

weigts1 <- 1/((sd.func1$fitted.values)^2)

wlsfit_1 <- lm(sales ~ facebook , weights = weigts1)
summary(wlsfit_1)                    #  MSE = 1.258,  ADJ R2 = 0.4066,  All coefficients and over all model is significant.

# Diagnostic metrics
wdm1 <- augment(wlsfit_1)

# Diagnostic plots
par(mfrow = c(2,2))
plot(wlsfit_1)
plot(wlsfit_1 , 4)

########################## removing observation having highest cooks distance #####################################
########################## Not useful #############################################################################
wlr <- subset(as.data.frame(wdm1),select=c(1,2,3), .cooksd <0.15)
wlsfit_2 <- lm(wlr$sales ~ wlr$facebook , weights = wlr$X.weights.)
summary(wlsfit_2)                    #  MSE = 1.232,  ADJ R2 = 0.3963,  All coefficients and over all model is significant.

# Diagnostic metrics
wdm2 <- augment(wlsfit_2)

# Diagnostic plots
par(mfrow = c(2,2))
plot(wlsfit_2)
plot(wlsfit_2 , 4)


#-------------------------------------------------------------------------------------------------------------------------

plot(sales , newspaper)
cor(sales , newspaper)
cor.test(sales , newspaper)

# Build model
model_3 <- lm(sales ~ newspaper)
summary(model_3)                    #  MSE = 6.193,  ADJ R2 = 0.05834,  All coefficients and over all model is significant.
plot(model_3,1)                     # Linearity assumption voilates

res2 <- (model_3$residuals)^2
#sqrt(sum(res2)/(160))     ----->  RSE = MSE    (FOR TRAINING DATA)

#-------------------------------------------------------------------------------------------------------------------------




#### K- fold Cross validation to check which model will perform best 


#datacv = read.csv(file.choose(),header=T)
library(tidyverse)
library(caret)

#  load marketing data set as follow:
data("marketing" , package = "datarium")
datacv <- as.data.frame(marketing)
#---------------------------------------------------------------------
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model
model_cv <- train(sales ~ youtube, data = marketing, method="lm",
               trControl = train.control)

# Summarize the results
print(model_cv)
summary(model_cv)

#---------------------------------------------------------------------
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model
model_cv_1 <- train(sales ~ facebook, data = marketing, method = "rlm",
                  trControl = train.control)

# Summarize the results
print(model_cv_1)
summary(model_cv_1)

#---------------------------------------------------------------------
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model
model_cv_2 <- train(sales ~ newspaper, data = marketing, method = "rlm",
                  trControl = train.control)

# Summarize the results
print(model_cv_2)
summary(model_cv_2)

########################################### Making predictions using sales ~ youtube model ###########################

# Importing test data directly from local disk
test.data <- read.csv(file.choose(),header=T)

# Make predictions and compute the R2, RMSE and MAE
newdata <- data.frame(test.data$youtube)

predictions1 <- wls_re_fit %>% predict(test.data)
predictions <- model_1 %>% predict(test.data)

length(as.numeric(test.data$youtube))

data.frame( R2 = R2(predictions, test.data$sales),
            RMSE = RMSE(predictions, test.data$sales),
            MAE = MAE(predictions, test.data$sales))














install.packages("lmtest")
library(lmtest)
bptest(model_1)
bptest(wls_re_fit)


####-----------------------------------------------------------------------------------------------------------------------

weigt <- 1/train.data$sales

wlsfit1 <- lm(sales ~ youtube , weights = weigt ,data = train.data)
summary(wlsfit1)                    #  MSE = 1.268,  ADJ R2 = 0.7345,  All coefficients and over all model is significant.

# Diagnostic metrics
wdm_11 <- augment(wlsfit1)

# Diagnostic plots
plot(wlsfit1)
plot(wlsfit1 , 1)





pairs(marketing)

