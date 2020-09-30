getwd()
setwd("D:\\Regression Model")
getwd()

# First install the datarium package:
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/datarium")

#---------------------------------------------------------------------------------------------------

# Loading Required R packages
  
# tidyverse for easy data manipulation and visualization
library(tidyverse)

# caret for easy machine learning workflow
#install.packages("caret")
library(caret)
theme_set(theme_bw())

#---------------------------------------------------------------------------------------------------

#  load marketing data set as follow:
data("marketing" , package = "datarium")

# Inspect data set
head(marketing , 5)

# inspect data randomly
sample_n(marketing , 5)
#--------------------------------------------------------------------------------------------------

## Loading train data directly
train.data <- read.csv("123.csv",header=T)

#--------------------------------------------------------------------------------------------------

# Randomly Split the data into training and test set
set.seed(123)
training.samples <- marketing$sales %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- marketing[training.samples, ]
test.data <- marketing[-training.samples, ]

#--------------------------------------------------------------------------------------------------

# Build Model
# Summarize Model
# Model diagnostic by numerical measures and plots (check assumptions ,if any)
# Make predictions
# Model performance (Prediction error/RMSE , R-square)

#--------------------------------------------------------------------------------------------------
attach(train.data)
par(mfrow = c(2,2))
plot(youtube , sales)
plot(facebook , sales)
plot(newspaper , sales)

cor(youtube , sales)               # r = 0.7877721
cor(facebook , sales)              # r = 0.589996
cor(newspaper , sales)             # r = 0.2533473

cor.test(youtube , sales)          # significant correlation        
cor.test(facebook , sales)         # significant correlation
cor.test(newspaper , sales)        # significant correlation

cor.test(youtube , facebook)       # r = 0.08658385 
cor.test(youtube , newspaper)      # r = 0.09033378
cor.test(facebook , newspaper)     # r = 0.3475573     significant correlation

plot(youtube,newspaper)
plot(youtube , facebook)
plot(facebook,newspaper)

hist(log(sales))

########## Simple Linear Regression ############

# Build model
model_1 <- lm(sales ~ youtube , data = train.data)

# Summarize Model
summary(model_1)

# Model Diagnostic

## broom: creates a tidy data frame (tibble) from statistical test results
library(broom)
theme_set(theme_classic())

## Obtaining dignostic metrics for model_1
model_1_dm <- augment(model_1)                            #### dm : diganostic metrics
head(model_1_dm)

## Plotting diagnostic plots
par(mfrow = c(2,2))

plot(model_1)               ### shows violation in homogenity of variance assumption (plot - 3) as well as influential points exists

plot(model_1 , 1)           ### fitted vs residual   -------> To check Linearity assumption between predictor and target 
plot(model_1 , 2)           ### normal Q-Q plot      -------> To check errors are normaly distributed or not
plot(model_1 , 3)           ### Spread location      -------> To check homosaedacity (constant error variance)
plot(model_1 , 4)           ### Cook's distance      -------> To determine observation no with highest (top - 3) cook's distance
## to see top 5 observations in plot - 4
plot(model_1, 4, id.n = 5)  ### Cook's distance
plot(model_1, 5)            ### Leverage vs standardized residuals  --------> To check outliers and high liverage point (influential points)

## see the regression line with residual error
ggplot(model_1_dm, aes(youtube, sales)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = youtube, yend = .fitted), color = "red", size = 0.3)

#--------------------------------------------------------------------------------------------------

## removing violation of homogenity of error variance from model_1 by transforming target (sales)
train.data['log_sales'] <- log(train.data['sales'])

## build new model
new_model_1 <- lm(log_sales ~ youtube , data = train.data)
summary(new_model_1)

new_model_1_dm <- augment(new_model_1)                              #### dm : diganostic metrics
head(new_model_1_dm)

par(mfrow = c(2,2))
plot(new_model_1)

## build another model
new1_model_1 <- lm(sqrt(sales) ~ youtube , data = train.data)
summary(new1_model_1)

new1_model_1_dm <- augment(new1_model_1)                            #### dm : diganostic metrics
head(new1_model_1_dm)

par(mfrow = c(2,2))
plot(new1_model_1)

#---------------------------------------------------------------------------------------------------
### Comparing model_1, new_model_1 and new1_model_1

##                  Adjusted R2        RSE(MSE)        Assumptions
##  model_1          0.6182              3.943       Homogenity violated
##  new_model_1      0.6148              0.2656      Homogenity not violated (log_sales)   ----> Most stable model due to less MSE
##  new1_model_1     0.6377              0.4788      Homogenity violated but less than model_1 (sqrt_sales)

#---------------------------------------------------------------------------------------------------

## removing effect of influential points from regression model

#getwd()
#write.csv(train.data,"123.csv")
#write.csv(test.data,"234test.csv")
#train.data <- read.csv(file.choose(),header =T)
#test.data <- read.csv(file.choose(),header =T)

#model_1_dm_index <- model_1_dm %>%
#  mutate(index = 1:nrow(model_1_dm)) %>%
#  select(index, everything())

plot(model_1, 4, id.n = 12) 

# Subsetting data 
par(mfrow=c(2,2))
dm <- model_1_dm

train.data.rip_1 <- subset(as.data.frame(dm) , .cooksd < 0.034188 , select = c(3,2))

model_1_rip <- lm(sales ~ youtube , data = train.data.rip_1)
summary(model_1_rip)
plot(model_1_rip)

dm <- augment(model_1_rip)

-----------------------------------------------------------------------------------

# log scale

newmodelrip <- lm(log(sales)~youtube , data = as.data.frame(train.data.rip_1))
summary(newmodelrip)
plot(newmodelrip)
dm1 <- augment(newmodelrip)

## removin influential points

train.data.rip_2log <- subset(as.data.frame(dm1) , .cooksd < 0.0363636 , select = c(3,2))

model_2_rip <- lm(log.sales. ~ youtube , data = train.data.rip_2log)
summary(model_2_rip)
plot(model_2_rip)

dm1 <- augment(model_2_rip)

# MSE = 0.1544 ADJR2 = 0.7542

#----------------------------------------------------------------------------

# sqrt

new1_modelrip <- lm(sqrt(sales)~youtube , data = as.data.frame(train.data.rip_1))
summary(new1_modelrip)
plot(new1_modelrip)
dm2 <- augment(new1_modelrip)

## removin influential points

train.data.rip_2sqrt <- subset(as.data.frame(dm2) , .cooksd < 0.035714 , select = c(3,2))

model_3_rip <- lm(sqrt.sales. ~ youtube , data = train.data.rip_2sqrt)
summary(model_3_rip)
plot(model_3_rip)

dm2 <- augment(model_3_rip)

######## MSE = 0.2985  ADJR2 = 0.7715
#----------------------------------------------------------------------------------

model_1y2 <- lm(sales ~ (youtube) , data = train.data)
plot(model_1y2)
summary(model_1y2)

#### polynomial model

p1 = lm(log(sales) ~ poly(youtube, 6, raw = TRUE), data = train.data)
summary(p1)
plot(p1)

### spline regression
knots <- quantile(train.data$youtube, p = c(0.25, 0.5, 0.75))
library(splines)
model <- lm (sales ~ bs(youtube, knots = knots), data = train.data)
summary(model)
plot(model)
# 
library(mgcv)
model1 <- gam(sales ~ s(youtube), data = train.data)
summary(model1)
plot(model1)


# weighted least square

model_1 <- lm(sales ~ youtube , data = train.data)
plot(model_1)


sd.func <- lm(abs(model_1$residuals) ~ train.data$youtube)
summary(sd.func)

weigts <- 1/((sd.func$fitted.values)^2)
weigts

wlsfit <- lm(train.data$sales ~ train.data$youtube , weights = weigts)
summary(wlsfit)
plot(wlsfit)

dm8 <- augment(wlsfit)

## removin influential points

wlsfitdata <- subset(as.data.frame(dm9) , .cooksd < 0.0268 , select = c(1,2,3))

model_9_rip <- lm(wlsfitdata$wlsfitdata.wlsfitdata.wlsfitdata.train.data.sales ~ wlsfitdata$wlsfitdata.wlsfitdata.wlsfitdata.train.data.youtube , weights = wlsfitdata$X.weights.)

summary(model_9_rip)
plot(model_9_rip)

dm9 <- augment(model_9_rip)

install.packages("olsrr")
library(olsrr)

ols_plot_cooksd_bar(model_9_rip)

#------------------------------------------------------------------------------------------------



cal.weights <- 1 / lm(abs(model_1$residuals) ~ model_1$fitted.values)$fitted.values^2
cal.lmw <- lm(sales ~ youtube, 
              data = train.data, 
              weights = cal.weights)
summary(cal.lmw)
plot(cal.lmw)

dm4 <- augment(cal.lmw)

## removin influential points

lwdata <- subset(as.data.frame(dm4) , .cooksd < 0.02684 , select = c(2,3,4))

model_6_rip <- lm(sales ~ youtube, 
                  data = lwdata, 
                  weights = X.weights.)
summary(model_6_rip)
plot(model_6_rip)

dm4 <- augment(model_6_rip)

#------------------------------------------------------

  
  lwdata1 <- subset(as.data.frame(dm5) , .hat < 0.05634 , select = c(2,3,4))

model_7_rip <- lm(sales ~ youtube, 
                  data = lwdata1, 
                  weights = X.weights.)
summary(model_7_rip)
plot(model_7_rip)

dm5 <- augment(model_7_rip)






#--------------------------------------------------------------------------------------------------

model_2 <- lm(sales ~ facebook , data = train.data)
summary(model_2)

model_3 <- lm(sales ~ newspaper , data = train.data)
summary(model_3)

### Model assesment
# broom: creates a tidy data frame from statistical test results
library(broom)
theme_set(theme_classic())

# Creating model diagnostic metrics
model_1.diagnostic.metrics <- augment(model_1)
head(model_1.diagnostic.metrics)

model_2.diagnostic.metrics <- augment(model_2)
head(model_2.diagnostic.metrics)

model_3.diagnostic.metrics <- augment(model_3)
head(model_3.diagnostic.metrics)

# Checking Normality assumption of residuals
hist(model_1_dm$.resid)
hist(model_2.diagnostic.metrics$.resid)
hist(model_3.diagnostic.metrics$.resid)


# plot of residual error with regression line
ggplot(model_1.diagnostic.metrics, aes(youtube, sales)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = youtube, yend = .fitted), color = "red", size = 0.3)


ggplot(model_2.diagnostic.metrics, aes(facebook, sales)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = facebook, yend = .fitted), color = "red", size = 0.3)

ggplot(model_3.diagnostic.metrics, aes(newspaper, sales)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = newspaper, yend = .fitted), color = "red", size = 0.3)

# In order to check regression assumptions, we'll examine the distribution of residuals.
### Diagnostics plots
par(mfrow = c(2, 2))
plot(model_1)


hist(marketing$sales)
a <- log(marketing$sales)
hist(a)

md <- lm((1/train.data$sales)~train.data$youtube)
summary(md)
plot(md)
adm <- augment(md)
        


  adata1 <- subset(as.data.frame(adm1) , .cooksd < 0.5 , select = c(1,2))

modelad1 <- lm(adata1$adata.X.1.train.data.sales. ~ adata1$adata.train.data.youtube) 
                  
summary(modelad1)
plot(modelad1)       

adm2 <- augment(modelad1)



