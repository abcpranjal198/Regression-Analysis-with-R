
setwd("D:\\Regression Model")

# Importing data 
train.data <- read.csv("123.csv",header = T)
test.data <- read.csv("234test.csv",header = T)

# Data Standardization
test.data$youtube <- (test.data$youtube - mean(test.data$youtube))/sd(test.data$youtube)  

hist(train.data$sales)

#boxTidwell(sales ~ youtube + facebook , data = train.data)
#BC= boxcox(sales ~ youtube + facebook , data = train.data)
#best.lm = BC$x[which(BC$y==max(BC$y))]


# Data transformation
train.data$sales <- (train.data$sales)^0.54
train.data$youtube <- ((train.data$youtube^0.35057)-1)/0.35057
test.data$youtube <- ((test.data$youtube^0.35057)-1)/0.35057

# Build model
pol_1 <- lm(sales ~ facebook + poly(youtube,1,raw = T), data = train.data)

summary(pol_1)

# Plotting
par(mfrow=c(2,2))
plot(pol_1)

# Checking Normality of errors
hist(pol_1$residuals)                     # Left skewed
shapiro.test(pol_1$residuals)             # errors are not normally distributed 
library(nortest)
ad.test(pol_1$residuals)                  # errors are normally distributed

# Checking idnependency of errors
durbinWatsonTest(pol_1)                   # errors are independent

# Checking hetrosadsticity
 bptest(pol_1)
ols_test_score(pol_1,rhs=F)               # homosadasticity
ols_test_f(pol_1)                         # homosadasticity
# ?ols_test_score

# Checking multicollinearity
plot(train.data$facebook , train.data$youtube)
plot(train.data$facebook , (train.data$youtube)^2)
plot(train.data$facebook , (train.data$youtube)^3)

cor(train.data$facebook , train.data$youtube)
cor(train.data$facebook , (train.data$youtube)^2)
cor(train.data$facebook , (train.data$youtube)^3)

cor.test(train.data$facebook , (train.data$youtube))       # correlation is not significant
cor.test(train.data$facebook , (train.data$youtube)^2)     # correlation is not significant
cor.test(train.data$facebook , (train.data$youtube)^3)     # correlation is not significant
vif(pol_1)                                                 # No multicollineariy


# Making predictions
pred <- pol_1 %>% predict(test.data)
pred <- pred^(1/0.54)
data.frame( R2 = R2(pred, test.data$sales),
            RMSE = RMSE(pred, test.data$sales),
            MAE = MAE(pred, test.data$sales))                     # R2         RMSE       MAE
# 0.9436412  1.386676   1.074112

# Detecting influential observation
plot(pol_1,4)

# Diagnostic metrics
pol_1_dm <- augment(pol_1)                                  # No influential observation






#####################################################################################################################

pol_1 <- lm(sales ~ facebook + youtube, data = train.data)
summary(pol_1)

par(mfrow=c(2,2))
plot(pol_1)

dm <- augment(pol_1)
train.data <- train.data[-131 ,]

# Making predictions
pred <- pol_1 %>% predict(test.data)
pred <- pred^(1/0.54)
data.frame( R2 = R2(pred, test.data$sales),
            RMSE = RMSE(pred, test.data$sales),
            MAE = MAE(pred, test.data$sales))


hist(pol_1$residuals)
shapiro.test(pol_1$residuals)
scatter.smooth(pred,test.data$sales)
vif(pol_1)
bptest(pol_1)
durbinWatsonTest(pol_1)

################################################################################

# Build the model
gm <- gam(sales ~ s(youtube) + facebook , data = train.data )
summary(gm)

# Plotting
plot(gm)

# Checking influential point 
admd <- augment(gm)   
train.data <- train.data[-131 ,]

# Make predictions
predictions_gm <- gm %>% predict(test.data)
predictions_gm <- (predictions_gm)^(1/0.9)
# Model performance
data.frame(
  RMSE = RMSE(predictions_gm, test.data$sales),
  R2 = R2(predictions_gm, test.data$sales)
)


hist(gm$residuals)
shapiro.test(gm$residuals)
scatter.smooth(predictions_gm,test.data$sales)
durbinWatsonTest(gm)

     


library(MASS)
?boxcox()
boxcox(sales ~. , data = train.data)


plot(train.data$facebook, train.data$sales)
hist(log(train.data$facebook))
hist(train.data$youtube)

install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(marketing[-3])

