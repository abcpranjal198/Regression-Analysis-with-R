train.data <- read.csv("123.csv",header = T)
test.data <- read.csv("234test.csv",header = T)

a <- train.data$sales
a <- (a-mean(a))/sd(a)  

train.data["sales"] <- a 
boxplot(train.data$sales)



train.data <- read.csv("1231.csv",header = T)



boxTidwell(sales ~ youtube + facebook , data = train.data)

a <- ((train.data$youtube^0.35057)-1)/0.35057

ma <- lm(sales ~ a + facebook , data = train.data)



hist(ma$residuals)
bptest(ma)
shapiro.test(ma$residuals)
car::vif(ma)




BC = boxcox(ma)
best.lm = BC$x[which(BC$y==max(BC$y))]
ml_model <- lm((sales^(0.54)) ~ a + facebook , data = train.data)
hist(ml_model$residuals)

shapiro.test(ml_model$residuals)

aug <- augment(ml_model)
plot(ml_model,4)


bd <- subset(as.data.frame(aug) , select=c(1,2,3),.cooksd < 0.5)
sales <- bd$X.sales..0.54..
youtube <- bd$a
facebook <- bd$facebook

ml <- lm(sales ~ youtube + facebook)

ac <- augment(ml)
hist(ml$residuals)
plot(ml,4)
shapiro.test(ml$residuals)
plot(ml)

bptest(ml)

ols_test_score(ml)
summary(ml)

library(tidyverse)
pref <- ml %>% predict(test.data)
library(caret)
data.frame( R2 = R2(pref, test.data$sales),
            RMSE = RMSE(pref, test.data$sales),
            MAE = MAE(pref, test.data$sales))


#########################################################################################################



test.data$youtube <- ((test.data$youtube^0.35057)-1)/0.35057



pref <- ml %>% predict(test.data)
pref <- pref^(1/0.54)

data.frame( R2 = R2(pref, test.data$sales),
            RMSE = RMSE(pref, test.data$sales),
            MAE = MAE(pref, test.data$sales))

sqrt(mean((pref-test)^2))

test <- as.numeric(test.data$sales)


test[1]-pref[1]


pref - test


mean((ac$.resid)^2)
sqrt(mean((ac$.resid)^2))

###################################################################################

hist(ml$residuals)
shapiro.test(ml$residuals)
bptest(ml)
plot(ml)


wts <- 1/fitted(lm(abs(residuals(ml)) ~ facebook))^2
ml_1 <- lm(sales ~ youtube + facebook , weights = weigts1)
summary(ml_1)
plot(ml_1)

hist(ml_1$residuals)
bptest(ml_1)
shapiro.test(ml_1$residuals)


library(mgcv)
gm <- gam(sales ~ s(youtube) + facebook )
summary(gm)

plot(gm)

# Checking normality assumptions 
hist(gm$residuals)                                # errors are  normaly distributed
#ad.test(gm$residuals)                             # errors are not normaly distributed
shapiro.test(gm$residuals)                        # errors are  normaly distributed

#checking independency of errors
durbinWatsonTest(gm)                              # errors are independent

# Checking heterosadasticity
# bptest can not be used due to non normality of errors
bptest(gm)                                        # indicates homosadsticity   (doubt)
#ols_test_score(gm$residuals)  
#ols_test_f(gm$residuals)       all these tests can not be applied as it is suitable for lm() not for gam().


# Make predictions
predictions_gm <- gm %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions_gm, test.data$sales),
  R2 = R2(predictions_gm, test.data$sales)
)                                                  #   RMSE        R2
# 1.380904   0.9443517    






























