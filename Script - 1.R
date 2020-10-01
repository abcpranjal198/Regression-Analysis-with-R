
data = read.csv(file.choose(),header=T)
data = data[-1]
attach(data)
colnames(data)
plot(Length1/Height^0.5 ,Weight/Height^0.5 )
plot(Length2 ,Weight )
plot(Length3 ,Weight )
plot(Height ,Weight )
plot(Width ,Weight )


model = lm((Weight+1) ~  . -1 -Length1, data = data , weights=1/(Weight+1))
summary(model)
par(mfrow=c(2,2))
plot(model)


a=var(model$residuals)

hist(Weight)
hist(Length1)
hist(Length2)
hist(Length3)
hist(Height)
hist(Width)

hist(model$residuals)

boxplot(data)

library(lmtest)
bptest(model)

for (i in 1:ncol(data)){ 
  
  data[ , i] <- (data[ , i] - min(data[i] , na.rm = T))/
    (max(data[i] , na.rm = T) - min(data[i] , na.rm = T))
}


data = read.csv(file.choose(),header=T)


model= lm(T ~ . -v2-1, data =data)
summary(model)
plot(model)

colnames(data)

library(tidyverse)
a = data %>%
    filter(ï..Species == "Bream")

class(a)
a = a[-1]
boxplot(a)
model = lm(Weight ~ length + Height + Width -1 , data = a)
summary(model)
shapiro.test(model$residuals)
par(mfrow=c(2,2))
plot(model)
hist(model$residuals)
library(broom)
dm = augment(model)
plot(model,4)

library(olsrr)
ols_test_f(model)
a = a[-34,]

plot(a$length ,a$Height)
plot(a$length ,a$Width)

library(caret)
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model
model_cv <- train(Weight ~ length + Height + Width -1, data = a, method="lm",
                  trControl = train.control)

# Summarize the results
print(model_cv)

library(car)
car::vif(model)

durbinWatsonTest(model)

attach(a)
plot(Length1 ,Weight )
plot(Length2 ,Weight )
plot(Length3 ,Weight )
plot(Height ,Weight )
plot(Width ,Weight )
pairs(a)
cor(a)


install.packages("glmnet")
x = as.matrix(a[-1])
y = as.vector(a[1])
library(glmnet)
fit <- glmnet(x, y, alpha = 0)
summary(fit)


a["length"] = a$Length1 + a$Length2 + a$Length3












