
library(tidyverse)
library(caret)
library(car)
library(lmtest)
library(olsrr)
#install.packages("pls")
library(pls)
library(elasticnet)
library(glmnet)


data = read.csv(file.choose(),header = T)

# Randomly Split the data into training and test set
set.seed(123)
training.samples <- data$Sales %>%
  createDataPartition(p = 0.75, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]


train.data["TV2"]<- (train.data$TV)^2
train.data["Radio2"]<- (train.data$Radio)^2
train.data["Radio3"]<- (train.data$Radio)^3
train.data["TVRadio"]<- (train.data$TV)*(train.data$Radio)
train.data <- train.data[-3]

data["TV2"]<- (data$TV)^2
data["Radio2"]<- (data$Radio)^2
data["Radio3"]<- (data$Radio)^3
data["TVRadio"]<- (data$TV)*(data$Radio)
data <- data[-3]


model = lm(Sales ~ ., data = train.data)
summary(model)

prediction1 = model %>% predict(test.data)

# Checking performance by calculating R2 , RMSE and MAE
data.frame( R2 = R2(prediction1, test.data$Sales),
            RMSE = RMSE(prediction1, test.data$Sales),                          # 1.264048
            MAE = MAE(prediction1, test.data$Sales))


plot(model)
ols_test_score(model)
shapiro.test(model$residuals)
durbinWatsonTest(model)
vif(model)


#######################################################################################

# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3 , search = "random"
                              )

# Train the model
model_cv <- train(Sales ~ ., data = data, method="glmnet",
                  trControl = train.control , tuneGrid = expand.grid(alpha = 0.4089769 , lambda = 0.001472246) )

# Summarize the results
print(model_cv)

model_cv$bestTune


##################################################################################
library(glmnet)

y <- c()
for(i in 1:151){y[i] <- train.data[i,3]}

x1 <- as.matrix(test.data[-3])
y1 <- c()
for(i in 1:49){y1[i] <- test.data[i,3]}

#####
md = cv.glmnet(x, y, family = "gaussian",alpha = 0 , type.measure = "mse")
print(md)

res =  predict(md ,s = md$lambda.min ,x1)

RMSE(res,y1)                                                                   # 1.473037
cor(res,y)


#####
md1 = cv.glmnet(x, y, family = "gaussian",alpha = 1  ,type.measure = "mse")
print(md1)
summary(md1)
res1 =  predict(md1 ,s = md1$lambda.min ,x1)


RMSE(res1,y1)                                                                 #  1.252529
cor(res1,y1)


rmse2 <- c()
j = 1
for(i in seq(0 , 0.1 , 0.0001)){
  md2 = cv.glmnet(x, y, family = "gaussian",alpha = i , type.measure = "mse")
  res2 =  predict(md2 ,s = md1$lambda.min ,x1)
  rmse2[j] <- RMSE(res2,y1)
  j = j+1
}
min(rmse2)

for(i in seq(0 , 0.1 , 0.001)){print(i)}

i = seq(0 , 0.1 , 0.0001)
a = data.frame(i,rmse2)

#####
md3 = cv.glmnet(x, y, family = "gaussian",alpha = 0.034 , type.measure = "mse")
print(md3)

res1 =  predict(md3 ,s = md1$lambda.min ,x1)


RMSE(res1,y1)                                                                 #  1.234942
cor(res1,y1)


#####
md3 = glmnet(x, y, family = "gaussian",alpha = 0.4089769, lambda =0.001472246 )
print(md3)
plot(md3)
res1 =  predict(md3 ,s = md1$lambda.min ,x1)


RMSE(res1,y1)                                                                 #  1.246796
cor(res1,y1)

#################################################################################################
# Orthogonal polynomial model
pm4 <- lm(Sales ~ poly(TV , 2) + poly(Radio , 3) + TV:Radio  , data = train.data)
summary(pm4)
prediction = pm4 %>% predict(test.data)

# Checking performance by calculating R2 , RMSE and MAE
data.frame( R2 = R2(prediction, test.data$Sales),
            RMSE = RMSE(prediction, test.data$Sales),                         #   1.264048
            MAE = MAE(prediction, test.data$Sales))
prediction[1]
predict(pm4, type = "coefficients")

##################################################################################################
# Principal component regression
model3 = pcr(Sales ~ . , data = train.data , ncomp = 3)
summary(model3)
validationplot(model3)

prediction <- model3 %>% predict(train.data )
data.frame( R2 = R2(prediction, train.data$Sales),
            RMSE = RMSE(prediction, train.data$Sales),
            MAE = MAE(prediction, train.data$Sales))    


plot(model3)
model3$model

vcov(model3)
model3$coefficients
###################################################################################################


ridge <- cv.glmnet(x, y , alpha = 0 , family = "gaussian")

print(ridge)

ridge <- glmnet(x, y , alpha = 0 ,lambda = 0.4759, family = "gaussian")
predict_train <- ridge %>% predict(x)
data.frame( R2 = R2(predict_train, y),
            RMSE = RMSE(predict_train, y),
            MAE = MAE(predict_train, y)) 


predict_test <- ridge %>% predict(x1)
data.frame( R2 = R2(predict_test, y1),
            RMSE = RMSE(predict_test, y1),
            MAE = MAE(predict_test, y1)) 


####################################################################
y <- scale(y , center = TRUE, scale = FALSE)
y <- scale(y , center = TRUE, scale = FALSE)

lasso <- cv.glmnet(x, y , alpha = 1 , family = "gaussian")

print(lasso)

lasso <- glmnet(x, y , alpha = 1 ,lambda = 0.00052, family = "gaussian")
predict_train1 <- lasso %>% predict(x)
data.frame( R2 = R2(predict_train1, y),
            RMSE = RMSE(predict_train1, y),
            MAE = MAE(predict_train1, y)) 


predict_test1 <- lasso %>% predict(x1)
data.frame( R2 = R2(predict_test1, y1),
            RMSE = RMSE(predict_test1, y1),
            MAE = MAE(predict_test1, y1)) 


########################################################################


enet <- cv.glmnet(x, y  ,alpha = 0.034, family = "gaussian", type.measure = "mse")

print(enet)

enet <- glmnet(x, y , alpha = 0.034 ,lambda = 0.0140, family = "gaussian")
predict_train2 <- enet %>% predict(x)
data.frame( R2 = R2(predict_train2, y),
            RMSE = RMSE(predict_train2, y),
            MAE = MAE(predict_train2, y)) 


predict_test2 <- enet %>% predict(x1)
data.frame( R2 = R2(predict_test2, y1),
            RMSE = RMSE(predict_test2, y1),
            MAE = MAE(predict_test2, y1)) 






rmse <- c()
j = 1
for(i in seq(0 , 1 , 0.0001)){
  md2 = cv.glmnet(x, y, family = "gaussian",alpha = i , type.measure = "mse")
  res2 =  predict(md2 ,s = md1$lambda.min ,x1)
  rmse[j] <- RMSE(res2,y1)
  j = j+1
}
min(rmse)
a = seq(0,1,0.0001)
b = data.frame(a,rmse)
plot(a$a , a$rmse)
which(a$rmse == "1.234942")
d = subset(b , b$rmse.1 == 1.234942)


predict(enet, type = "coefficients")

predict_test2[1]
x1[1,]

plot(enet$beta)
lasso$a0
lasso$df
lasso$dim
lasso$lambda
lasso$dev.ratio
lasso$nulldev
lasso$npasses
lasso$jerr
lasso$offset
lasso$call
lasso$nobs


##################################
enet1 <- glmnet(x, y  ,alpha = 0.4089769, lambda = 0.001472246 ,family = "gaussian")

print(enet1)

enet1 <- glmnet(x, y , alpha = 0.1 ,lambda = 0.009499932, family = "gaussian")
predict_train21 <- enet1 %>% predict(x)
data.frame( R2 = R2(predict_train21, y),
            RMSE = RMSE(predict_train21, y),
            MAE = MAE(predict_train21, y)) 


predict_test21 <- enet1 %>% predict(x1)
data.frame( R2 = R2(predict_test21, y1),
            RMSE = RMSE(predict_test21, y1),
            MAE = MAE(predict_test21, y1)) 

enet$beta
