# Build model
ml_model <- lm(sales ~ . -newspaper, data = train.data)
summary(ml_model)

ml_model1 <- lm(sales ~ . , data = train.data)
summary(ml_model1)

anova(ml_model,ml_model1)

boxplot(marketing)

############################################################################################
m1 = lm(sales ~ youtube, data = train.data)
summary(m1)                                                         # 0.5951

m2 = lm(sales ~ facebook, data = train.data)  
summary(m2)                                                         # 0.3249

m3 = lm(sales ~ newspaper, data = train.data)
summary(m3)                                                         # 0.05768



m4 = lm(sales ~ youtube + facebook , data = train.data)
summary(m4)                                                         # 0.8919
vif(m4)
plot(m4)
anova(m1,m4)    # Statistically significant
bptest(m4)
shapiro.test(m4$residuals)
durbinWatsonTest(m4)
ols_test_f(m4)
dm3 = augment(m4)
data1 = as.data.frame(dm3 %>% filter(.cooksd < 0.3) %>%select(c(2,3,4)))
m5 = lm(sales ~ youtube + facebook , data = data1)
summary(m5)
plot(m5)

attach(train.data)
scatterplot3d(train.data[c(1,2,4)],highlight.3d=T)
abline(lm(sales ~ youtube + facebook))
?scatterplot3d
f=outer(youtube,facebook,sales = youtube+facebook)
contour(youtube,facebook,sales)






m5 = lm(sales ~ youtube + facebook + newspaper, data = train.data)
summary(m5)                                                         # 0.8914


# sales = youtube + facebook + error

train.data["y2"] <- (train.data$youtube)*(train.data$facebook)
m15 = lm(sales ~ poly(youtube,12) + facebook + y2, data = train.data)
summary(m15) 
vif(m15)
shapiro.test(m15$residuals)
library(car)
durbinWatsonTest(m15)
library(olsrr)
ols_test_f(m15)
hist(m15$residuals)
plot(m15,4)

data15 = train.data %>% filter(sales > 1.92)
m16 = lm(sales ~ poly(youtube,6) + facebook + y2, data = data15)
summary(m16) 
vif(m16)
shapiro.test(m16$residuals)
durbinWatsonTest(m16)
ols_test_f(m16)
hist(m16$residuals)
par(mfrow=c(2,2))
plot(m16)


library(psych)


test.data["y2"] <- (test.data$youtube)*(test.data$facebook)

prediction <- m16 %>% predict(test.data)
data.frame( R2 = R2(prediction, test.data$sales),
            RMSE = RMSE(prediction, test.data$sales),
            MAE = MAE(prediction, test.data$sales)) 


data=marketing
data["y2"] <- (data$youtube)*(data$facebook)
data["Y2"]= data$
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model
model_cv <- train(sales ~ poly(youtube,6) + facebook + y2, data = data, method="lm",
                  trControl = train.control)

# Summarize the results
print(model_cv)
#############################################################################################


plot(m4,1)

pairs(marketing)


m6 = lm(sales ~ poly(youtube,3) + facebook  , data = train.data)
summary(m6)                                                         # 

vif(m6)


m7 = lm(sales ~ poly(youtube,4) + facebook  , data = train.data)
summary(m7)                                                         # 

vif(m7)
plot(m7)
anova(m6,m7)

shapiro.test(m7$residuals)
hist(m7$residuals)
bptest(m7)
ols_test_f(m7)
durbinWatsonTest(m7)





########################################################################################################
library(splines)

# Build the model
knots <- quantile(train.data$youtube, p = c(0.25, 0.5, 0.75))
spl_1 <- lm (sales ~ bs(youtube, knots = knots) + facebook , data = train.data)
summary(spl_1)

anova(m7,spl_1)

plot(spl_1)

durbinWatsonTest(spl_1)
shapiro.test(spl_1$residuals)
vif(spl_1)
1/vif(spl_1)
mean(vif(spl_1))


library(olsrr)
ols_test_f(spl_1)  

library(broom)
dm = augment(spl_1)
plot(spl_1,4)                       # One influential point

data = train.data %>% filter(train.data$sales != 1.92)

knots <- quantile(data$youtube, p = c(0.25, 0.5, 0.75))
spl_1 <- lm (sales ~ bs(youtube, knots = knots) + facebook , data = data)
summary(spl_1)

anova(m7,spl_1)

plot(spl_1)

durbinWatsonTest(spl_1)
shapiro.test(spl_1$residuals)
vif(spl_1)
1/vif(spl_1)
mean(vif(spl_1))


library(olsrr)
ols_test_f(spl_1)
library(lmtest)
bptest(spl_1)

library(broom)
dm = augment(spl_1)
plot(spl_1,4)       


summary(train.data$youtube)

######################################################################################################

boxplot(train.data$youtube)

pairs(train.data)

train.data["y2"] = (train.data$youtube)^2
m9 = lm(sales ~ youtube*facebook , data = train.data)
summary(m9)                                                         # 0.9654 

vif(m9)
anova(m4,m9)

m10 = lm(sales ~ youtube*facebook*newspaper  - newspaper -youtube:newspaper-facebook -youtube + poly(youtube,7), data = train.data )
summary(m10)                                                         # 0.9944 

vif(m10)
#anova(m6,m10)
par(mfrow=c(2,2))
plot(m10)

shapiro.test(m10$residuals)
bptest(m10)
durbinWatsonTest(m10)
ols_test_score(m10)


#######################################################################

attach(train.data)
sd.func <- lm((m10$residuals)^2 ~ 1/m10$fitted.values)
summary(sd.func)

weigts <- 1/((sd.func$fitted.values)^2)

m11 = lm(sales ~ youtube*facebook*newspaper  - newspaper -youtube:newspaper-facebook -youtube + poly(youtube,5), data = train.data ,weights = weigts)
summary(m11)                                                                             # 0.9949 
plot(m11)
dm1 = augment(m11)
#vif(m11)
#anova(m6,m10)
#par(mfrow=c(2,2))

a <- weighted.residuals(m11)
plot(m11$fitted.values,a)

shapiro.test(m11$residuals)
bptest(m11)
durbinWatsonTest(m11)
ols_test_score(m11)

shapiro.test(sales)
library(MASS)
BC = boxcox(m11,c(-10,10))
best.lm = BC$x[which(BC$y==max(BC$y))]
hist(sales^2)


plot(m11$fitted.values , a)

m12$residuals <- a

data = as.data.frame(dm2 %>% filter(.cooksd < 0.5) %>%select(c(1,2,3,4,5)))
weigts1 = data$`(weights)`         
m12 = lm(sales ~ youtube*facebook*newspaper  - newspaper -youtube:newspaper-facebook -youtube + poly(youtube,3) -`(weights)`, data = data ,weights = weigts1)
summary(m12)                                                                             # 0.9949 
plot(m12)

dm2 = augment(m12,data)

a = weighted.residuals(m12)
plot(m12$fitted.values , a)






############################################################################################################


# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model
model_cv <- train(sales ~ youtube*facebook, data = train.data, method="lm",
                  trControl = train.control)

# Summarize the results
print(model_cv)


durbinWatsonTest(m6)
shapiro.test(m6$residuals)
vif(m6)
1/vif(m6)
mean(vif(m6))


library(olsrr)
ols_test_f(m6)  

library(broom)
dm = augment(m6)
plot(spl_1,4)                       # One influential point

##############################################################################################
library(mgcv)

# Build the model
gm <- gam(sales ~ s(youtube) + facebook , data = train.data)
summary(gm)

# Plotting
plot(gm)

# Checking normality assumptions 
hist(gm$residuals)                                # errors are not normaly distributed
ad.test(gm$residuals)                             # errors are not normaly distributed
shapiro.test(gm$residuals)                        # errors are not normaly distributed

#checking independency of errors
durbinWatsonTest(gm)                              # errors are independent

# Checking heterosadasticity
# bptest can not be used due to non normality of errors
bptest(gm)   

####################################################################
a = lm(sales ~ youtube + facebook ,data =train.data)
summary(a)

ms = lm(sales ~ poly(youtube,8,raw=T)  +  youtube:facebook ,data =train.data)
summary(ms)

train.data['y2'] = (train.data$youtube)^2
ms1 = lm(sales ~ poly(youtube,8,raw=T) + poly(facebook,1,raw=T)+youtube:facebook ,data =train.data)
summary(ms1)

par(mfro=c(2,2))
plot(ms)


anova(ms,ms1)

ms = lm(sales ~ poly(youtube,9,raw=T) + facebook +  youtube:facebook ,data =train.data)
summary(ms)

anova(ms1,ms)

shapiro.test(ms$residuals)
bptest(ms)
library(tidyverse)








data = read.csv(file.choose() , header =T)

# Randomly Split the data into training and test set
set.seed(123)
training.samples <- data$Sales %>%
  createDataPartition(p = 0.5, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

pm1 <- lm(Sales ~ poly(TV ,2,raw = T) + poly(Radio ,3,raw = T) + TV:Radio , data = data)
summary(pm1)

par(mfrow=c(2,2))
plot(pm1)
shapiro.test(pm1$residuals)
hist(pm1$residuals)
bptest(pm1)
ols_test_f(pm1)
library(caret)
durbinWatsonTest(pm1)
vif(pm1)

dm = augment(pm1)
min(dm$.std.resid)
which(dm$.std.resid == "-3.654809")
data = train.data %>% filter(train.data$Sales > 1.6)


data["tv2"] = (data$TV)^2
data["R2"] = (data$Radio)^2
data["R3"] = (data$Radio)^3
data["int"] = (data$TV) * (data$Radio)
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", number = 4, repeats = 2)
                              

# Train the model
model_cv <- train(Sales ~  ., data = data, method="lm",
                  trControl = train.control)

# Summarize the results
print(model_cv)

model_cv1 <- lm(Sales ~  poly(TV , 1 ) + , data = data)

vif(model_cv1)
# Making predictions
prediction <- pm1 %>% predict(test.data)
data.frame( R2 = R2(prediction, test.data$Sales),
            RMSE = RMSE(prediction, test.data$Sales),
            MAE = MAE(prediction, test.data$Sales)) 


data = marketing
write.csv(data , "Advertising Data set.csv",row.names = F)


attach(marketing)
plot(youtube , sales)               # r = 0.7877721
plot(facebook , sales)              # r = 0.589996
plot(newspaper , sales)             # r = 0.2533473

cor.test(youtube , sales)          # significant correlation        
cor.test(facebook , sales)         # significant correlation
cor.test(newspaper , sales , method = "kendall")        # significant correlation


ml = lm(sales ~. , data = marketing )
plot(ml,1)

summary(ml)



get(model_info())



