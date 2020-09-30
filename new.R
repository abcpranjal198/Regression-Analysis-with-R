data1 <- data

str(data1)
data1[1,22] <- "3"

a <- lm(Attrition_rate ~ . +0 , data = data1)
summary(a)



b <- lm(Attrition_rate^0.26 ~ Education_Level + Gender +0 , data=data1)
summary(b)



test.data <- read.csv("testcomp.csv",header=T)
library("tidyverse")
pred <- b %>% predict(test.data)
data.frame( R2 = R2(pred, test.data$Attrition_rate),
           RMSE = RMSE(pred, test.data$Attrition_rate),
           MAE = MAE(pred, test.data$Attrition_rate)) 


mtcars.pca <- prcomp(data[,c(-1,-4,-5,-6,-7,-14,-23)], center = TRUE,scale. = TRUE)

summary(mtcars.pca)


###################################################################################################

md <- lm(Attrition_rate ~ Gender + VAR2 + Time_of_service + Time_since_promotion + Work_Life_balance+Decision_skill_possess+Unit+Compensation_and_Benefits +0 ,data=data)
summary(md)

data <- data[c(-2365,-3606),]

av1 <- lm(Attrition_rate ~   Education_Level +0 , data = data)
summary(av1)
print(av1)

library(MASS)
BC = boxcox(Attrition_rate ~  +0 , data = data)
best.lm = BC$x[which(BC$y==max(BC$y))]

pred <- av1 %>% predict(test.data)
pred <- pred^(1/0.26)

library(leaps)
library(caret)
data.frame( R2 = R2(pred, test.data$Attrition_rate),
            RMSE = RMSE(pred, test.data$Attrition_rate),
            MAE = MAE(pred, test.data$Attrition_rate)) 
plot(test.data$Attrition_rate,pred)
cor(data$Age,data$Attrition_rate)
