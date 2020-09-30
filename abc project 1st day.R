#########################################################################################
# Getting working directory
getwd()

# Set working directory
setwd("D://Regression Model")
write.csv(test.data , "testcomp.csv")
########################################################################################

# Loading necessary libraries
library(tidyverse)
library(car)        # VIF
library(caret)      # training and testing split

# Load data
data = read.csv(file.choose(),header=T)

# droping irrelevant columns
data <- data[,-1]


#########################################################################################

# Checking Missing values
for (i in 1:ncol(data)){
                         print(names(data[i]))
                         print(sum(is.na(data[i])))
}

###########################################################################################
# Checking class of variables
str(data)

##########################################################################################

# Missing value imputation
summary(data[13])
for (i in 1 :nrow(data)){
  ifelse(is.na(data[i, 19]),data[i,19] <- 2 , data[i,19] <- data[i,19])
}
table(is.na(data))
table(data[19])

###########################################################################################

# Randomly Split the data into training and test set
set.seed(123)
training.samples <- data$Attrition_rate %>%
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

##########################################################################################
names(data)
library(car)
model <- lm(Attrition_rate ~ Gender ,data = data)
             
Anova(model)
summary(model)

#########################################################################################
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model
model1 <- train(Attrition_rate ~ ., data = data, method = "adaboost",
                    trControl = train.control)

# Summarize the results
print(model1)
summary(model_cv_2)

names(getModelInfo())


##############################################################################################

install.packages("leaps")
library(leaps)
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Attrition_rate ~. +0, data = data,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)


data1 <- read.csv(file.choose(),header = T)
data12 <- data1[ , -1]



pred <- model %>% predict(data12)

final <- as.data.frame(data1[,1])
final["Attrition_rate"] <- as.data.frame(pred)

write.csv(final,"Finalcopy.csv",row.names = F)

data.frame( R2 = R2(pred, test.data$Attrition_rate),
            RMSE = RMSE(pred, test.data$Attrition_rate),
            MAE = MAE(pred, test.data$Attrition_rate)) 

train.data <- read.csv("traincomp.csv",header=T)
attach(train.data)

train.data <- train.data[c(-2365,-3606),]
?aov
a <- aov(Attrition_rate ~ Gender+Time_since_promotion + Compensation_and_Benefits)
summary(a)


table(VAR1)
for (i in 1 :nrow(train.data)){
                         if(VAR2[i] == -1.8176){ VAR2[i] <- a }
  
                        # if(VAR2[i] == -0.9612){ VAR2[i] <- b }
                        # if(VAR2[i] == -0.1048){ VAR2[i] <- c}
                        # if(VAR2[i] == 0.7516){ VAR2[i] <- f }
                        # if(VAR2[i] == 1.6081 ){ VAR2[i] <- e }
  
}

d <- lm(Attrition_rate  ~ . +0 , data = train.data  )
summary(d)
?lm

hist(Attrition_rate^0.26)

library(MASS)
BC = boxcox(Attrition_rate ~ . , data = train.data)
best.lm = BC$x[which(BC$y==max(BC$y))]


ac <- as.factor(train.data$Pay_Scale)
ac[1]

###################################################################################################################
train.data <- read.csv("traincomp.csv",header=T)
data <- train.data[, -1]

#names(data)

#Education_Level <- as.factor(data$Education_Level)

#Time_since_promotion <- as.factor(data$Time_since_promotion)


#Travel_Rate <- as.factor(data$Travel_Rate)


#Post_Level <- as.factor(data$Post_Level)

#Pay_Scale <- as.factor(data$Pay_Scale)

#Work_Life_balance <- as.factor(data$Work_Life_balance)

#VAR1  <- as.factor(data$VAR1)

#VAR2 <- as.factor(data$VAR2)

#VAR3 <- as.factor(data$VAR3)

#VAR4 <- as.factor(data$VAR4)

#VAR5 <- as.factor(data$VAR5)

#VAR6 <- as.factor(data$VAR6)

#VAR7 <- as.factor(data$VAR7)

attach(data)
detach(data)
#data1 <- data.frame(data$Gender , data$Age , Education_Level , data$Relationship_Status , data$Hometown , data$Unit , data$Decision_skill_possess , data$Time_of_service
#                    ,Time_since_promotion , data$growth_rate , Travel_Rate , Post_Level , Pay_Scale, data$Compensation_and_Benefits, 
 #                   Work_Life_balance , VAR1, VAR2, VAR3, VAR4, VAR5,VAR6,VAR7 ,data$Attrition_rate)



#data[3] <- as.numeric(data[3])

attach(data)
names(data)
model_new <- aov( Attrition_rate ~ Gender + Age + as.factor(Education_Level) + Relationship_Status + Hometown + Unit +
                   Decision_skill_possess + Time_of_service + as.factor(Time_since_promotion) + growth_rate + as.factor(Travel_Rate) +
                   as.factor(Post_Level) + as.factor(Pay_Scale)+ Compensation_and_Benefits +
                   as.factor(Work_Life_balance) + as.factor(VAR1) + as.factor(VAR2) + 
                   as.factor(VAR3) + as.factor(VAR4) + as.factor(VAR5) + as.factor(VAR6)+as.factor(VAR7) +0) 

summary(model_new)




a =  aov(Attrition_rate ~ as.factor(VAR1) )
summary(a)
print(a)


ms <- lm(Attrition_rate ~ . +0 ,data = data)
summary(ms)

