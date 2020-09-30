
library(tidyverse)
library(caret)

#  load marketing data set as follow:
data("marketing" , package = "datarium")






#-----------------------------------------------10-Fold cross-validation--------------------------------------------
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model
model_cv <- train(sales ~ ., data = marketing, method="leapBackward",
                  trControl = train.control)

# Summarize the results
print(model_cv)
plot(model_cv)
?train
#-----------------------------------------------------------------------------
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model
model_cv_1 <- train(sales ~ youtube + facebook, data = marketing, method="lm",
                  trControl = train.control)

# Summarize the results
print(model_cv_1)                                                                   #  RMSE     Rsquared   MAE     
                                                                                   # 1.996372  0.9047591  1.525559

#-----------------------------------------------------------------------------
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model
model_cv <- train(sales ~ youtube + newspaper, data = marketing, method="lm",
                  trControl = train.control)

# Summarize the results
print(model_cv)

#----------------------------------------------------------------------------
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)

# Train the model
model_cv <- train(sales ~ facebook + newspaper, data = marketing, method="leapBackward",
                  trControl = train.control)

# Summarize the results
print(model_cv)


install.packages("elasticnet")
install.packages("neuralnet")
install.packages("kernelab")
install.packages("leaps")





#-----------------------------------------------------------------------------------------------------------------
################################################-- Best Model --##################################################
##################################################################################################################

# Randomly Split the data into training and test set
set.seed(123)
training.samples <- marketing$sales %>%
  createDataPartition(p = 0.75, list = FALSE)
train.data  <- marketing[training.samples, ]
test.data <- marketing[-training.samples, ]


?pairs

# Build model
ml_model <- lm(sales ~ . -newspaper, data = train.data)
summary(ml_model)

ml_model1 <- lm(sales ~ . , data = train.data)
summary(ml_model1)

anova(ml_model,ml_model1)

boxplot(marketing)
#-----------------------------------------------------------------------------------
library(MASS)
BC = boxcox(ml_model)
best.lm = BC$x[which(BC$y==max(BC$y))]
ml_model <- lm((sales^(0.14)) ~ log(youtube) + facebook , data = train.data)
plot( train.data$youtube , train.data$sales^0.7)

# boxTidwell(sales ~ youtube + facebook , data = train.data)
#-----------------------------------------------------------------------------------

# Plotting
par(mfrow=c(2,2))
plot(ml_model)
plot(ml_model,1)                                      # suggest y and x's are related non linearly.



# detecting multicollinearity
install.packages("car")
library(car)
car::vif(ml_model)                                    #-----> No multicollinearity
cor(train.data$youtube,train.data$facebook)
cor.test(train.data$youtube,train.data$facebook)      # correlation is not significant i.e. 0
plot(train.data$youtube,train.data$facebook)          # suggest predictors are independent


hist(ml_model$residuals)                              # errors are not normally distributed
# checking normality assumptions
ad.test(ml_model$residuals)                           # voilated
shapiro.test(ml_model$residuals)                      # voilated


# checking independency of errors
durbinWatsonTest(ml_model)                            # errors are independent


# Checking for heterosadasticity
library(lmtest)
# bptest can not be used due to non normality of errors
install.packages("olsrr")
library(olsrr)
ols_test_score(ml_model , rhs =F)                     # suggest homosadasticity
ols_test_f(ml_model)                                  # suggest homosadasticity


# Diagnostic metrics
library(broom)
mldm <- augment(ml_model)
plot(ml_model,4)                                      # No influential observation

# Making predictions
prediction <- ml_model %>% predict(test.data)
data.frame( R2 = R2(prediction, test.data$sales),
            RMSE = RMSE(prediction, test.data$sales),
            MAE = MAE(prediction, test.data$sales))                       # R2         RMSE       MAE
                                                                          # 0.9100358  1.736897   1.316125
#plot(test.data$sales , prediction)


#------------------------------------------------------------------------
plot(ml_model,4)
ad <- subset(as.data.frame(mldm),select = c(2,3,4), .cooksd < 0.2)

ml_model2 <- lm(ad$X.sales..0.14.. ~ ad$log.youtube. + ad$facebook)
hist(ml_model2$residuals)
adm <- augment(ml_model2)
shapiro.test(ml_model2$residuals)
plot(ml_model2)

bptest(ml_model2)
ols_test_f(ml_model2)




#######################################################################################################################

# No statistical proof of removing an observation which is outlier but not lie outside from cook's distance 0.5/1 
#roo <- subset(as.data.frame(mldm) , select = c(1,2,3) , abs(.std.resid) < 3 )
#sales <- roo$sales
#facebook <- roo$facebook
#youtube <- roo$youtube
#ml_model_1 <- lm(sales~youtube + facebook )


# Making predictions
#prediction23 <- ml_model_1 %>% predict(test.data)
#data.frame( R2 = R2(prediction23, test.data$sales),
#            RMSE = RMSE(prediction23, test.data$sales),
#            MAE = MAE(prediction23, test.data$sales)) 

#ad.test(ml_model_1$residuals)
#hist(ml_model_1$residuals)
#adm <- augment(ml_model_1)

######################################################################################################################

#-----------------------------------------------------------------------------------------------------------------
# checking whether newspaper must be included in the model or not 

### Residual vs explanatory variable plot
plot(train.data$newspaper,ml_model$residuals)  # shows newspaper have not useful information in linear form

### Partial regression plot
regr <- lm(train.data$newspaper~ train.data$youtube + train.data$facebook)

plot(regr$residuals , ml_model$residuals) # newspaper is not present in higher order or transformation terms in the model

# but what about interaction terms ?  ---> interaction term may increase the performance of model. It must be checked later.

######################################################################################################################

plot(train.data$youtube,train.data$sales)      # suggest a small curvilinear relationship and big linear relationship

plot(train.data$facebook,train.data$sales)     # suggest linear relationship

#ml_model1 <- lm(log(sales)~youtube + facebook , data = train.data)
#summary(ml_model1)
#par(mfrow=c(2,2))
#plot(ml_model1)


######################################################################################################################
plot(train.data$youtube,train.data$sales)      # suggest a small curvilinear relationship and big linear relationship

plot(train.data$facebook,train.data$sales)     # suggest linear relationship


#################################---polynomial order 3---##########################################################

# Build model
pol_1 <- lm(sales ~ facebook + poly(youtube,3,raw=T), data = train.data)

summary(pol_1)

# Plotting
plot(pol_1)

# Checking Normality of errors
hist(pol_1$residuals)                     # Left skewed
shapiro.test(pol_1$residuals)             # errors are not normally distributed 
ad.test(pol_1$residuals)                  # errors are normally distributed

# Checking idnependency of errors
durbinWatsonTest(pol_1)                   # errors are independent

# Checking hetrosadsticity
# bptest(pol_1)
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
data.frame( R2 = R2(pred, test.data$sales),
            RMSE = RMSE(pred, test.data$sales),
            MAE = MAE(pred, test.data$sales))                     # R2         RMSE       MAE
                                                                  # 0.9436412  1.386676   1.074112

# Detecting influential observation
plot(pol_1,4)

# Diagnostic metrics
pol_1_dm <- augment(pol_1)                                  # No influential observation



#########################################################################################################################

library(splines)

# Build the model
knots <- quantile(train.data$youtube, p = c(0.25, 0.5, .75))
spl_1 <- lm (sales ~ bs(youtube, knots = knots) + facebook, data = train.data)

# Make predictions
pred_spl <- spl_1 %>% predict(test.data)

# Model performance
data.frame(
  RMSE = RMSE(pred_spl, test.data$sales),
  R2 = R2(pred_spl, test.data$sales)
)


########################################################################################################################

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
bptest(gm)                                        # indicates homosadsticity   (doubt)
#ols_test_score(gm$residuals)  
#ols_test_f(gm$residuals)       all these tests can not be applied as it is suitable for lm() not for gam().


# Make predictions
predictions_gm <- gm %>% predict(test.data)
predictions_gm <- (predictions_gm)^(1/0.54)
# Model performance
data.frame(
  RMSE = RMSE(predictions_gm, test.data$sales),
  R2 = R2(predictions_gm, test.data$sales)
)                                                  #   RMSE        R2
                                                   # 1.380904   0.9443517    



#####car::vif(gm)(doubt)#######

# Checking influential point 
admd <- augment(gm)                                # No influential point


########################################################################################################################

#?train()
names(getModelInfo())
#range(train.data$youtube)
#range(train.data$facebook)
#range(train.data$newspaper)
#range(train.data$sales)

#######################################################################################################################

################################---- MLR with interaction----######################################################

#Build Model
#inter <- lm(train.data$sales ~ train.data$youtube*train.data$facebook*train.data$newspaper)
#summary(inter)      ----> indicates remove newspaper

# Build Model without newapaper
inter <- lm(sales ~ youtube*facebook ,data = train.data)
summary(inter)

#Plottig
plot(inter)

# Checking independency
durbinWatsonTest(inter)                 # Errors are independent

# Checking Normality
shapiro.test(inter$residuals)           # Errors are not normally distributed
ad.test(inter$residuals)                # Errors are not normally distributed

# Checking heterosadasticity
# bptest(inter)  we can't apply it due to non normality of errors
ols_test_score(inter)
ols_test_f(inter)                       # heterosadasticity

# checking multicollinerity
car::vif(inter)                         # multicollinearity (6.777438)

# Making predictions
predf <- inter %>% predict(test.data)
data.frame(
  RMSE = RMSE(predf, test.data$sales),
  R2 = R2(predf, test.data$sales)                      # RMSE        R2
                                                       # 0.9159955 0.9747544
)

lm(formula = )

# Checking influential observation
df <- augment(inter)
plot(inter , 4)                                       # One influential point




# Removing influential observation but this must be used when all other previous assumptions are already satisfied else don't use
inter_rip <- lm(sales ~ youtube*facebook)
summary(inter_rip)

predf2 <- inter_rip %>% predict(test.data)
data.frame(
  RMSE = RMSE(predf2, test.data$sales),
  R2 = R2(predf2, test.data$sales)                      # RMSE        R2
                                                        # 0.9039259   0.975153
)

plot(inter_rip)



# a <- train.data$youtube*train.data$facebook
# cor(a,train.data$facebook)

# cor(a,train.data$youtube)


######################################################################################################################
# Checking outliers
boxplot(marketing$youtube)
boxplot(marketing$facebook)
boxplot(marketing$newspaper)
boxplot(marketing$sales)
######################################################################################################################