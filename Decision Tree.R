install.packages("party")
library(party)
library(rpart)
data = read.csv(file.choose(),header = T)

set.seed(123)
pd <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
train <- data[pd==1, ]
test <- data[pd==2, ]

model = ctree(Sales ~ . , data = train , controls = ctree_control(minsplit = 20))
plot(model)

b= predict(model,train)
RMSE(b,train$Sales)
R2(b,train$Sales)


a = predict(model,test)
RMSE(a,test$Sales)
R2(a,test$Sales)


# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3 , search = "random"
)

# Train the model
model_cv <- train(Sales ~ ., data = data, method="glmnet",
                  trControl = train.control )

# Summarize the results
print(model_cv)

model_cv$bestTune[1,2]
