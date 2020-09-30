# Getting working directory
getwd()
# Setting working directory
setwd("D:\\Regression Model")
# Checking working directory
getwd()

# Importing data set from internal storage
data <- read.csv(file.choose(),header = T, sep = ",")

# View top 6 rows of "data"
head(data)

# Shape of data
dim(data)

# Name of columns of "data"
colnames(data)

# Data type of object data
typeof(data)       #### list
class(data)        #### dataframe
mode(data)         #### list


# data type of each column
typeof(data['car_ID'])       ### list
class(data['car_ID'])        ### dataframe  ----> This information is provided by str() command as below
mode(data['car_ID'])         ### list

typeof(data['car_ID'][1,])   ### integer  
class(data['car_ID'])        ### integer    ----> This information is provided by str() command as below
mode(data['car_ID'])         ### numeric

typeof(data['CarName'][1,])  ### integer
class(data['CarName'][1,])   ### factor     ----> This information is provided by str() command as below
mode(data['CarName'][1,])    ### numeric


# Getting structure (describes class and dimension of "data" as well as class of different columns of "data" ) of data
str(data)

# Decide how many categorical and/or quantitative variables/features are in object "data"
#  Quantitative Variables - (wheelbase, carlength, carwidth, carheight, curbweight, enginesize, boreratio, stroke, 
#                            compressionratio, horsepower, peakrpm, citympg, highwaympg, price)
#  Qualitative Variables  - (car_ID, symboling, CarName, fueltype,aspiration, doornumber, carbody, drivewheel, 
#                            enginelocation, enginetype, cylindernumber, fuelsystem)

# Dropping irrelevant columns from object "data"
data1 <- data[,c(-1,-3)]
head(data1)

# installing and loading libraries
install.packages("dummies")
library(dummies)

# Converting categorical to quantitative features (creating dummy variables)
dum_symb              <-  as.data.frame(dummies::dummy(data = data1 , x = "symboling"))
dum_fuel              <-  as.data.frame(dummies::dummy(data = data1 , x = "fueltype"))
dum_asp               <-  as.data.frame(dummies::dummy(data = data1 , x = "aspiration"))
dum_door              <-  as.data.frame(dummies::dummy(data = data1 , x = "doornumber"))
dum_carbody           <-  as.data.frame(dummies::dummy(data = data1 , x = "carbody"))
dum_drive             <-  as.data.frame(dummies::dummy(data = data1 , x = "drivewheel"))
dum_enginelocation    <-  as.data.frame(dummies::dummy(data = data1 , x = "enginelocation"))
dum_enginetype        <-  as.data.frame(dummies::dummy(data = data1 , x = "enginetype"))
dum_cylinder          <-  as.data.frame(dummies::dummy(data = data1 , x = "cylindernumber"))
dum_fuelsystem        <-  as.data.frame(dummies::dummy(data = data1 , x = "fuelsystem"))

# Combining all data frames in object "data1"
data1 <- cbind.data.frame(data1,dum_symb,dum_fuel,dum_asp,dum_door,dum_carbody,dum_drive,dum_enginelocation,dum_enginetype,dum_cylinder,dum_fuelsystem )

# Dropping unuseful columns from object "data1"
data1 <- data1[, c(-1,-2,-3,-4,-5,-6,-7,-13,-14,-16)]

# Checking missing data
table(is.na(data1))

# Normalizing the data set stored in object "data1" for valid comparisons
install.packages("scales")
library(scales)

 for(i in 1:58) { 
                    data1[,i] <- rescale(data1[,i], to = c(0,1))
                } 

# Saving the object data1 in a csv file for further use.
write.csv(data1, "clean_data.csv")

# Load clean data set
data <- read.csv(file.choose(),header = T)


# What is target -----"price" which is continuous variable, hence --> Use concept of "Multiple linear Regression Analysis" to form a model for prediction.
# Checking normality assumption of target variable 
hist(price)

model_1 <- lm(data,subset=0.8)
?lm()
summary(model_1)

attach(data)
model_1 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                boreratio + stroke + compressionratio + horsepower + peakrpm + 
                citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                symboling1 + symboling2 + symboling3 + fueltypediesel + fueltypegas + 
                aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                carbodyconvertible + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheel4wd + drivewheelfwd + 
                drivewheelrwd + enginelocationfront + enginelocationrear + 
                enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                fuelsystem1bbl + fuelsystem2bbl + fuelsystem4bbl + fuelsystemidi + 
                fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi + fuelsystemspfi)

summary(model_1)

model_2 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                boreratio + stroke + compressionratio + horsepower + peakrpm + 
                citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                symboling1 + symboling2 + symboling3 + fueltypediesel + fueltypegas + 
                aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                carbodyconvertible +  carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheel4wd + drivewheelfwd + 
                drivewheelrwd + enginelocationfront + enginelocationrear + 
                enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                fuelsystem1bbl + fuelsystem2bbl + fuelsystem4bbl + fuelsystemidi + 
                fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi + fuelsystemspfi)
summary(model_2)

model_3 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                boreratio + stroke + compressionratio + horsepower + peakrpm + 
                citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                symboling1 + symboling2 + symboling3 + fueltypediesel + fueltypegas + 
                aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                carbodyconvertible +  carbodyhatchback + 
                carbodysedan  + drivewheel4wd + drivewheelfwd + 
                drivewheelrwd + enginelocationfront + enginelocationrear + 
                enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                fuelsystem1bbl + fuelsystem2bbl + fuelsystem4bbl + fuelsystemidi + 
                fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi + fuelsystemspfi)

summary(model_3)

model_4 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                boreratio + stroke + compressionratio + horsepower + peakrpm + 
                citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                symboling1 + symboling2 + symboling3 + fueltypediesel + fueltypegas + 
                aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                carbodyconvertible +  carbodyhatchback + 
                carbodysedan  + drivewheel4wd + drivewheelfwd + 
                drivewheelrwd + enginelocationfront + enginelocationrear + 
                enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                fuelsystem1bbl + fuelsystem2bbl  + fuelsystemidi + 
                fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi + fuelsystemspfi)

summary(model_4)

model_5 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                boreratio + stroke + compressionratio + horsepower + peakrpm + 
                citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                symboling1 + symboling2 + symboling3 + fueltypediesel + fueltypegas + 
                aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                carbodyconvertible +  carbodyhatchback + 
                carbodysedan  + drivewheel4wd + drivewheelfwd + 
                drivewheelrwd + enginelocationfront + enginelocationrear + 
                enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                fuelsystem1bbl + fuelsystem2bbl  + fuelsystemidi + 
                fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi)

summary(model_5)


model_6 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                boreratio + stroke + compressionratio + horsepower + peakrpm + 
                citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                symboling1 + symboling2 + symboling3 + fueltypediesel + fueltypegas + 
                aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                carbodyconvertible +  carbodyhatchback + 
                carbodysedan  + drivewheel4wd + drivewheelfwd + 
                drivewheelrwd + enginelocationfront + enginelocationrear + 
                enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                cylindernumberfive  + cylindernumbersix + 
                cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                fuelsystem1bbl + fuelsystem2bbl  + fuelsystemidi + 
                fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi)

summary(model_6)

model_7 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                boreratio + stroke + compressionratio + horsepower + peakrpm + 
                citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                symboling1 + symboling2 + symboling3 + fueltypediesel + fueltypegas + 
                aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                carbodyconvertible +  carbodyhatchback + 
                carbodysedan  + drivewheel4wd + drivewheelfwd + 
                drivewheelrwd + enginelocationfront + enginelocationrear + 
                enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                cylindernumberfive  + cylindernumbersix + 
                cylindernumberthree  + cylindernumbertwo + 
                fuelsystem1bbl + fuelsystem2bbl  + fuelsystemidi + 
                fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi)

summary(model_7)

model_8 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                boreratio + stroke + compressionratio + horsepower + peakrpm + 
                citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                symboling1 + symboling2 + symboling3 + fueltypediesel + fueltypegas + 
                aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                carbodyconvertible +  
                carbodysedan  + drivewheel4wd + drivewheelfwd + 
                drivewheelrwd + enginelocationfront + enginelocationrear + 
                enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                cylindernumberfive  + cylindernumbersix + 
                cylindernumberthree  + cylindernumbertwo + 
                fuelsystem1bbl + fuelsystem2bbl  + fuelsystemidi + 
                fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi)

summary(model_8)

model_9 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                boreratio + stroke + compressionratio + horsepower + peakrpm + 
                citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                symboling1 + symboling2 + symboling3 + fueltypediesel + fueltypegas + 
                aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                carbodyconvertible +  
                carbodysedan  + drivewheel4wd + drivewheelfwd + 
                drivewheelrwd + enginelocationfront + enginelocationrear + 
                enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                  cylindernumbersix + 
                cylindernumberthree  + cylindernumbertwo + 
                fuelsystem1bbl + fuelsystem2bbl  + fuelsystemidi + 
                fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi)

summary(model_9)

model_10 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                boreratio + stroke + compressionratio + horsepower + peakrpm + 
                citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                symboling1  + symboling3 + fueltypediesel + fueltypegas + 
                aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                carbodyconvertible +  
                carbodysedan  + drivewheel4wd + drivewheelfwd + 
                drivewheelrwd + enginelocationfront + enginelocationrear + 
                enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                cylindernumbersix + 
                cylindernumberthree  + cylindernumbertwo + 
                fuelsystem1bbl + fuelsystem2bbl  + fuelsystemidi + 
                fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi)

summary(model_10)

model_11 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                 boreratio + stroke + compressionratio + horsepower + peakrpm + 
                 citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                 symboling1   + fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystem1bbl + fuelsystem2bbl  + fuelsystemidi + 
                 fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi)

summary(model_11)

model_12 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                 boreratio + stroke + compressionratio + horsepower + peakrpm + 
                 citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                 symboling1   + fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystem1bbl   + fuelsystemidi + 
                 fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi)

summary(model_12)

model_13 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                 boreratio + stroke + compressionratio + horsepower + peakrpm + 
                 citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                 symboling1   + fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                     fuelsystemidi + 
                 fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi)

summary(model_13)

model_14 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                 boreratio + stroke + compressionratio + horsepower + peakrpm + 
                 citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                 symboling1   + fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo + doornumberfour + doornumbertwo + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_14)

model_15 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                 boreratio + stroke + compressionratio + horsepower + peakrpm + 
                 citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                 symboling1   + fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo +  doornumbertwo + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_15)

model_16 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                 boreratio + stroke + compressionratio + horsepower + peakrpm + 
                 citympg + highwaympg + symboling.2 + symboling.1 + symboling0 + 
                 symboling1   + fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_16)

model_17 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                 boreratio + stroke + compressionratio + horsepower + peakrpm + 
                  highwaympg + symboling.2 + symboling.1 + symboling0 + 
                 symboling1   + fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_17)

model_18 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                 boreratio + stroke + compressionratio + horsepower + peakrpm + 
                 highwaympg + symboling.2 + symboling.1  + 
                 symboling1   + fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_18)

model_19 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                 boreratio + stroke + compressionratio + horsepower + peakrpm + 
                 highwaympg + symboling.2 + symboling.1  + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_19)

model_20 <- lm(price ~ wheelbase + carlength + carwidth + carheight + curbweight + enginesize + 
                 boreratio + stroke + compressionratio +  peakrpm + 
                 highwaympg + symboling.2 + symboling.1  + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_20)

model_21 <- lm(price ~  carlength + carwidth + carheight + curbweight + enginesize + 
                 boreratio + stroke + compressionratio +  peakrpm + 
                 highwaympg + symboling.2 + symboling.1  + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_21)

model_22 <- lm(price ~  carlength + carwidth + carheight + curbweight + enginesize + 
                 boreratio + stroke + compressionratio +  peakrpm + 
                  symboling.2 + symboling.1  + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_22)

model_23 <- lm(price ~  carlength + carwidth + carheight + curbweight + enginesize + 
                   stroke + compressionratio +  peakrpm + 
                 symboling.2 + symboling.1  + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_23)

model_24 <- lm(price ~  carlength + carwidth +  curbweight + enginesize + 
                 stroke + compressionratio +  peakrpm + 
                 symboling.2 + symboling.1  + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan  + drivewheel4wd + drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_24)

model_25 <- lm(price ~  carlength + carwidth +  curbweight + enginesize + 
                 stroke + compressionratio +  peakrpm + 
                 symboling.2 + symboling.1  + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan  +  drivewheelfwd + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_25)

model_26 <- lm(price ~  carlength + carwidth +  curbweight + enginesize + 
                 stroke + compressionratio +  peakrpm + 
                 symboling.2 + symboling.1  + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan   + 
                 drivewheelrwd + enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_26)

model_27 <- lm(price ~  carlength + carwidth +  curbweight + enginesize + 
                 stroke + compressionratio +  peakrpm + 
                 symboling.2 + symboling.1  + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan   + 
                  enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_27)


model_28 <- lm(price ~  carlength + carwidth +  curbweight + enginesize + 
                 stroke + compressionratio +  peakrpm + 
                 symboling.2   + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan   + 
                 enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemmfi  + fuelsystemspdi)

summary(model_28)

model_29 <- lm(price ~  carlength + carwidth +  curbweight + enginesize + 
                 stroke + compressionratio +  peakrpm + 
                 symboling.2   + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan   + 
                 enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemspdi)

summary(model_29)

model_30 <- lm(price ~   carwidth +  curbweight + enginesize + 
                 stroke + compressionratio +  peakrpm + 
                 symboling.2   + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan   + 
                 enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemspdi)

summary(model_30)

model_31 <- lm(price ~   carwidth +  curbweight + enginesize + 
                 stroke +   peakrpm + 
                 symboling.2   + 
                 fueltypediesel + fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan   + 
                 enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemspdi)

summary(model_31)

model_32 <- lm(price ~   carwidth +  curbweight + enginesize + 
                 stroke +   peakrpm + 
                 symboling.2   + 
                 fueltypegas + 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan   + 
                 enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemspdi)

summary(model_32)

model_33 <- lm(price ~   carwidth +  curbweight + enginesize + 
                 stroke +   peakrpm + 
                 symboling.2   + 
           
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan   + 
                 enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 fuelsystemidi + 
                 fuelsystemspdi)

summary(model_33)

model_34 <- lm(price ~   carwidth +  curbweight + enginesize + 
                 stroke +   peakrpm + 
                 symboling.2   + 
                 
                 aspirationstd + aspirationturbo  + 
                 carbodyconvertible +  
                 carbodysedan   + 
                 enginelocationfront + enginelocationrear + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv + enginetyperotor + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  + cylindernumbertwo + 
                 
                 fuelsystemspdi)

summary(model_34)

model_35 <- lm(price ~   carwidth +  curbweight + enginesize + 
                 stroke +   peakrpm + 
                 symboling.2   + 
                 
                 aspirationstd +  
                 carbodyconvertible +  
                 carbodysedan   + 
                 enginelocationfront  + 
                 enginetypedohc + enginetypedohcv + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetypeohcv  + cylindernumbereight + 
                 cylindernumbersix + 
                 cylindernumberthree  +  
                 
                 fuelsystemspdi)

summary(model_35)

hist(data$curbweight)


# Train test split
?sample
?setdiff
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
test_index  <- setdiff(1:nrow(data), train_index)
X_train <- data[train_index, -1]
y_train <- data[train_index, "price"]

X_test <- data[test_index, -1]
y_test <- data[test_index, "price"]

# combining y_train and x_train and renaming the first column of data1 to price
data1 = cbind.data.frame(y_train , X_train)
names(data1)[1] <- "price"




install.packages("Hmisc")
library("Hmisc")
a <- rcorr(as.matrix(data))

cor(s.matrix(data$price),as.matrix(data$carbody))
sd(as.numeric(data$carbodyconvertible))




