#install.packages("lattice")
#library("lattice")
# install.packages("dplyr")
# library("dplyr")
# install.packages('party')
# library('party')
# install.packages("glmmLasso")
# library("glmmLasso")



# First I have uploaded the training and the test file into the R environment.
trainFrame <- read.csv("train.csv",sep = ",",header = TRUE)
testFrame  <- read.csv("test.csv",sep = ",", header = TRUE)
#testResult  <- read.csv("test.csv",sep = ",", header = TRUE)
#--------------------Insight of the dataset-------------------------
# datetime - hourly date + timestamp  
# season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
# holiday - whether the day is considered a holiday
# workingday - whether the day is neither a weekend nor holiday
# weather - 1: Clear, Few clouds, Partly cloudy, Partly cloudy 
# 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 
# 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds 
# 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
# temp - temperature in Celsius
# atemp - "feels like" temperature in Celsius
# humidity - relative humidity
# windspeed - wind speed
# casual - number of non-registered user rentals initiated
# registered - number of registered user rentals initiated
# count - number of total rentals

# 1. Season can be used categorical data.
# 2. Holiday can be also reated as categorical data with 0 being regular
#    day and 1 being treated as a holiday.
# 3. workingday this can be also treated as a categorical ddata.
# 4. Weather can be also factored and treated like categorical data.
# 5. Temp is the outside temp in Celsius.(For now taken as it is)
# 6. Feels like temp(For now taken as it is)
# 7. Humidity (For now taken as it is)
# 8. Windspeed is the real number representing the windspeed
# 9. Casuals number of non-registered users from 0 to 367
# 10. Registeted number of registered users from 0 to 886
# 11. Count this will be the response variable. 
# 12. Data which has the time and hour we can convert into timestamp.

#-------------------------Pre-processing------------------------------
# Factoring of the variables.
# Now after that we have figured that what we can change in the
# data set we can actaully start working on the preprocessing phase.
# 1. Factor the variables such as season,Holiday,workingday,weather
trainFrame$season <- factor(trainFrame$season)
trainFrame$holiday <- factor(trainFrame$holiday)
trainFrame$workingday <- factor(trainFrame$workingday)
trainFrame$weather <- factor(trainFrame$weather)

testFrame$season <- factor(testFrame$season)
testFrame$holiday <- factor(testFrame$holiday)
testFrame$workingday <- factor(testFrame$workingday)
testFrame$weather <- factor(testFrame$weather)
# 2. Now we need to transform the datetime column.
#    We could have used to convert this into timestamp values. 
#    Which would have been much better way but as we have to train our
#    dataset with respect to the count of bicycles for a day and that too 
#    for an perticular time we need to substring the variable
# Thus we will first seperate the date and the time values
# Creating a new column in the training frame
trainFrame$timeB <- substring(trainFrame$datetime,12,20)
trainFrame$timeB <- factor(trainFrame$timeB)

testFrame$timeB <- substring(testFrame$datetime,12,20)
testFrame$timeB <- factor(testFrame$timeB)
# 3. Now we can plan to change the datetime column into a more refined 
# one. By changing the values to Day name with this we can easily create
# categories and work in 7 days value which will be treated as a factor.
trainFrame$dayDate <- weekdays(as.Date(trainFrame$datetime))
trainFrame$dayDate <- as.factor(trainFrame$dayDate)

testFrame$dayDate <- weekdays(as.Date(testFrame$datetime))
testFrame$dayDate <- as.factor(testFrame$dayDate)
# 4.Now as some of the values are termed to be zero I seek to scale
# the variables.
trainFrame$temp <- scale(trainFrame$temp,center = TRUE,scale = TRUE)
trainFrame$atemp <- scale(trainFrame$atemp,center = TRUE,scale = TRUE)
trainFrame$humidity <- scale(trainFrame$humidity,center = TRUE,scale = TRUE)
trainFrame$windspeed <- scale(trainFrame$windspeed,center = TRUE,scale = TRUE)

testFrame$temp <- scale(testFrame$temp,center = TRUE,scale = TRUE)
testFrame$atemp <- scale(testFrame$atemp,center = TRUE,scale = TRUE)
testFrame$humidity <- scale(testFrame$humidity,center = TRUE,scale = TRUE)
testFrame$windspeed <- scale(testFrame$windspeed,center = TRUE,scale = TRUE)
# 4. After Analysing the plots and finding the aggregate value of count
# for each day we can seperate the weekend sunday form the other days 
# treat this as a categorical variable.
# We shall give the value as 1 for sunday and 0 for anyother weekday
trainFrame$sunday[trainFrame$dayDate == "Sunday"] <- 1
trainFrame$sunday[trainFrame$dayDate != "Sunday"] <- 0
testFrame$sunday[testFrame$dayDate == "Sunday"] <- 1

testFrame$sunday[testFrame$dayDate != "Sunday"] <- 0
trainFrame$sunday <- as.factor(trainFrame$sunday)
testFrame$sunday <- as.factor(testFrame$sunday)

# 5. Now I wish to convert the time column into a more refined 
# way. I wish to convert this too into a factor variable
# As we are working with time between 0 - 23 we can assigne different 
# values depending upon the days.
trainFrame$hour <- as.numeric(substr(trainFrame$timeB,1,2))
testFrame$hour <- as.numeric(substr(testFrame$timeB,1,2))
# 6. Creating a new column and assigning the which hour comes
# in which part of the cycle.
# Default value as 4
trainFrame$dayZone <- 4

testFrame$dayZone <- 4
# Assigning numeric values for different day zones
trainFrame$dayZone[(trainFrame$hour < 10) & (trainFrame$hour > 3)] <- 1
trainFrame$dayZone[(trainFrame$hour < 16) & (trainFrame$hour > 9)] <- 2
trainFrame$dayZone[(trainFrame$hour < 22) & (trainFrame$hour > 15)] <- 3
trainFrame$dayZone <- as.factor(trainFrame$dayZone)
trainFrame$hour <- as.factor(trainFrame$hour)

testFrame$dayZone[(testFrame$hour < 10) & (testFrame$hour > 3)] <- 1
testFrame$dayZone[(testFrame$hour < 16) & (testFrame$hour > 9)] <- 2
testFrame$dayZone[(testFrame$hour < 22) & (testFrame$hour > 15)] <- 3
testFrame$dayZone <- as.factor(testFrame$dayZone)
testFrame$hour <- as.factor(testFrame$hour)
#============================STEPS====================================
# Now we will work on the model fitting part of the problem 
# In this we will run through the following steps
# 1. Find the preditable variables which are of our interst
# 2. Fit a lasso or ridge regression model to find the best possible
#    predictable variables togeather.
# 3. Use cross validation techniques to validate the result.
# 4. Work on the model fitting using all the variabls needed.
# 5. For this case we will work on posssion regression model
# 6. Find any possible outliers in the model.
# 7. Plot the graphs which are necessary.
# 8. Work on the test set and find the corresponding results.
#=======================Model With cTree=======================
# For Start I am working on ctree The core of the package is ctree(),
# an implementation of conditional inference trees which 
# embed tree-structured regression models into a well defined theory of 
# conditional inference procedures.
# This to get an initial prediction values just to find out the 
# that the preprocessing part done was good or not.

fit1 <- ctree(count ~ season + holiday + workingday + weather + temp + 
                atemp + humidity + hour + dayZone + sunday, 
              data=trainFrame)
# Finding the predicted values by fitting the model with the test set
predict <- round(predict(fit1, testFrame))
# Creating a result data frame which will store the values of the 
# date and count values.
result.ctree <- data.frame(datetime = testFrame$datetime, count=predict)
# Writing the files.
# write.csv(result.ctree,file="Result_cTree.csv",row.names=FALSE)
#=======================Model With Poission=========================
# I have tried fitting the Poission distribution into the model
# because of the response variable being count.
# I achieved evidence of overdispesion in the model and thereofore
# tried to reduce overdispersion by multiplying and dividing by the 
# dispersion parameter.
#=======================Model With QuasiPoission======================
# To find the dispersion parameter I used the quassipoission family
# and plotted a model on that family.
#=======================Model With Weighted_R======================
# Looking at the plot for the fitted model I can see the evidence of
# non-constant error distibution and thus tried weighted regresison 
# model.
#===================Model With Negative_Binomial======================
# After applying the possion ditribution the model result was not close 
# the Google Prediction API result.
# Therefore I applied Negative Binomial Distribution.
# Fitted the model using NB and predicting the values from the test set.
#=======================Model With glmmLasso=========================

# fit.glmmLasso <- glmmLasso(fix = count ~ season + holiday + workingday + weather + temp + 
#                              atemp + humidity + hour + dayZone + sunday,
#                            rnd = rnd.formula,
#                            data = trainFrame,
#                            lambda = 10,
#                            family = poisson(link = "log"),
#                            switch.NR = TRUE,
#                            final.re = TRUE)
# summary(fit.glmmLasso)
# predict.glmm <- predict(fit.glmmLasso, testFrame)
# result.ctree <- data.frame(datetime = testFrame$datetime, count=predict)

#=====================================================================
#============================FUTURE WORK==============================
#=====================================================================
# Given the time frame I have achieved a considerable accuracy in the
# training model.
# As you can see that I have not incorporated any plots which can be 
# considered for the next model building phase.
# 
# I have tried and applied the Poission and Negative Binomial 
# distribution for the training model and find out the predicted values
# but with warning messages(If you need the code I can share)

# For future I wish to perform the following steps:-
# 1. Preprocessing the data.
# 2. Use subset selection method or PCA(principal component analysis)
# to get some better information about the data.
# 3. Performing Bootstrap method to get a the CI for the coefficients.
# 4. Using crossvalidation techniques to validate the model.
# 5. Understanding the data and use count of a particular hour
# as the exposure(offset) in poission distribution.



























































































