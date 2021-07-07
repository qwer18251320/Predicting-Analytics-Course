# Hw2: Chia-Jo Chen

# 1. Perform all necessary cleaning and transformation of 
#     the data to make it useful for regression modeling.  
#     Hint:  It might be easier in some steps to set up the 
#     qualitative variables in the data set as factors rather 
#     than to create individual indicator variables across 
#     the board (use the “as.factor()” function).  
#     This is because there are many levels for some of 
#     the qualitative variables in the data set.  
#     However, the choice of set up is up to each individual student.  
#     See included script at the end of this assignment.  

#  Create two data sets from the original for analysis
#
dat <- read.csv("hw2_hour.csv")
str(dat)
names(dat)
#
#  Create dat2 – with indicator variables
mnth <- as.factor(dat$mnth)
season <- as.factor(dat$season)
hr <- as.factor(dat$hr)
wkday <- as.factor(dat$wkday)
weathersit <- as.factor(dat$weathersit)
tmp_mnth <- data.frame(model.matrix(~mnth-1))
tmp_season <- data.frame(model.matrix(~season-1))
tmp_hr <- data.frame(model.matrix(~hr-1))
tmp_wkday <- data.frame(model.matrix(~wkday-1))
tmp_weathersit <- data.frame(model.matrix(~weathersit-1))
dat2 <- cbind(dat[,c(15,1,4)], tmp_season[,1:3], 
              tmp_mnth[,1:11], dat[,c(7,9)], 
              tmp_wkday[,1:6], tmp_hr[,1:23], 
              tmp_weathersit[,2:4], dat[,11:14])
rm(mnth, season, hr, wkday, weathersit)
rm(tmp_mnth, tmp_season, tmp_hr, tmp_wkday)
rm(tmp_weathersit)
str(dat2)





# 2. Compute the correlation between all independent variables 
#     and the response.  List the top 3 (or more) correlated variables.  
#     Explain why these make sense or why they are surprising.  
#     Hint:  The easiest way to perform this step is to use 
#     the original data set without the date column.

# Remove the variable "date"
data <- dat[c(3:15)]
str(data)

# Correlation
library(corrplot)
correlation <- cor(data)
correlation

corrplot(correlation, method = "circle", type = "lower", 
         tl.col = "black", tl.cex = 0.6)

#   Interpretation:
#     The top 3 correlated variables have a higher correlation 
#     with the response "cnt" variable are the following:
#       1. temp: 0.40477228
#       2. atemp: 0.40092930
#       3. hr: 0.39407150
#   It is reasonable that temperature, feeling temperature, 
#   and hour have a higher correlation with the rental count 
#   since whether the weather is too cool or too hot would affect  
#   the willingness to ride a bike; on the contrary, the weather 
#   of warm may prompt people to actively participate in outdoor 
#   activity, which further turns to an increase in rental 
#   count. Surprisingly, the correlation between rental count 
#   and the variables of holiday, weekday, and workday is nearly 0.






# 3. Using the seasonal variables, the hourly indicators, and 
#    the weather variables, construct a linear regression model 
#    predicting the rental count (cnt) using this set of variables.  
#    Interpret the coefficient of determination.  
#    Which variables are significant at the 5% level?

# subset the required variables, named newdata_1
str(dat2)
newdata_1 <- dat2[c(1,4:6,26:55)]
str(newdata_1)

reg.1 <- lm(cnt ~ ., data = newdata_1)
reg.1
summary(reg.1)

#   Interpretation for Estimated Regression:
#     Rental count (cnt) = 69.519 + (-62.348)*season1 
#       + (-26.551)*season2 + (-45.673)*season3 
#       + (-31.293)*hr0 + (-48.091)*hr1 + (-56.227)*hr2 
#       + (-66.252)*hr3 + (-68.314)*hr4 + (-52.036)*hr5 
#       + 140.715*hr7 + 279.962*hr8 + 130.527*hr9 + 74.039*hr10
#       + 97.624*hr11 + 135.411*hr12 + 129.463*hr13 + 113.023*hr14 
#       + 122.297*hr15 + 184.710*hr16 + 339.198*hr17 + 308.301*hr18
#       + 201.119*hr19 + 122.729*hr20 + 74.311*hr21 + 38.190*hr22
#       + (-5.756)*weathersit2 + (-60.245)*weathersit3 + 192.291*temp
#       + 94.438*atemp + (-102.320)*hum + (-40.439)*windspeed

#   All variables, except "hr6" and "weathersit4", are significant 
#   at the 5% level because the p-value is smaller than 0.05.

#   Interpretation for R-squared: 
#     The R-squared (multiple r-squared) is 0.6261.
#     Interpretation of R-squared: 62.61% of the variation in the 
#     rental count is explained by variables.

#   Interpret the coefficient of determination.
#     For each additional season1, rental count decreases by 62.348
#     For each additional season2, rental count decreases by 26.551
#     For each additional season3, rental count decreases by 45.673
#     For each additional hr0, rental count decreases by 31.293
#     For each additional hr1, rental count decreases by 48.091
#     For each additional hr2, rental count decreases by 56.227
#     For each additional hr3, rental count decreases by 66.252
#     For each additional hr4, rental count decreases by 68.314
#     For each additional hr5, rental count decreases by 52.036
#     For each additional hr7, rental count increases by 140.715
#     For each additional hr8, rental count increases by 279.962
#     For each additional hr9, rental count increases by 130.527
#     For each additional hr10, rental count increases by 74.039
#     For each additional hr11, rental count increases by 97.624
#     For each additional hr12, rental count increases by 135.411
#     For each additional hr13, rental count increases by 129.463
#     For each additional hr14, rental count increases by 113.023
#     For each additional hr15, rental count increases by 122.297
#     For each additional hr16, rental count increases by 184.710 
#     For each additional hr17, rental count increases by 339.198
#     For each additional hr18, rental count increases by 308.301
#     For each additional hr19, rental count increases by 201.119
#     For each additional hr20, rental count increases by 122.729
#     For each additional hr21, rental count increases by 74.311
#     For each additional hr22, rental count increases by 38.190
#     For each additional weathersit2, rental count decreases by 5.756
#     For each additional weathersit3, rental count decreases by 60.245
#     For each additional weathersit4, rental count decreases by 36.057
#     For each additional temp, rental count increases by 192.291     
#     For each additional atemp, rental count increases by 94.438
#     For each additional hum, rental count decreases by 102.320
#     For each additional windspeed, rental count decreases by 40.439






# 4. Choose preferred linear regression model predicting 
#    the rental count (cnt) using a subset of the variables 
#    described in the prior question.  
#    Justify the model choice by using the nested F-test to 
#    compare the chosen model in this question to the model 
#    fit in prior question.  
#    Interpret the result of the hypothesis test.   

# Subset the variables, which is significant from Q3, named newdata_2
str(newdata_1)
newdata_2 <- newdata_1[c(1:10,12:29,31:34)]
str(newdata_2)

reg.2 <- lm(cnt ~ ., data = newdata_2)
reg.2
summary(reg.2)

#   Interpretation for Estimated Regression:
#    Rental count (cnt) = 72.655 + (-62.402)*season1 
#      + (-26.489)*season2 + (-45.551)*season3 
#      + (-34.478)*hr0 + (-51.332)*hr1 + (-59.425)*hr2 
#      + (-69.456)*hr3 + (-71.525)*hr4 + (-55.250)*hr5 
#      + 137.509*hr7 + 276.768*hr8 + 127.354*hr9 + 70.889*hr10
#      + 94.495*hr11 + 132.301*hr12 + 126.367*hr13 + 109.935*hr14 
#      + 119.213*hr15 + 181.575*hr16 + 336.104*hr17 + 305.145*hr18
#      + 197.993*hr19 + 119.591*hr20 + 71.157*hr21 + 35.025*hr22
#      + (-5.742)*weathersit2 + (-60.343)*weathersit3 + 192.062*temp
#      + 94.237*atemp + (-102.014)*hum + (-40.410)*windspeed

#   Interpretation for R-squared: 
#     The R-squared (multiple r-squared) is 0.6261
#     Interpretation of R-squared: 62.61% of the variation in the 
#     rental count is explained by variables.

# The Nested-Models F-Test
anova(reg.1, reg.2)

#   Analysis of Variance Table
#    Model 1: cnt ~ season1 + season2 + season3 + hr0 + hr1 + hr2 + hr3 + 
#    hr4 + hr5 + hr6 + hr7 + hr8 + hr9 + hr10 + hr11 + hr12 + hr13 + 
#    hr14 + hr15 + hr16 + hr17 + hr18 + hr19 + hr20 + hr21 + hr22 + 
#    weathersit2 + weathersit3 + weathersit4 + temp + atemp + hum + windspeed

#    Model 2: cnt ~ season1 + season2 + season3 + hr0 + hr1 + hr2 + hr3 + 
#    hr4 + hr5 + hr7 + hr8 + hr9 + hr10 + hr11 + hr12 + hr13 + hr14 + 
#    hr15 + hr16 + hr17 + hr18 + hr19 + hr20 + hr21 + hr22 + 
#    weathersit2 + weathersit3 + temp + atemp + hum + windspeed

#    Res.Df        RSS       Df      Sum of Sq      F Pr(>F)
# 1  17345     213760547                           
# 2  17347     213779250     -2    -18703 0.7588     0.4682


#####   Interpretation the result of the hypothesis test   #####
#     Hypothesis 
#       Ho:  Models are same
#       Ha:  Model with more variables is better
#
#   P-value is 0.46982 greater than alpha, 0.05, so fail to reject 
#   Ho because the models are the same. And we do not have evidence 
#   to prove that the big model is better than the small model. 
#   For both models(reg.1 and reg.2), the R-squared is 62.61%. 
#   Therefore, the small model (reg.2) is as good at explaining 
#   the rental count for model fit.






# 5. Using the entire data set, chose a preferred model 
#    for predicting rentals. ustify the model choice both 
#    using metrics for measuring model quality and practically. 

# Subset the all data, and remove the variable of "obs," named newdata_3
newdata_3 <- dat2[c(1,3:55)]

reg.3_all <- lm(cnt ~ ., data = newdata_3)
reg.3_all
summary(reg.3_all)

#   Interpretation for the model of reg.3_all:
#     As we can see that the p-value of the following variables 
#     "mnth1", "mnth2", "mnth4", "mnth6", "mnth7", "mnth11", 
#     "workday", "wkday3", "wkday4", "wkday5", "hr6 ,"weathersit4,"
#     are not significant at the 95% of confident level since 
#     the p-value is larger than 0.05.
#     The R-squared is 68.64% of the variation in the rental 
#     count is explained by variables.

# Run linear regression without non-significant variables from reg.all
reg.4_all <- lm(cnt ~ . - mnth1 - mnth2 - mnth4 - mnth6 - mnth7
                - mnth11 - workday - wkday3 - wkday4 - wkday5 - hr6 
                - weathersit4, data = newdata_3)
summary(reg.4_all)

#   Interpretation for the model of reg.4_all:
#     As we can see that the p-value of the variable of "wkday2"
#     is not significant at the 95% of confident level since 
#     the p-value, 0.0708, is larger than 0.05. 
#     The R-squared is 68.6% of the variation in the rental 
#     count is explained by variables.

# Run linear regression without non-significant variables from reg.all_3
reg.5_all <- lm(cnt ~ . - mnth1 - mnth2 - mnth4 - mnth6 - mnth7
                - mnth11 - workday - wkday3 - wkday4 - wkday5 - hr6 
                - weathersit4 - wkday2, data = newdata_3)
summary(reg.5_all)

#####   Interpretation for the model of reg.all_3   #####
#   From the reg.all_3, we got a preferred model for predicting 
#   rentals count with the following Estimated Regression:
#     Rental count (cnt) = 25.347+ 85.365*yr + (-58.730)*season1 
#       + (-15.851)*season2 + (-36.179)*season3 + 11.634*mnth3
#       + 15.031*mnth5 + 16.356*mnth8 + 41.007*mnth9 + 24.347*mnth10
#       + (-27.535)*holiday + (-14.409)*wkday0 + (-4.955)*wkday1 
#       + (-33.772)*hr0 + (-51.126)*hr1 + (-60.105)*hr2 
#       + (-70.756)*hr3 + (-73.954)*hr4 + (-57.193)*hr5 
#       + 136.713*hr7 + 277.048*hr8 + 129.270*hr9 + 74.527*hr10
#       + 99.845*hr11 + 139.075*hr12 + 133.987*hr13 + 118.107*hr14 
#       + 127.556*hr15 + 189.600*hr16 + 343.419*hr17 + 311.429*hr18
#       + 202.918*hr19 + 123.351*hr20 + 73.953*hr21 + 37.056*hr22
#       + (-10.332)*weathersit2 + (-65.174)*weathersit3 + 120.046*temp
#       + 124.043*atemp + (-84.515)*hum + (-29.261)*windspeed

#     The R-squared is 68.59% of the variation in the rental 
#     count is explained by variables.

#####   Compare these three model   #####
R_reg.3_all <- summary(reg.3_all)$r.squared 
R_reg.4_all <- summary(reg.4_all)$r.squared 
R_reg.5_all <- summary(reg.5_all)$r.squared 

compare_table <- matrix(c(R_reg.3_all, R_reg.4_all, R_reg.5_all), 
                        ncol=3, byrow=FALSE)
colnames(compare_table) <- c("reg.3_all", "reg.4_all", "reg.5_all")
rownames(compare_table) <- c("R-Squared")
compare_table

#   Interpretation:  
#     The r-squared of these three models are similar, 
#     69% of the variation in rental count is explained.
#     Furthermore, under the value of r-squared is similar, 
#     the model of reg.5_all would be the simplest and 
#     concise to explain to the rental count because it 
#     deletes the non-significant variables from the 
#     models of reg.3_all and reg.4_all.





# 6.  Randomly select approximately half of the observations 
#     to create a training data set.  
#     Add the remaining rows to create a test data set.  
#     Using the variables chosen in the model from question 5, 
#     compute the RSS, MSE, and RMSE on the training data.  
#     Repeat this on the test data, by using the model fit 
#     on the training data to compute predictions on the test set.  
#     Comment on the performance of the model with respect to 
#     the bias-variance tradeoff.

# Delete variable "obs"
dat2 <- dat2[c(1,3:55)] 

# Create a training and testing data set
set.seed(12345)
train <- sample(nrow(dat2),nrow(dat2)/2)
dat2.train <- dat2[train,]
dat2.test <- dat2[-train,]

# Run linear regression, using the variables chosen in the model from Q5
reg.6 <- lm(cnt ~ . - mnth1 - mnth2 - mnth4 - mnth6 - mnth7
            - mnth11 - workday - wkday3 - wkday4 - wkday5 - hr6 
            - weathersit4 - wkday2, data = dat2.train)
reg.6
summary(reg.6)

# Compute the RSS, MSE, and RMSE on the training data
RSS.train <- sum(reg.6$residuals^2)
MSE.train <- RSS.train/(nrow(dat2.train)-40-1)
RMSE.train <- sqrt(RSS.train/(nrow(dat2.train)-40-1))

# Compute the RSS, MSE, and RMSE on the testing data
yhat.test <- predict(reg.6, dat2.test)
RSS.test <- sum((dat2.test$cnt-yhat.test)^2)
MSE.test <- RSS.test/(nrow(dat2.test)-40-1)
RMSE.test <- sqrt(RSS.test/nrow(dat2.test)-40-1)

# Summary table for training data and testing data
tab <- matrix(c(RSS.train, MSE.train, RMSE.train, 
                RSS.test, MSE.test, RMSE.test), ncol=2, byrow=FALSE)
colnames(tab) <- c("Training", "Test")
rownames(tab) <- c("RSS", "MSE", "RMSE")
tab

# Interpretation for the model:
#   In this model, RMSE for training data and testing data 
#   are 101.16 and 102.52 respectively, which is similar. 
#   Thus, we can reasonably explain that the model is well 
#   captured for both training data and testing data under 
#   this approach of sample selection.






# 7. Repeat question 6 using the same training and test data sets.  
#     In this case, fit the model using all variables in the data set.  
#     Again, compute the model fit errors on the training set as well 
#     as on the test set by using the training model to make the test 
#     set predictions.  
#     Comment on the performance of the model with respect to the 
#     bias-variance tradeoff. Compare results to question 6.      

# Run linear regression, using the variables chosen in the model from Q5
reg.7 <- lm(cnt ~ ., data = dat2.train)
reg.7
summary(reg.7)

# Compute the RSS, MSE, and RMSE on the training data
RSS.train_all <- sum(reg.7$residuals^2)
MSE.train_all <- RSS.train_all/(nrow(dat2.train)-40-1)
RMSE.train_all <- sqrt(RSS.train_all/(nrow(dat2.train)-40-1))

# Compute the RSS, MSE, and RMSE on the testing data
yhat.test_all <- predict(reg.7, dat2.test)
RSS.test_all <- sum((dat2.test$cnt-yhat.test_all)^2)
MSE.test_all <- RSS.test_all/(nrow(dat2.test)-40-1)
RMSE.test_all <- sqrt(RSS.test_all/nrow(dat2.test)-40-1)

# Summary table for training data and testing data
tab_all <- matrix(c(RSS.train_all, MSE.train_all, RMSE.train_all, 
                RSS.test_all, MSE.test_all, RMSE.test_all), 
                ncol=2, byrow=FALSE)
colnames(tab_all) <- c("Training", "Test")
rownames(tab_all) <- c("RSS", "MSE", "RMSE")
tab_all

# The R-square of Model "reg.6" and "All-in" Model 
# Compare under the same training data and testing data
R_reg.6 <- summary(reg.6)$r.squared 
R_reg.7 <- summary(reg.7)$r.squared 

compare_table_3 <- matrix(c(R_reg.6, R_reg.7), ncol=2, byrow=FALSE)
colnames(compare_table_3) <- c("R_reg.6", "R_reg.7")
rownames(compare_table_3) <- c("R-Squared")
compare_table_3

######   Interpretation for the model/comparison   ######
#   Compared to the subset variables model from Q6 
#   and the all-in model from Q7, r-squared separately 
#   are 68.36% and 68.43%, which is not a big difference.
#   In the previous model, RMSE is respectively 101.16 
#   and 102.52. In this model, RMSE is respectively 101.06 
#   and 102.50. Therefore, we can reasonably conclude that 
#   as the variables add, for both training set and testing 
#   data, the RSS and RMSE are slightly decreased.






# 8. Repeat question 7 using the first year’s data as the 
#    training data set and the second year’s data as the test set.  
#    How does this result compare to the two earlier questions?

# Create a training and testing data set
library(dplyr)
set.seed(12345)
dat2.train_2 <- dat2 %>% filter(yr == 0)
dat2.test_2 <- dat2 %>% filter(yr == 1)

# Run linear regression, using the variables chosen in the model from Q5
reg.8 <- lm(cnt ~ ., data = dat2.train_2)
reg.8
summary(reg.8)

# Compute the RSS, MSE, and RMSE on the training data
RSS.train_yr <- sum(reg.8$residuals^2)
MSE.train_yr <- RSS.train_yr/(nrow(dat2.train_2)-40-1)
RMSE.train_yr <- sqrt(RSS.train_yr/(nrow(dat2.train_2)-40-1))

# Compute the RSS, MSE, and RMSE on the testing data
yhat.test_yr <- predict(reg.8, dat2.test_2)
RSS.test_yr <- sum((dat2.test_2$cnt-yhat.test_yr)^2)
MSE.test_yr <- RSS.test_yr/(nrow(dat2.test_2)-40-1)
RMSE.test_yr <- sqrt(RSS.test_yr/nrow(dat2.test_2)-40-1)

# Summary table for training data and testing data
tab_yr <- matrix(c(RSS.train_yr, MSE.train_yr, RMSE.train_yr, 
                    RSS.test_yr, MSE.test_yr, RMSE.test_yr), 
                 ncol=2, byrow=FALSE)
colnames(tab_yr) <- c("Training", "Test")
rownames(tab_yr) <- c("RSS", "MSE", "RMSE")
tab_yr

# The R-square of Model "reg.7" and "reg.8" Model 
# Compare under the same training data and testing data
R_reg.7 <- summary(reg.7)$r.squared 
R_reg.8 <- summary(reg.8)$r.squared 

compare_table_4 <- matrix(c(R_reg.7, R_reg.8), ncol=2, byrow=FALSE)
colnames(compare_table_4) <- c("R_reg.7", "R_reg.8")
rownames(compare_table_4) <- c("R-Squared")
compare_table_4

######   Interpretation for the model/comparison   ######
#   Compared to the all-in model from Q7 and Q8, 
#   r-squared separately are 68.43% and 68.48%, 
#   which is not a big difference.
#   In this model, under the different approach to 
#   setting the training data and testing data, RMSE 
#   is respectively 75.29 and 157.86, which has a big 
#   difference compared with the model in Q7. 
#   Additionally, in this model, RMSE between training 
#   data and testing has large difference; therefore, 
#   the model is not better fit in this case. 
#   It implies that variable of "year" is an important 
#   variable since the condition may differ by year 
#   and further to affect the accuracy of model fit. 






# 9. Summarize, briefly, what the managers of the bike rental 
#     concern should know about the prediction models found.  
#     How effective are the models?  
#     What model should be used to predict in the future (year 3)?  
#     How confident are you in future predictions? 

# Compare the model from Q6 to Q8
compare_table_5 <- matrix(c(R_reg.6, R_reg.7, R_reg.8), 
                          ncol=3, byrow=FALSE)
colnames(compare_table_5) <- c("R_reg.6", "R_reg.7", "R_reg.8")
rownames(compare_table_5) <- c("R-Squared")
compare_table_5

              #####   Interpretation   #####
#   Comparing with the model in Q6, Q7, and Q8, the r-squared 
#   separately are 68.36%, 68.42%, and 68.47%, which is not 
#   a big difference, but the model in Q8 has the highest 
#   r-square that 68.47% of the variation in the rental 
#   count well explained by other variables. However, as 
#   we can see that the approach of subset the testing 
#   and training by year are more dynamic than evenly 
#   split, which is not a better fit in the model from 
#   Q8 since the RMSE is a large difference. 
#   As a result, the model from Q6 could be the better 
#   prediction model for the rental count, has a similar 
#   r-squared with other models, and a similar RMSE between 
#   training and testing data. For me, I am more confident 
#   using the reg.6 to predict the future, which is more 
#   stable and flexible under an even split of training 
#   and testing data for better model fit. 






# 10. Are there any transformations of the variables that can 
#     be added to improve the model fit (e.g., squared variables)?  
#     Are there any interaction variables (products of variables) 
#     that can be added?  
#     Try at least one variable with higher order terms 
#     (e.g., squared,) and at least one interaction model to 
#     see if the biased can be reduced in fitting the model on 
#     the training set. Report results. 

# correlation of dat2
correlation_dat2 <- cor(dat2)
correlation_dat2

corrplot(correlation_dat2, method = "circle", type = "lower", 
         tl.col = "black", tl.cex = 0.6)

# Using the model from Q6
summary(reg.6)

# Squared variables
reg.6_1 <- lm(cnt ~ . - mnth1 - mnth2 - mnth4 - mnth6 - mnth7
             - mnth11 - workday - wkday3 - wkday4 - wkday5 - hr6 
             - weathersit4 - wkday2 + I(temp^2), data = dat2.train)
reg.6_1
summary(reg.6_1)

# Interaction variables
reg.6_2 <- lm(cnt ~ . - mnth1 - mnth2 - mnth4 - mnth6 - mnth7
              - mnth11 - workday - wkday3 - wkday4 - wkday5 - hr6 
              - weathersit4 - wkday2 + hr17 * atemp, data = dat2.train)
reg.6_2
summary(reg.6_2)

        #####   Interpretation   #####
#   To improve the model fit, we use the squared variable 
#   of temp, as we can see that the R-squared is 68.37%, 
#   which does not have a significant difference from the 
#   model from Q6 (68.36%). As interaction variables, 
#   choose the variables of "hr17" and "atemp," a positive 
#   correlation with the rental count. Based on the result, 
#   the R-squared from this model is improved to 69.27%. 
#   At the 95% confidence level, the p-value is smaller 
#   than 0.05, which is significant; for each additional 
#   hr17*atemp, rental count increases by 471.846. 
#   To improve the model fitting and reduce the biased, 
#   add the interaction variable, hr17*atemp, which might 
#   predict the rental count more accurately.

