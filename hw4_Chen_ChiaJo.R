# Hw4: Chia-Jo Chen

dat <- read.csv("hw4_bike_share_day.csv")
str(dat)
names(dat)

# Q1. Perform all necessary cleaning and transformation of ####
#   the data to make it useful for linear regression.  
#   That is, transform all qualitative variables to factors.  
#   No need to set up individual indicator columns in this assignment.  
#   Add a time trend variable and a quadratic time trend (trend squared) 
#   variable to the data set.  

# Transform all qualitative variables to factors.
dat$season <- as.factor(dat$season)
dat$mnth <- as.factor(dat$mnth)
dat$weekday <- as.factor(dat$weekday)
dat$weathersit <- as.factor(dat$weathersit)

# Transform dteday to date
dat$dteday <- as.Date(dat$dteday, format = "%m/%d/%Y")

# Add a time trend variable and a quadratic time trend variable to the data set.
timetr <- data.frame(1:731) 
timetrsq <- timetr^2

# “cbind” command to bind these new columns to the data set.
bike <- cbind(dat, timetr, timetrsq)

# Use the “names” command to give these new variables names, if necessary. 
names(bike)[15] <- "time_trend"
names(bike)[16] <- "time_squared"

# List all the variables transformed and/or added in the comments.
str(bike)
names(bike)





# Q2. Which of the variables could be considered as candidates ####
#   for seasonal variables in this data set?  Explain you answer clearly.
#   Hint:  What makes a variable a “seasonal” variable?

# Interpretation: Variables of Season, Month, and Weekday could be considered as candidates for seasonal variables in this data set. The count of rental bike would decline in early winter and summer; also, would increase in months with summer break and winter break that would be the seasonality trend.



# Q3. Run a regression model with time, time-squared, ####
#   and the month variables.  Comment on the quality of the model.  
#   Is there a significant trend in the data set?  
#   Squared trend?  
#   Is there significant seasonality?  
#   Compute the RMSE.
reg1 <- lm(cnt ~ time_trend + time_squared + mnth, data = bike)
summary(reg1)

# Compute RMSE
RMSE_reg1 <- summary(reg1)$sigma 
RMSE_reg1
# Interpretation: P-value is 2.2e-16, which is significant at the 95% confidence level. Only mnth2 and mnth11 are not significant, which is greater than 0.05. The R-squared is 71.3% of the variation in the bike rental count is explained by variables of time trend and month. 
# The RMSE of this model is 1047.201.




# Q4. Repeat question 3 by replacing the “month” variables ####
#   with the “season” variables.  
#   Compare to the model of question 3.  
#   Which is preferred and why?  
#   Compute the RMSE. 
reg2 <- lm(cnt ~ time_trend + time_squared + season, data = bike)
summary(reg2)

# Compute RSS, MSE, RMSE
RMSE_reg2 <- summary(reg2)$sigma 
RMSE_reg2 

# Interpretation: P-value is 2.2e-16, which is significant at the 95% confidence level. The R-squared is 63.73% of the variation in the bike rental count is explained by variables of time trend and season. Compared with the model of question 3, the model built with time trend and month is more preferred since, by month, Model in Q3 has a more significant seasonality trend in the dataset.
# The RMSE of this model is 1170.851.


# Q5. Create a (new) time series object, “cntts”, from the ####
#   count (cnt) variable with frequency 7.  
#   Plot the new time series version of the count variable in time.  
#   Describe any signals in this series.  
#   Plot the autocorrelation function for the series.  
#   Is there significant autocorrelation?  Explain.  
#   Hint:  See Hyndman, Sections 2.1, 2.2, 2.3 and 2.8. 

# Create a (new) time series object, “cntts”
cntts <- ts(bike[,14], start = 1, frequency = 7)
cntts[1:10]

# Plot the new time series version of the count variable in time.  
library(fpp2)
autoplot(cntts) +
  ggtitle("Weekly Count of bike rental 2011 - 2012") +
  ylab("Bike Rental Count") +
  xlab("Week") 

# Plot the autocorrelation function for the series
gglagplot(cntts) 
ggAcf(cntts)
cor(cntts[1:730],cntts[2:731])

# Interpretation: Based on the above graphs, there is a significant correlation between period and period.





# Q6. Generate a 28-period forecast using random walk forecast####
#   with drift and a second forecast using the seasonal naïve forecast.  
#   Compute the RMSE for each forecast.  
#   Run time series cross-validation for each of the two forecasted series.  
#   Report RMSEs from the cross-validation routines.
#   Hint:  See Hyndman, Sections 3.1 and 3.4.

# random walk forecast and seasonal naive forecast
f_rwf <- rwf(cntts, h = 28)
f_snv <- snaive(cntts, h = 28)

# Compute the RMSE for each forecast
res_rwf <- f_rwf$residuals
MSE_rwf <- mean((res_rwf[13:216])^2)
RMSE_rwf <- MSE_rwf^0.5

res_snv <- f_snv$residuals
MSE_snv <- mean((res_snv[13:216])^2)
RMSE_snv <- MSE_snv^0.5

# Plot {add one more large plot from ebook}
autoplot(f_rwf) +
  ylab("Bike Rental Count")  +
  xlab("Week")

autoplot(f_snv) +
  ylab("Bike Rental Count") +
  xlab("Week")

# Run time series cross-validation
e.rwf <- tsCV(cntts, rwf, drift=TRUE, h=28)
e.snv <- tsCV(cntts, naive, h=28) 

# Report RMSEs from the cross-validation routines.
cv.RMSE_rwf <- sqrt(mean(e.rwf^2 , na.rm = TRUE))
cv.RMSE_snv <- sqrt(mean(e.snv^2 , na.rm = TRUE))

# Summary table
rwf.snv.table <- matrix(c(RMSE_rwf, cv.RMSE_rwf, RMSE_snv, cv.RMSE_snv), ncol=2, byrow=FALSE)
colnames(rwf.snv.table) <- c("Random walk forecast", "Seasonal naive forecast")
rownames(rwf.snv.table) <- c("RMSE", "cv.RMSE")
rwf.snv.table
  
# Interpretation: RMSE for random walk forecast is 714.88, and for the seasonal naive forecast is 841.39. Under time-series cross-validation, RMSE for random walk forecast is 1471.65, and for the seasonal naive forecast is 1412.4.



# Q7. Estimate a 5-period (centered) moving average model ####
#   for the count data, that is, a moving average model with order equal to 5.  
#   What is the RMSE (root mean squared error) of this model?  
#   Try a few other odd values (e.g., 11 and 17) for the number 
#   periods and compute RMSEs to see if a better model can be found.  
#   Plot these models on the same graph.  
#   Report the RMSE for the best “ma” model.  
#   Hint:  See section 6.2.  Watch out for missing observations 
#   for fitted values at the beginning and end of the forecast series!   

# Estimate a 5-period (centered) moving average model and RMSE
ma5 <- ma(cntts, 5)
MSE_ma5 <- mean((ma5[3:729] - cntts[3:729])^2)
RMSE_ma5 <- MSE_ma5^0.5

# Estimate a 7-period (centered) moving average model and RMSE
ma7 <- ma(cntts, 7)
MSE_ma7 <- mean((ma7[4:728] - cntts[4:728])^2)
RMSE_ma7 <- MSE_ma7^0.5

# Plot these models on the same graph.
autoplot(cntts, series="Week") +
  autolayer(ma(cntts,5), series="5-MA") + 
  autolayer(ma(cntts,7), series="7-MA") +
  xlab("Bike Rental Count") + ylab("Week") +
  ggtitle("Weekly Count of bike rental 2011 - 2012") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red", "7-MA"="blue"),
                      breaks=c("Data","5-MA","7-MA"))

# Report the RMSE for the best “ma” model
ma.table <- matrix(c(RMSE_ma5, RMSE_ma7), ncol=2, byrow=FALSE)
colnames(ma.table) <- c("ma5", "ma7")
rownames(ma.table) <- c("RMSE")
ma.table

# Interpretation: RMSE of a 5-period moving average model is 707.59, and the RMSE of a 7-period moving average model is 794.97. Compared to these two models, 5-period is the best ma model since the RMSE is smaller than 7-period.

  

  
# Q8. Execute the classical additive and multiplicative ####
#   decompositions on the count (“cnt”) series.  
#   Based on the two decompositions, what is observed 
#   regarding trend and seasonal effect?  
#   Compute the RMSE from the remainder series for both these decompositions.  
#   Hint:  Use the “remainder” command to obtain the remainder component 
#   of a decomposition.  See Hyndman, Section 6.3.  
dc1_classical_add <- decompose(cntts, type = "additive")
autoplot(dc1_classical_add) +
  ggtitle("Bike Count Classical Additive Decomposition") + 
  xlab("Week")

dc1_classical_mult <- decompose(cntts, type = "multiplicative")
autoplot(dc1_classical_mult) +
  ggtitle("Bike Count Classical Multiplicative Decomposition") + 
  xlab("Week")

# RMSE - additive
dc1_classical_add$random
MSE_dc1_cA <- mean((dc1_classical_add$random[4:728])^2)
RMSE_dc1_cA <- MSE_dc1_cA^0.5
RMSE_dc1_cA

# RMSE - multiplicative
fit_cM <- dc1_classical_mult$trend * dc1_classical_mult$seasonal
MSE_dc1_cM <- mean((cntts[4:728] - fit_cM[4:728])^2)
RMSE_dc1_cM <- MSE_dc1_cM^0.5
RMSE_dc1_cM

# Summary Table
decom.table <- matrix(c(MSE_dc1_cA, RMSE_dc1_cA, MSE_dc1_cM, RMSE_dc1_cM), ncol=2, byrow=FALSE)
colnames(decom.table) <- c("additive", "multiplicative")
rownames(decom.table) <- c("MSE", "RMSE")
decom.table

# Interpretation: RMSE of classical additive and multiplicative decompositions on the count series separately is 781.38 and 782.53. 
# There is an increasing trend, and it seems like a repeated seasonality trend happened. Also, when we look at the end, there is a similar trend.





# Q9. Apply the STL decomposition to the count (“cnt”) series.  ####
#   Use s.window = 11 and t.window = 7 for these required parameters.  
#   Compute the RMSE from the remainder series.  
#   Hint:  See Hyndman, Section 6.6.  
decomp_STL <- stl(cntts, s.window = 11, t.window = 7, robust = TRUE)     
autoplot(decomp_STL) +
  ggtitle("Bike Count Classical Additive Decomposition") + 
  xlab("Week")

# The decomposition compnents are seasonal, trend-cycle and remander:
sa_stl <- decomp_STL$time.series[,1]
tc_stl <- decomp_STL$time.series[,2]
rem_stl <- decomp_STL$time.series[,3]

# MSE and RMSE
MSE_stl <- mean((rem_stl - mean(rem_stl))^2)
RMSE_stl <- MSE_stl^0.5
RMSE_stl

# Interpretation: RMSE of the STL decomposition to the count series is 737.0169.





# Q10. Compare all RMSEs computed.   ####
#   Which one had the lowest RMSE?  
#   Was the best procedure a forecasting procedure?   
#   If not, which of the forecasting procedures have the lowest RMSE?  
#   Summarize, in a sentence or two, the signals detected 
#   in the count data based on the work in this assignment.   

all.table <- matrix(c(RMSE_reg1, RMSE_reg2, RMSE_ma5, RMSE_ma7, 
                      RMSE_rwf, cv.RMSE_rwf, RMSE_snv, cv.RMSE_snv, 
                      RMSE_dc1_cA, RMSE_dc1_cM, RMSE_stl), ncol=1, byrow=FALSE)
colnames(all.table) <- c("RMSE")
rownames(all.table) <- c("RMSE_reg1", "RMSE_reg2", "RMSE_ma5", "RMSE_ma7",
                         "RMSE_rwf", "cv.RMSE_rwf", "RMSE_snv", "cv.RMSE_snv", 
                         "RMSE_dc1_cA", "RMSE_dc1_cM", "RMSE_stl")
all.table

# Interpretation: Based on the above table, we can see that the model of a 5-period moving average model, which is forecasting procedures, has the lowest RMSE with 707.5955. In this assignment, we can the trend in the dataset that the count of bike rentals will be increasing in the next year.