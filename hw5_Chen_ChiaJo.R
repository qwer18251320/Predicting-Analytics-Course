# Hw5: Chia-Jo Chen
library(fpp2)
library(dplyr)
dat <- read.csv("hw5_bike_share_day.csv")
str(dat)
names(dat)


# Q1. Create time series object from the bike sharing ####
cntts <- ts(dat$cnt, start = 1, frequency = 7)

# simple exponential smoothing (ses) model - α = 0.25
fit_hs1 <- ses(cntts, alpha = 0.25, initial = "simple")
summary(fit_hs1) 

MSE_hs_SES1 <- mean((cntts - fit_hs1$fitted)^2)
RMSE_hs_SES1 <- MSE_hs_SES1^0.5
RMSE_hs_SES1

# simple exponential smoothing (ses) model - Optimal
fit_hs2 <- ses(cntts, initial = "optimal")
summary(fit_hs2)

MSE_hs_SES2 <- mean((cntts - fit_hs2$fitted)^2)
RMSE_hs_SES2 <- MSE_hs_SES2^0.5
RMSE_hs_SES2

# Summary Table
ses.table <- matrix(c(RMSE_hs_SES1, RMSE_hs_SES2), ncol=2, byrow=FALSE)
colnames(ses.table) <- c("SES-0.25", "SES-Optimal")
rownames(ses.table) <- c("RMSE")
ses.table

# plot
autoplot(cntts, series = "Bike Rental Count") +
  autolayer(fit_hs1$fitted, series = "SES-0.25") +
  autolayer(fit_hs2$fitted, series = "SES-Optimal") +
  ggtitle("Weekly Count of bike rental 2011 - 2012") +
  ylab("Bike Rental Count") +
  xlab("Week")

# Interpretation: Based on the graph and RMSE, the SES-Initial model has a better fit, which RMSE is 964.5168, slightly smaller than SES-with α = 0.25.





# Q2. Fit Holt’s model with the bike sharing data series, cntts.  ####
fit_holt <- holt(cntts)
summary(fit_holt) 

# RMSE of holt's model = 964.7877
RMSE_holt <- (mean((cntts - fit_holt$fitted)^2))^0.5
RMSE_holt

# Summary Table
Holt.table <- matrix(c(RMSE_hs_SES2, RMSE_holt), ncol=2, byrow=FALSE)
colnames(Holt.table) <- c("SES-Initial", "Holt's Model")
rownames(Holt.table) <- c("RMSE")
Holt.table

# plot
autoplot(cntts, series = "Bike Rental Count") +
  autolayer(fit_holt$fitted, series = "Holt's Forecast") +
  ggtitle("Weekly Count of bike rental 2011 - 2012") +
  ylab("Bike Rental Count") +
  xlab("Week")

# Interpretation: Compared with the best model from Question 1, only slight difference between these two models; however, the SES-Optimal model with RMSE 964.5168, is a better fit.





# Q3. Fit Holt-Winters’ seasonal method ####
fit_AS1 <- hw(cntts, seasonal = "additive") 
summary(fit_AS1)
fit_AS2 <- hw(cntts, seasonal = "multiplicative") 
summary(fit_AS2)
fit_AS3 <- hw(cntts, damped = TRUE, seasonal="multiplicative") 
summary(fit_AS3)

# Compute RMSE
RMSE_AS_mean <- (mean((cntts - mean(cntts))^2))^0.5 
RMSE_AS_mean
RMSE_AS_add <- (mean((cntts - fit_AS1$fitted)^2))^0.5
RMSE_AS_add
RMSE_AS_mult <- (mean((cntts - fit_AS2$fitted)^2))^0.5
RMSE_AS_mult
RMSE_AS_damp_mult <- (mean((cntts - fit_AS3$fitted)^2))^0.5
RMSE_AS_damp_mult

# Summary Table
Winters.table <- matrix(c(RMSE_AS_mean, RMSE_AS_add, 
                          RMSE_AS_mult,RMSE_AS_damp_mult), ncol=1, byrow=FALSE)
colnames(Winters.table) <- c("RMSE")
rownames(Winters.table) <- c("Mean","Additive Season","Multiplicative Season",
                             "Damped Multiplicative Season")
Winters.table

# Interpretation: 
  # Within these three models, Damped Multiplicative is preferred (Smoothing parameters: alpha = 0.281, beta  = 1e-04, and gamma = 1e-04) that captured with a damped trend and multiplicative seasonality.  
  # Comparing with the models from questions 1 and 2, it's not surprising that the Damped Multiplicative model is more fitting. It not only considers the trend and seasonality but also the RMSE dropped significantly to 944.5366. 
  # The multiplicative model worked best on the bike rental data that identified the weekly seasonal pattern and the increasing trend at the end of the data.





# Q4. Create forecasts for the preferred model ####
fore_AS3 <- forecast(fit_AS3, h = 4)
summary(fit_AS3)

autoplot(fore_AS3) +
  ggtitle("Weekly Count of bike rental 2011 - 2012 Forecasts") + 
  ylab("Bike Rental Count") +
  xlab("Week")





# Q5. Built into R package fpp2 are many data sets.  ####
# Import johnson and johnson dataset
JohnsonJohnson <- JohnsonJohnson
fix(JohnsonJohnson)
str(JohnsonJohnson)

# Describe this data. What is the data range? How many observations?  
library(pastecs)
stat.desc(JohnsonJohnson)
# Interpretation: There are 84 observations. The standard deviation is 4.3099912. The maximum is 16.2, and the minimum is 0.55, the range of 15.76.

# What is periodicity of this data set?
autoplot(JohnsonJohnson)
Acf(JohnsonJohnson)
Pacf(JohnsonJohnson)
# Interpretation: Frequency = 4, which is quarterly period.

# Run the “AAA” ETS model on the earnings for Johnson and Johnson.  
fit_ets_jj <- ets(JohnsonJohnson, model = "AAA") 
summary(fit_ets_jj)

# Report the coefficients and the fit of the AAA ETS model.  
coef(fit_ets_jj)
accuracy(fit_ets_jj)
fitted(fit_ets_jj)
# Coefficients: Alpha = 0.07005553, Beta = 0.06992075, and gamma = 0.85627583

# RMSE of the AAA ETS model = 0.4363976
RMSE_jj <- (mean((JohnsonJohnson - fit_ets_jj$fitted)^2))^0.5
RMSE_jj

# Plot the three model components
autoplot(fit_ets_jj)

autoplot(JohnsonJohnson, series = "JohnsonJohnson") +
  autolayer(fitted(fit_ets_jj), series = "AAA") +  
  ggtitle("Johnson and Johnson 1960 - 1980") + 
  ylab("Count") +
  xlab("Year")





# Q6. Compute the best ETS model on the Johnson and Johnson data.  ####

# Compute the best ETS model on the Johnson and Johnson data
fit_jj <- ets(JohnsonJohnson)
summary(fit_jj)
# Best model is MAA ETS model

# RMSE of the MAA ETS model = 0.4713261
RMSE_jj_best <- (mean((JohnsonJohnson - fit_jj$fitted)^2))^0.5
RMSE_jj_best

# Interpretation: MAA model seems like the best preferred model fit since its Smoothing parameters: alpha = 0.2776, beta  = 0.0636, and gamma = 0.5867. RMSE of the MAA ETS model in JohnsonJohnson dataset is 0.4713261




# Q7. How is the preferred model selected by the ETS ####
# Interpretation: When the modeler does not specify a certain type of ETS model, the ETS command based on the lowest AIC to pick the preferred model.




# Q8. Compute the best ETS model on the monthly debit card use in Iceland (data set “debitcards”). ####
# Import the monthly debit card use in Iceland
card <- debitcards

# Compute the best ETS model
fit_card <- ets(card)
summary(fit_card)
# Best model is MAM ETS model

# RMSE of the MAM ETS model = 0.7239404
RMSE_card <- (mean((card - fit_card$fitted)^2))^0.5
RMSE_card

# Display the model components (chart)
autoplot(fit_card)

# Make forecasts with 80% confidence bands for the next two years using the chosen model.  
fore_card <- forecast(fit_card, h=24, level=c(80))
fore_card

# Graph the data and the forecasts.
autoplot(fore_card) +
  ggtitle("Monthly debit card use in Iceland 2000 - 2013 with Forecasts") + 
  ylab("$") +
  xlab("Year")

autoplot(card, series = "Monthly debit card") +
  autolayer(fitted(fit_card), series = "MAM") + 
  ggtitle("Monthly debit card use in Iceland 2000 - 2013") + 
  ylab("$") +
  xlab("Year")

# Interpretation: MAM model sis the best preferred model fit with the RMSE 0.7239404. Its Smoothing parameters: alpha = 0.3831 , beta  = 1e-04 , and gamma = 5e-04. 




# Q9. Compute the best ETS model on the Google closing price data (data set “goog”).  ####
# Import the Google closing price data
goog <- goog

# Compute the best ETS model
fit_goog_ets <- ets(goog)
summary(fit_goog_ets)
# Best model is MNN ETS model

# RMSE of the MNN ETS model = 8.729954
RMSE_goog_ets <- (mean((goog - fit_goog_ets$fitted)^2))^0.5
RMSE_goog_ets

# Display the model components (chart)
autoplot(fit_goog_ets)

# Make forecasts for the next 30 days using the chosen model. 
fore_goog_ets <- forecast(fit_goog_ets, h=30)
fore_goog_ets

# Graph the data and the forecasts.
autoplot(fore_goog_ets) +
  ggtitle("Google closing price with Forecasts") + 
  ylab("$") +
  xlab("Daily")

autoplot(goog, series = "Google closing price") +
  autolayer(fitted(fit_goog_ets), series = "MNN") + 
  ggtitle("Google closing price data") + 
  ylab("$") +
  xlab("Daily")

# Interpretation: MNN model is the best preferred model fit with the RMSE 8.729954. Its Smoothing parameters: alpha = 0.9999.  




# Q10. Compare the results of question 9 with a “best” ARIMA model on data set “goog”.  ####
# Comparing ARIMA Models
fets <- function(goog, h) {
  forecast(ets(goog), h =30)
}
farima <- function(goog, h) {
  forecast(auto.arima(goog), h=30)
}

# Compute CV errors for ETS as e1
e1 <- tsCV(goog, fets, h=30)
# Compute CV errors for ARIMA as e2
e2 <- tsCV(goog, farima, h=30)
# Find MSE of each model class
RMSE_ets <- (mean(e1^2, na.rm=TRUE))^0.5
RMSE_ets
RMSE_ar <- (mean(e2^2, na.rm=TRUE))^0.5
RMSE_ar

# Summary Table
google.table <- matrix(c(RMSE_ets, RMSE_ar), ncol=1, byrow=FALSE)
colnames(google.table) <- c("RMSE")
rownames(google.table) <- c("ETS Models","ARIMA Models")
google.table

# Interpretation: Interpretation: ETS model is more fitting than the ARIMA model in the google dataset, which RMSE is 31.63468 with the forecasts for the next 30 days.

# Plotting
fit_goog_ar <- auto.arima(goog)
summary(fit_goog_ar)
# Model for Google is ARIMA(0,1,0) with drift c = 0.4213

# Forecast with ARIMI
fore_goog_ar <- forecast(fit_goog_ar, h=30)

# Plot
autoplot(fore_goog_ar) +
  ggtitle("Google closing price data with Forecasts") + 
  ylab("$") +
  xlab("Daily")

autoplot(goog) +
  autolayer(fore_goog_ets, series = "ETS Forecast") + 
  autolayer(fore_goog_ar, series = "ARIMA Firecast") + 
  ggtitle("Google closing price data with Forecasts") + 
  ylab("$") +
  xlab("Daily")
