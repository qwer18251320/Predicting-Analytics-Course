# Hw6: Chia-Jo Chen
library(fpp2)
library(dplyr)
library(lubridate)
library(readxl)
library(MASS)

family <- read_excel("hw6_one_family_homes.xlsx")
family$Date <- as.Date(family$Date, format = "%Y%m%d")
str(family)
names(family)

usgdp <- read_excel("hw6_USGDP.xlsx")
usgdp$Date <- as.Date(usgdp$Date, format = "%Y%m%d")
str(usgdp)
names(usgdp)

# Q1. Generate 200 observations of the following series.  ####
#   In each case, plot the series generated.  
#   Also, graph the autocorrelation and partial auto-correlation functions. 
#   Finally, in each case, comment on how the pattern in the auto- and 
#   partial auto-correlation functions are related to the series generated.  
#   Hint:  Use the “rnorm” function in R for part a and build the 
#          other series from this.

# a. White noise
set.seed(123456)
wnts <- ts(data.frame(rnorm(200))) 

# Plot series, acf, pacf, residual
autoplot(wnts)
Acf(wnts)
Pacf(wnts)
checkresiduals(wnts)

# b. AR(1) with parameter, φ1 = 0.6
set.seed(123456)
ysim1 <- ts(data.frame(matrix(rep(0),200,1)))
ysim1[1,1] <- wnts[1]
for (i in 2:200) {
  ysim1[i,1] <- 0.6*ysim1[i-1,1] + wnts[i] 
}

# Plot series, acf, pacf, residual
autoplot(ysim1)
acf(ysim1)
pacf(ysim1)
checkresiduals(ysim1)

# c. AR(2) with parameters, φ1 = 0.6 and φ2 = 0.3
set.seed(123456)
ysim2 <- ts(data.frame(matrix(rep(0),200,1)))
ysim2[1,1] <- wnts[1]
ysim2[2,1] <- wnts[2]
for (i in 3:200) {
  ysim2[i,1] <- 0.6*ysim2[i-1,1] + 
    + 0.3*ysim2[i-2,1] + wnts[i] 
}

# Plot series, acf, pacf, residual
autoplot(ysim2)
acf(ysim2)
pacf(ysim2)
checkresiduals(ysim2)

# d. AR(2) with parameters, φ1 = 0.8 and φ2 = – 0.3
set.seed(123456)
ysim3 <- ts(data.frame(matrix(rep(0),200,1)))
ysim3[1,1] <- wnts[1]
ysim3[2,1] <- wnts[2]
for (i in 3:200) {
  ysim3[i,1] <- 0.8*ysim3[i-1,1] + 
    - 0.3*ysim3[i-2,1] + wnts[i] 
}

# Plot series, acf, pacf, residual
autoplot(ysim3)
acf(ysim3)
pacf(ysim3)
checkresiduals(ysim3)

# e. MA(1) with parameter, θ1 = 0.6
set.seed(123456)
ysim4 <- ts(data.frame(matrix(rep(0),200,1)))
ysim4[1,1] <- wnts[1]
for (i in 2:200) {
  ysim4[i,1] <- wnts[i] + 0.6*wnts[i-1] 
}

# Plot series, acf, pacf, residual
autoplot(ysim4)
acf(ysim4)
pacf(ysim4)
checkresiduals(ysim4)

# f. ARMA(1,1) with parameters, φ1 = 0.5 and θ1 = 0.4
set.seed(123456)
ysim5 <- ts(data.frame(matrix(rep(0),200,1)))
ysim5[1,1] <- wnts[1]
for (i in 2:200) {
  ysim5[i,1] <- 0.5*ysim5[i-1] + 0.4*wnts[i-1] + wnts[i]
}

# Plot series, acf, pacf, residual
autoplot(ysim5)
acf(ysim5)
pacf(ysim5)
checkresiduals(ysim5)

# g. ARIMA(1,1,1) with parameters, φ1 = 0.5 and θ1 = 0.4
set.seed(123456)
ysim6 <- Arima(wnts, order = c(1,1,1), fixed = c(0.5, 0.4))
summary(ysim6)

# Plot series, acf, pacf, residual
autoplot(ysim6$fitted)
checkresiduals(ysim6)
ysim6 %>% residuals() %>% ggtsdisplay()

# h. ARIMA(1,1,1)(0,1,0)[4] with parameters, φ1 = 0.5 and θ1 = 0.4
set.seed(123456)
ysim7 <- Arima(wnts, order = c(1,1,1), 
               seasonal = list(order = c(0,1,0), period = 4), 
               fixed = c(0.5, 0.4))
summary(ysim7)

# Plot series, acf, pacf, residual
autoplot(ysim7$fitted)
checkresiduals(ysim7)
ysim7 %>% residuals() %>% ggtsdisplay()




# Q2. For the US Quarterly GDP data, complete the following steps ####

# a. Set up the data set for analysis.  If necessary, 
#    find a suitable Box-Cox transformation for the data set 
#    (see Hyndman, section 3.2).
gdp <- ts(usgdp[,2], start = 1947, frequency = 4)
lambda1 <- BoxCox.lambda(gdp)
gdp.BoxCox <- BoxCox(gdp, lambda1)

# b. Fit a suitable ARIMA model using auto.arima().
gdp.arima1 <- auto.arima(gdp.BoxCox) 
summary(gdp.arima1) # ARIMA(3,1,2)(0,1,2)[4] 

# Plot residual, acf, pacf
checkresiduals(gdp.arima1)
gdp.arima1 %>% residuals() %>% ggtsdisplay()

autoplot(gdp.BoxCox, series = "GPD") +
  autolayer(gdp.arima1$fitted, series = "Fitted") + 
  ggtitle("USGDP") +
  ylab("GDP") +
  xlab("Year")

RMSE1 <- mean((gdp.BoxCox-gdp.arima1$fitted)^2)^0.5 
RMSE1 # RMSE 0.2406697

# c. Try 3 other ARIMA models by experimenting with the model orders, p and q.
gdp.arima2 <- Arima(gdp.BoxCox, order = c(1,1,1), seasonal=c(1,0,0))
summary(gdp.arima2)
RMSE2 <- mean((gdp.BoxCox-gdp.arima2$fitted)^2)^0.5 
RMSE2 # RMSE 0.2921278

gdp.arima3 <- Arima(gdp.BoxCox, order = c(2,1,1), seasonal=c(1,1,1))
summary(gdp.arima3)
RMSE3 <- mean((gdp.BoxCox-gdp.arima3$fitted)^2)^0.5 
RMSE3 # RMSE 0.2489536

gdp.arima4 <- Arima(gdp.BoxCox, order = c(2,2,0), seasonal=c(1,1,1))
summary(gdp.arima4)
RMSE4 <- mean((gdp.BoxCox-gdp.arima4$fitted)^2)^0.5
RMSE4 # RMSE 0.2816829

# d. Choose the preferred model and check the residual diagnostics. Report observations.
# Summary Table
gpd.table <- matrix(c(RMSE1,RMSE2,RMSE3,RMSE4,
                      gdp.arima1$aic,gdp.arima2$aic,gdp.arima3$aic,gdp.arima4$aic), ncol=2, byrow=FALSE)
colnames(gpd.table) <- c("RMSE", "AIC")
rownames(gpd.table) <- c("ARIMA 1","ARIMA 2","ARIMA 3","ARIMA 4")
gpd.table

checkresiduals(gdp.arima1)
# The best model is arima1: AARIMA(3,1,2)(0,1,2)[4] with non-zero mean with the RMSE 0.2406697 and AIC= 20.94232

# e. Produce forecasts for the next two years. Do not forget to transform the data back to the original form, if necessary! Let's transform the logged data back to the original series
fcst_best_gdp <- forecast(gdp.arima1, h = 8)
autoplot(fcst_best_gdp, series = "Forecasts") +
  ggtitle("USGDP") +
  ylab("GDP") +
  xlab("Year")

# transform the logged data back to the original series
?InvBoxCox()
fit_arima1 <- gdp.arima1$fitted
inv_gdp <- InvBoxCox(fit_arima1, lambda=lambda1)
fcst_best_gdp <- forecast(inv_gdp, h = 8)
fcst_best_gdp

autoplot(gdp, series = "Actual Data") +
  autolayer(fcst_best_gdp, series = "Forecasts") +
  ggtitle("USGDP") +
  ylab("GDP") +
  xlab("Year")
 
# f. Compare the results with those obtained from running ets() on the non-transformed series.
# ets model
fit_ets_gdp <- ets(gdp.BoxCox)
summary(fit_ets_gdp) # ETS(A,A,A) 
RMSE_ets_gdp <- (mean((gdp.BoxCox - fit_ets_gdp$fitted)^2))^0.5
RMSE_ets_gdp # RMSE 0.2733539

autoplot(fit_ets_gdp)

fcst_ets_gdp <- forecast(fit_ets_gdp, h = 8)
autoplot(fcst_ets_gdp, series = "Forecasts") +
  ggtitle("USGDP") +
  ylab("GDP") +
  xlab("Year")

# Comparing ARIMA and ets Models
fets <- function(x, h) {
  forecast(ets(x), h=h)
}
farima <- function(x, h) {
  forecast(Arima(x, order = c(3,1,2), seasonal = c(0,1,2)), h=h)
}

# Compute CV errors for ETS as e1
e1_gdp <- tsCV(gdp.BoxCox, fets, h=8)

# Compute CV errors for ARIMA as e2
e2_gdp <- tsCV(gdp.BoxCox, farima, h=8)

# Find MSE of each model class
RMSE_ets_gdp <- (mean(e1_gdp^2, na.rm=TRUE))^0.5
RMSE_ets_gdp
RMSE_ar_gdp <- (mean(e2_gdp^2, na.rm=TRUE))^0.5
RMSE_ar_gdp

# Summary Table
all.table_gdp <- matrix(c(RMSE_ets_gdp, RMSE_ar_gdp), ncol=1, byrow=FALSE)
colnames(all.table_gdp) <- c("RMSE")
rownames(all.table_gdp) <- c("ETS Models","ARIMA Models")
all.table_gdp
          



   
# Q3. Repeat the previous question using the US single family home sales data. ####
# a. Set up the data set for analysis.  If necessary, 
#    find a suitable Box-Cox transformation for the data set 
#    (see Hyndman, section 3.2).
fam <- ts(family[,2], start = 1963, frequency = 12)
lambda2 <- BoxCox.lambda(fam) # 0.5930056
fam.BoxCox <- BoxCox(fam, lambda2)

# b. Fit a suitable ARIMA model using auto.arima().
fam.arima1 <- auto.arima(fam.BoxCox)
summary(fam.arima1) # ARIMA(2,1,3)(1,0,2)[12] 

# Plot residual, acf, pacf
checkresiduals(fam.arima1)
fam.arima1 %>% residuals() %>% ggtsdisplay()

autoplot(fam.BoxCox, series = "Sales(in 1000s") +
  autolayer(fam.arima1$fitted, series = "Fitted") + 
  ggtitle("Family member Sales(in 1000s)") +
  ylab("Sales(in 1000s") +
  xlab("Year")

RMSE1 <- mean((fam.BoxCox-fam.arima1$fitted)^2)^0.5 
RMSE1 # RMSE 3.114723

# c. Try 3 other ARIMA models by experimenting with the model orders, p and q.
fam.arima2 <- Arima(fam.BoxCox, order = c(1,1,1), seasonal=c(1,0,0))
summary(fam.arima2)
RMSE2 <- mean((fam.BoxCox-fam.arima2$fitted)^2)^0.5 
RMSE2 # RMSE 3.178938

fam.arima3 <- Arima(fam.BoxCox, order = c(2,1,1), seasonal=c(1,1,1))
summary(fam.arima3)
RMSE3 <- mean((fam.BoxCox-fam.arima3$fitted)^2)^0.5 
RMSE3 # RMSE 3.167641

fam.arima4 <- Arima(fam.BoxCox, order = c(2,2,0), seasonal=c(1,1,1))
summary(fam.arima4)
RMSE4 <- mean((fam.BoxCox-fam.arima4$fitted)^2)^0.5
RMSE4 # RMSE 3.794254

# d. Choose the preferred model and check the residual diagnostics.  Report observations.
# Summary Table
fam.table <- matrix(c(RMSE1,RMSE2,RMSE3,RMSE4,
                      fam.arima1$aic,fam.arima2$aic,fam.arima3$aic,fam.arima4$aic), ncol=2, byrow=FALSE)
colnames(fam.table) <- c("RMSE", "AIC")
rownames(fam.table) <- c("ARIMA 1","ARIMA 2","ARIMA 3","ARIMA 4")
fam.table

checkresiduals(fam.arima1)
# The best model is arima1: ARIMA(2,1,3)(1,0,2)[12]  with non-zero mean with the RMSE 3.114723 and AIC= 3525.706

# e. Produce forecasts for the next two years.  
#    Do not forget to transform the data back to the original form, if necessary!
fcst_best_fam <- forecast(fam.arima1, h = 24)
autoplot(fcst_best_fam) +
  ggtitle("Family member Sales(in 1000s)") +
  ylab("Sales(in 1000s") +
  xlab("Year")

# transform the logged data back to the original series
fit_arima_fam <- fam.arima1$fitted
inv_fam <- InvBoxCox(fit_arima_fam, lambda=lambda2)
fcst_best_fam <- forecast(inv_fam, h = 24)
fcst_best_fam

autoplot(fam, series = "Actual Data") +
  autolayer(fcst_best_fam, series = "Forecasts") +
  ggtitle("Family member Sales(in 1000s)") +
  ylab("Sales(in 1000s") +
  xlab("Year")

# f. Compare the results with those obtained from running ets() on the non-transformed series.
# ets model
fit_ets_fam <- ets(fam.BoxCox)
summary(fit_ets_fam) # ETS(A,N,N)  
RMSE_ets <- (mean((fam.BoxCox - fit_ets_fam$fitted)^2))^0.5
RMSE_ets # RMSE 3.201463

autoplot(fit_ets_fam)

fcst_ets_fam <- forecast(fit_ets_fam, h = 24)
autoplot(fcst_ets_fam, series = "Forecasts") +
  ggtitle("Family member Sales(in 1000s)") +
  ylab("Sales(in 1000s") +
  xlab("Year")

# Comparing ARIMA and ets Models
# Summary Table
all.table_fam <- matrix(c(RMSE_ets, RMSE1), ncol=1, byrow=FALSE)
colnames(all.table_fam) <- c("RMSE")
rownames(all.table_fam) <- c("ETS Models","ARIMA Models")
all.table_fam 

