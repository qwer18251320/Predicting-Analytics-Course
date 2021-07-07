# Hw3: Chia-Jo Chen

dat <- read.csv("hw3_vio_crime_updated.csv")
str(dat)
names(dat)

# 1. Prepare the data for analysis.  ####
#   The data set provided with this assignment has had columns 
#   removed from the 128, so that it begins with 103 variables.  
#   It has also had one row removed leaving 1993.  
#   After reading in the data, examine the data’s structure.  
#   Note the definitions for columns 1, 2, and 3.  
#   These could remain in, but for this homework remove them.  
#   Why?  Use this set of columns as the “prepared data set” 
#   for the remainder of the assignment.   

# prepared data set
data <- dat[-c(1:3)]
str(data)
names(data)

# Interpretation: Columns 1, 2, and 3 have been deleted because 
# their data types are integers and characters.





# 2. Use the entire prepared data set for this part. ####  
#   Can we perform best subsets regression to pick a “good” model?   
#   Why or why not?  Explain what is happening.  
library(leaps)

# regfit.full 0
# regfit.full <- regsubsets(VioCrime~., data, really.big=T, nvmax = 14, nbest = 5)
# summary(regfit.full)
# summary(regfit.full)$rsq
# plot(summary(regfit.full)$rsq)
# names(reg.summary)
# reg.summary$rsq

# Interpretation: We can perform best subsets regression 
#   to pick a good model as the subset regression model 
#   has less bias and a higher R-squared. Under a better 
#   fitting, best subsets provide additional information. 
#   However, we cannot perform the best subsets regression 
#   using the entire prepared data set since the dataset 
#   is too large and there are 100 variables.





# 3. Use the entire prepared data set for this part. ####  
#   Perform forward stepwise regression to pick a 
#   “good” model by evaluating adjusted R-squared, 
#   Mallows Cp, and BIC. How many variables were 
#   in the models was selected using these criteria?  

regfit.fwd <- regsubsets(VioCrime~.,data=data, method = "forward")
regfit.fwd.summary <- summary(regfit.fwd)

coef(regfit.fwd, 8)

# Plot
par(mfrow=c(2,2))
plot(regfit.fwd.summary$rsq, xlab = "Number of Variables", ylab ="R-squared")
plot(regfit.fwd.summary$adjr2, xlab = "Number of Variables", ylab ="Adj R-squared")
which.max(regfit.fwd.summary$adjr2) 
maxar2.fwd <- which.max(regfit.fwd.summary$adjr2)
points(maxar2.fwd,regfit.fwd.summary$adjr2[maxar2.fwd], col = "red", cex = 2, pch = 20)
plot(regfit.fwd.summary$cp, xlab = "Number of Variables", ylab ="Mallows Cp")
which.min(regfit.fwd.summary$cp)
mincp.fwd <- which.min(regfit.fwd.summary$cp)
points(mincp.fwd,regfit.fwd.summary$cp[mincp.fwd], col = "blue", cex = 2, pch = 20)
plot(regfit.fwd.summary$bic, xlab = "Number of Variables", ylab ="Bayesian Info Crit")
which.min(regfit.fwd.summary$bic)
minbic.fwd <- which.min(regfit.fwd.summary$bic)
points(minbic.fwd,regfit.fwd.summary$bic[minbic.fwd], col = "green", cex = 2, pch = 20)

# Summary Table
fwd.table <- matrix(c(maxar2.fwd, mincp.fwd, minbic.fwd), ncol=3, byrow=FALSE)
colnames(fwd.table) <- c("Adj R-squared", "Mallows Cp", "Bayesian Info Crit")
rownames(fwd.table) <- c("Number of Variables")
fwd.table

#   Interpretation: While evaluating the Adj R-squared, 
#   Mallows Cp, and BIC, 8 variables in the models were 
#   selected using these criteria.





# 4. Repeat question 3 for backward stepwise regression.  #####
#   Is a “good” model in backward different from forward stepwise?  
#   Explain. Summarize what stepwise regression is providing.
regfit.bwd <- regsubsets(VioCrime~.,data=data, method = "backward")
regfit.bwd.summary <-summary(regfit.bwd)

coef(regfit.bwd, 8)

# Plot
par(mfrow=c(2,2))
plot(regfit.bwd.summary$rsq, xlab = "Number of Variables", ylab ="R-squared")
plot(regfit.bwd.summary$adjr2, xlab = "Number of Variables", ylab ="Adj R-squared")
which.max(regfit.bwd.summary$adjr2) 
maxar2.bwd <- which.max(regfit.bwd.summary$adjr2)
points(maxar2.bwd,regfit.bwd.summary$adjr2[maxar2.bwd], col = "red", cex = 2, pch = 20)
plot(regfit.bwd.summary$cp, xlab = "Number of Variables", ylab ="Mallows Cp")
which.min(regfit.bwd.summary$cp)
mincp.bwd <- which.min(regfit.bwd.summary$cp)
points(mincp.bwd,regfit.bwd.summary$cp[mincp.bwd], col = "blue", cex = 2, pch = 20)
plot(regfit.bwd.summary$bic, xlab = "Number of Variables", ylab ="Bayesian Info Crit")
which.min(regfit.bwd.summary$bic)
minbic.bwd <- which.min(regfit.bwd.summary$bic)
points(minbic.bwd,regfit.bwd.summary$bic[minbic.bwd], col = "green", cex = 2, pch = 20)

# Summary Table
bwd.table <- matrix(c(maxar2.bwd, mincp.bwd, minbic.bwd), ncol=3, byrow=FALSE)
colnames(bwd.table) <- c("Adj R-squared", "Mallows Cp", "Bayesian Info Crit")
rownames(bwd.table) <- c("Number of Variables")
bwd.table

# Interpretation: While evaluating the Adj R-squared, 
#   Mallows Cp, and BIC, 8 variables in the models 
#   were selected using these criteria.

# Summarize what stepwise regression is providing
coef(regfit.fwd, 8)
coef(regfit.bwd, 8)

# Summary Table for both Forward and Backward
table_fwd.bwd <- matrix(c(maxar2.fwd, mincp.fwd, minbic.fwd, 
                  maxar2.bwd, mincp.bwd, minbic.bwd), ncol=3, byrow=FALSE)
colnames(table_fwd.bwd) <- c("Adj R-squared", "Mallows Cp", "Bayesian Info Crit")
rownames(table_fwd.bwd) <- c("Forward", "Backward")
table_fwd.bwd

# Interpretation: Both forward and backward stepwise regression 
# selected 8 variables; however, the variables these selected are different.
# Forward stepwise regression result: racePctWhite, pctUrban, 
# FemalePctDiv, PctKids2Par, PctWorkMom, PctIlleg, HousVacant, NumStreet 
# Backward stepwise regression result: racepctblack, pctUrban, 
# MalePctDivorce, PctKids2Par, PctPersDenseHous, HousVacant, RentLowQ, MedRent  





# 5. Use your own 6-digit identification number as a seed.####
#   Randomly select approximately 50% of the rows for a 
#   training data set and include the rest of the observations 
#   in a test data set.  Run the “all-variables-in” regression 
#   model on the training data.  
#   What is the R-squared for this model on the training data?  
#   What is the RMSE?  
#   What is the RMSE when using this model to predict on the test data?  
#   Is overfitting an issue?  
#   Note:  We could spend time picking a “preferred” model here…but since there are so many variables this would be very time consuming.

set.seed(123456)
train <- sample(nrow(data),nrow(data)/2)
data.train <- data[train,]
data.test <- data[-train,]

# Run regression
reg.all <- lm(VioCrime~., data = data.train)
summary(reg.all)
sum.all <- summary(reg.all)

# RMSE, MSE, RSS, R-squared
RSS.train <- sum((reg.all$residuals)^2)
MSE.train <- RSS.train/nrow(data.train)
RMSE.train <- MSE.train^0.5

yhat.test <- predict(reg.all, data.test)
RSS.test <- sum((data.test$VioCrime - yhat.test)^2)
MSE.test <- RSS.test/nrow(data.test)
RMSE.test <- MSE.test^0.5

# Summary table for training data and testing data
all.table <- matrix(c(RSS.train, MSE.train, RMSE.train, 
                   RSS.test, MSE.test, RMSE.test), ncol=2, byrow=FALSE)
colnames(all.table) <- c("Training", "Test")
rownames(all.table) <- c("RSS", "MSE", "RMSE")
all.table

# Interpretation: For this model on the training data, 
# the R-squared is 72.46% of the variation in the VioCrime 
# is explained by variables. The RMSE on the training data 
# is 0.1214 and on the testing data is 0.1420, where 
# training data is more fit with the model. 
# It doesn't look like the overfitting happen.






# 6. Using the all-in model from question 5 run 5-Fold and #####
# 10-Fold cross-validation.  Save the MSEs and RMSEs?  
# Compare to question 5.  Why don’t we run LOOCV here?    
library(boot)
glm.1 <- glm(VioCrime~., data = data)

#  K = 5
set.seed(123456)
cv.err.5 <- cv.glm(data, glm.1, K = 5) 
MSE.5 <- cv.err.5$delta[2] 
RMSE.5 <- MSE.5^0.5

#  K = 10
set.seed(123456)
cv.err.10 <- cv.glm(data, glm.1, K = 10)
MSE.10 <- cv.err.10$delta[2]
RMSE.10 <- MSE.10^0.5

# Summary Table
cv.fold.table <- matrix(c(MSE.train, RMSE.train, MSE.test, RMSE.test,
                     MSE.5, RMSE.5, MSE.10, RMSE.10), ncol=4, byrow=FALSE)
colnames(cv.fold.table) <- c("Training", "Testing", "5-Fold", "10-Fold")
rownames(cv.fold.table) <- c("MSE", "RMSE")
cv.fold.table

# Interpretation: Compared within 5-Fold and 10-Fold, 
#   the MSE and RMSE are both nearly the same that model 
#   fitting in both testing and training data, but 10-Fold 
#   is slightly better. However, compared with the model in Q5, 
#   it looks like training data is more fit since its error 
#   is slightly lower. We cannot use LOOCV because the dataset 
#   has a large number of variables, which causes the potential 
#   to be computationally expensive and very time-consuming. 
#   Therefore, in this case, using k-fold cross-validation is 
#   better than LOOCV.





# 7. Repeat the process described on page 253 of James et al. ####  
#   First set up the data appropriate.  
#   Then set up the lambda grid.  
#   Then run “glmnet” using the training data and the lambda grid.  
#   Then use the best lambda from the glmnet run to fit a ridge 
#   regression model on the training data.   
#   Then predict y for the test data using this model and compute MSE/RMSE.   

# Set up the data appropriate
x <- model.matrix(VioCrime~., data)[,-1]
y <- data$VioCrime
dim(x)
head(x)

# Set up the lambda grid
library(glmnet)
set.seed(123456)
grid <- 10^seq(10,-2,length=100)

# Training and Testing 
set.seed(123456)
train <- sample(1:nrow(x), nrow(x)/2)
x.train <- x[train,]
y.train <- y[train]
x.test <- x[-train,]
y.test <- y[-train]

# Run “glmnet” using the training data and the lambda grid
ridge.mod <- glmnet(x.train, y.train, alpha = 0, lambda = grid, thresh = 1e-12)
ridge.coeff <- matrix(0, nrow = ncol(x), ncol = 100)
ridge.pred <- matrix(0,nrow = length(y.test), ncol = 100)
testerr <- matrix(0, nrow = 100, ncol = 1)

# Save values for 100 models
for (j in 1:100) {
  ridge.coeff[,j] <- ridge.mod$beta[,j]
  ridge.pred[,j] <- predict(ridge.mod, s = grid[j], 
                            newx = x.test)
  testerr[j] <- mean((ridge.pred[,j] - y.test)^2)
}

#  Plot the test MSEs for the 100 models
par(mfrow=c(1,1))
plot(testerr, xlab = "Model Number", 
     ylab = "Test Mean Suqare Error")
which.min(testerr)
# From the above, we know that model number 100 have the lowest MSE

ridge.mod$lambda[100]
grid[100]
# 100 model = lambda value of 0.01

RMSE.R.100 <- testerr[100]^0.5
RMSE.R.100 
# In this best model with lambda =0.01 have RMSE = 0.1405

# Best lambda from the glmnet run to fit a ridge regression model on the training data
ridge.mod.best <- glmnet(x.train, y.train, alpha = 0, 
                         lambda = grid[100], thresh = 1e-12)

# Predict y for the test data using this model and compute MSE/RMSE
ridge.pred.best <- predict(ridge.mod.best, s=grid[100], newx = x.test)
coef(ridge.mod.best)

# MSE and RMSE
MSE.ridge.best <- mean((ridge.pred.best-y.test)^2)
RMSE.ridge.best <- MSE.ridge.best^0.5

# Summary Table
ridge.table <- matrix(c(MSE.ridge.best, RMSE.ridge.best), ncol=1, byrow=FALSE)
colnames(ridge.table) <- c("ridge.best")
rownames(ridge.table) <- c("MSE", "RMSE")
ridge.table
# After improvement, the MSE is 0.01975, and the RMSE is 0.1405.


  


# 8. Now run cv.glmnet to perform cross validation on the #####
#   training data using ridge regression.  
#   Note you do not need “grid” here, cv.glmnet chooses that automatically.  
#   Once again pick the best lambda from glmnet to fit a ridge 
#   regression model on the training data.  
#   Then predict y for the test data and compute MSE/RMSE.  

# cv.glmnet
set.seed(123456)
cv.out <- cv.glmnet(x.train, y.train, alpha = 0) 
plot(cv.out)

# Best lambda from glmnet to fit a ridge regression model on the training data
bestlam = cv.out$lambda.min
bestlam 
log(bestlam) 

set.seed(123456)
cv.ridge.mod.best <- glmnet(x.train, y.train, alpha = 0, 
                        lambda = bestlam, thresh = 1e-12)

# Predict y for the test data using this model
cv.ridge.pred.best <- predict(cv.ridge.mod.best, s=bestlam, newx = x.test)

# MSE and RMSE
MSE.cv.ridge.best <- mean((cv.ridge.pred.best-y.test)^2)
RMSE.cv.ridge.best <- MSE.cv.ridge.best^0.5

# Summary Table
cv.R.best.table <- matrix(c(MSE.cv.ridge.best, RMSE.cv.ridge.best), 
                          ncol=1, byrow=FALSE)
colnames(cv.R.best.table) <- c("cv,ridge")
rownames(cv.R.best.table) <- c("MSE.cv.ridge.best", "RMSE.cv.ridge.best")
cv.R.best.table
# With our further improvement, MSE is 0.0198, and RMSE is 0.14068.





# 9. Repeat question 8 using LASSO.####
# cv.glmnet
set.seed(123456)
cv.out1 <- cv.glmnet(x.train, y.train, alpha = 1)
plot(cv.out1)

# Best lambda from glmnet to fit a ridge regression model on the training data
bestlam1 <- cv.out1$lambda.min
bestlam1
log(bestlam1)

set.seed(123456)
cv.lasso.mod.best <- glmnet(x.train, y.train, alpha = 1, 
                        lambda = bestlam1, thresh = 1e-12)

# Predict y for the test data using this model
cv.lasso.pred.best <- predict(cv.lasso.mod.best, s=bestlam1, newx = x.test)

# MSE and RMSE
MSE.cv.lasso.best <- mean((cv.lasso.pred.best-y.test)^2)
RMSE.cv.lasso.best <- MSE.cv.lasso.best^0.5

# Summary Table
cv.L.best.table <- matrix(c(MSE.cv.lasso.best, RMSE.cv.lasso.best), 
                          ncol=1, byrow=FALSE)
colnames(cv.L.best.table) <- c("cv,ridge")
rownames(cv.L.best.table) <- c("MSE.cv.lasso.best", "RMSE.cv.lasso.best")
cv.L.best.table
# With our further improvement, MSE is 0.0198, and RMSE is 0.1408.





# 10. Based on the work in questions 5-9, what is a ####
#   “fair” estimate of the MSE/RMSE.  Explain.   

# Comparison table of the model coefficients
options(scipen=999)
coeff.tab <- cbind(coef(lm(VioCrime~.,data=data)), 
                   coef(cv.ridge.mod.best), coef(cv.lasso.mod.best))
colnames(coeff.tab) <- c("Regular Regression", "Ridge Regression", "Lasso")
coeff.tab

# Summary Table
summary.table <- matrix(c(RMSE.train, RMSE.test, RMSE.5, RMSE.10, 
                          RMSE.ridge.best, RMSE.cv.ridge.best, 
                          RMSE.cv.lasso.best), ncol=1, byrow=FALSE)
colnames(summary.table) <- c("RMSE")
rownames(summary.table) <- c("RMSE.train", "RMSE.test", "RMSE.5", "RMSE.10", 
                             "RMSE.ridge.best", "RMSE.cv.ridge.best", 
                             "RMSE.cv.lasso.best")
summary.table

# Interpretation: Comparing with RMSE, we can see the 
# training data in the Q5 model has the lowest RMSE, 
# but it has slightly different from testing data; 
# thus, it doesn't look like a fair estimate model. 
# For me, I regard 10-fold cross-validation as a fairly good estimate.
