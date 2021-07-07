# Hw1: Chia-Jo Chen

# Import data
data <- read.csv("hw1_universal_bank.csv")
names(data)
str(data)

# A. Write scripts to display at least 4 graphical statistical analyses
#    of the Universal Bank data.   
#    Create at least on each: pie chart, bar chart, histogram, and box plots.

Color <- c("lightsteelblue2", "rosybrown1", "burlywood1", "darkolivegreen1")

# Pie chart
Edu <- table(data$Education)
Edu

pie(Edu, labels = paste(round(prop.table(Edu)*100), "%", sep = ""),
    main = "Pie Chart: Education Level Distribution", col = Color)
legend("right", c("Level 1","Level 2","Level 3"), cex = 0.8, fill = Color)

# Bar Chart
Fam <- table(data$Fam_Size)
Fam

barplot(Fam, main = "Boxchart: Family Size",
        xlab = "Family Size", ylab = "Count", horiz = F, col = Color)

# Histogram
hist(data$Crdt_Crd_Avg, main = "Histogram: Average Credit Card Debt",
     xlab = "Spending on Credit Card (in $1,000s)", 
     col = c("lightsteelblue2"))

# Boxplot
boxplot(Income ~ Acpt_Offer, data = data, 
        main = "Boxplot: Income ~ Accept Offer", horizontal = F, 
        xlab = "Accept Offer", ylab = "Income (in $1,000s)", col = Color) 





# B. Create a boxplot of the Average Credit Card Debt conditional 
#    on the customer’s acceptance of the bank’s offer.  
#    Create a second “conditioned” chart of your choice as well.  
#    Hint:  For the boxplot, use the “~” command after the variable of 
#    interest to link it to the one where the conditioning occurs.

Color2 <- c("lightsteelblue2", "rosybrown1", "burlywood1")

boxplot(Crdt_Crd_Avg ~ Acpt_Offer, data = data, horizontal = F, col = Color2, 
        main = "Boxplot: Credit Card Debt vs Accept Offer",
        xlab = "Accept Offer", 
        ylab = "Credit Card Average Spending ($1,000)") 

boxplot(Crdt_Crd_Avg ~ Acpt_Offer + Education, data = data, horizontal = F, 
        col = Color2, 
        main = "Boxplot: Average Credit Card Debt",
        xlab = "Accept Offer : Education Level", 
        ylab = "Credit Card Average Spending ($1,000)") 





# C- Part 1. Create a scatter plot with Income on the x-axis and 
#    Average Credit Card Debt on the y-axis.  
#    Add the least squares regression line to the plot.  
#    In the context of this data set, what information does this chart convey?  
plot(data$Income, data$Crdt_Crd_Avg, main = "Scatter Plot",
     xlab="Income", ylab = "Average Credit Card Debt ($1000s)")
abline(lm(data$Crdt_Crd_Avg ~ data$Income), col = c("Red"))

#   Interpretation:
#       In Question C-1 scatter plot, we can see that there is a 
#       positive linear relationship, more income more credit card 
#       spending per month. Also, lower-income with lower debt is 
#       denser than higher-income with higher debt customers. 
#       The abline represents the regression line on Average Credit 
#       Card Debt (in $1,000) by independent variable "income."



# C- Part 2. Create a second scatter plot with Average Credit Card Debt 
#    on the x-axis and Accept Offer on the y-axis.  
#    Add the least squares regression line to the plot.  
#    What is the take-away from this chart?
plot(data$Crdt_Crd_Avg, data$Acpt_Offer, main = "Scatter Plot",
     xlab="Average Credit Card Debt", ylab = "Accept Offer")
abline(lm(data$Acpt_Offer ~ data$Crdt_Crd_Avg), col=c("Red"))

#   Interpretation:
#       There is no linear relationship whether customers accept 
#       the offer or not with the variable "Average Credit Card Debt." 
#       Therefore, we can further use logistic regression to predict 
#       the dependent variable of accepting or not accepting offers. 





# D. In well-written sentences, provide at least three unique 
#    insights regarding the Universal Bank data based on the 
#    analyses completed in parts A-C. 

#   Interpretation:
#       1. Based on the pie chart, Education Level 1 has a larger 
#          percentage, 43%, than Education Level 2 and 3.
#       2. Based on the Histogram, there is a right-skewness distribution, 
#          and most customers spending no more than $3,000 on a 
#          credit card each month
#       3. According to the bar chart in part a, spending more on a credit
#          card each month, prefer to accept a personal loan offer.  
#          And customers, who spend less on a credit card, prefer to reject 
#          the offer. However, there are a lot of outliers lies on Acpt_Offer = 0.
#       4. Customers, who accept a loan offer, as the education level 
#          increases, their average spending on a credit card is increasing. 





# E. Compute descriptive statistics, e.g., means, medians, standard 
#    deviations, etc., for all variables in the data set.  

new_data <- subset(data, select = -c(ID))

# Mean and Median
summary(new_data)

# Standard Deviation
apply(new_data,2,sd)

# Another method for Standard Deviation
library(psych)
psych::describe(new_data)





# F. Compute the Pearson correlation for all pairs variables.  
#    Given the goal of finding out which customers will accept the bank’s 
#    offer, which three variables appear to be the most likely predictors?

library(corrplot)

str(new_data)
cor(new_data)[14,]

# Correlation Table
correlation <- cor(new_data)
correlation

corrplot(correlation, method = "circle", type = "lower", 
         tl.col = "black", tl.cex = 0.6)

corrplot(correlation, method = "number", type = "lower", number.cex=0.7,
         tl.col = "black", tl.cex = 0.6)

#   Interpretation:
#       To find out which customer will accept the bank's loan offer, 
#       we need to focus on the following variables, which has a higher 
#       correlation with the acpt_offer variable:
#           1. Income: 0.5172405150
#           2. Crdt_Crd_Avg: 0.3769046179 
#           3. CD_Account: 0.3383284125 





# G. Fit a simple linear regression that predicts monthly 
#    average credit card debt as a function of income.  
#    What is the estimated regression model?   
#    How well does this model fit?  
#    Interpret the value of the slope coefficient in this regression.
linear_reg_1 <- lm(Crdt_Crd_Avg ~ Income, data = new_data) 
summary(linear_reg_1)

sumreg_1 <- summary(linear_reg_1)
sumreg_1$r.squared

#   Interpretation for Estimated Regression:
#       Crdt_Crd_Avg ($1000s): 0.1103670 + 0.0247278 * Income (in $1000s)

#   Interpretation for R-squared: 
#       The R-squared (multiple r-squared) is 0.4132
#       Interpretation of R-squared: 41.3% of the variation in the 
#       Average credit card debt is explained by the income variable.

#   Interpretations of the Slope and Intercept:
#       (Slope)  For each additional 1000 income, 
#       average credit card debt increases by 24.72.





# H. Fit a simple linear regression that predicts which customers 
#    will accept the bank’s offer of the personal loan product as 
#    a function of monthly average credit card debt.  
#    How well does this model fit?  
#    Interpret the value of the slope coefficient in the regression.  
#    Discuss the usefulness of this model.

linear_reg_2 <- lm(Acpt_Offer ~ Crdt_Crd_Avg, data = new_data) 
summary(linear_reg_2)

sumreg_2 <- summary(linear_reg_2)
sumreg_2$r.squared

#   Interpretation for Estimated Regression:
#       Acpt_Offer: -0.02181 + 0.063656 * Crdt_Crd_Avg (in $1000s)

#   Interpretation for R-squared: 
#       The R-squared (multiple r-squared) is 0.1421
#       Interpretation of R-squared: 14.21% of the variation in Accept Offer
#       is explained by the Average credit card debt variable.

#   Interpretations of the Slope and Intercept:
#       (Slope)  For each additional Average credit card debt, 
#       Accept Offer increases by 0.063656.





# I- Part 1. For the models fit in parts h and i, provide and interpret 
#    a 99% prediction interval estimate for the dependent variable.  

#   Interpretation: 
#       The model in parts G and H are significant because both 
#       p-value, 2e-16, are smaller than 0.05; therefore, 
#       we can reject the null hypothesis.



# I- Part 2. Assume that given values for income and credit card debt to use 
#    for prediction are $75,000 and $1,250, respectively.  
#    Are these estimates useful?  Why or why not? 

pred_1 <- data.frame(Income = 75)
predict(linear_reg_1, pred_1, interval = "prediction", level = 0.99)
#   Result: fit = 1.964954, lwr = -1.581923, upr = 5.51183
#   In Part G: Crdt_Crd_Avg ($1000s): 0.1103670 + 0.0247278 * 75 = 1.964952

#   Interpretation: 
#       These estimates are useful, which is the same result as the 
#       regression model used. For the income, the 99% prediction 
#       interval estimate for average credit card debt for a person 
#       making $75,000 in annual income is between 0 to 5511. 

pred_2 <- data.frame(Crdt_Crd_Avg = 1.25)
predict(linear_reg_2, pred_2, interval = "prediction", level = 0.99)
#   Result: fit = 0.05775935, lwr = -0.6665824, upr = 0.7821011
#   In Part H: Acpt_Offer: -0.02181 + 0.063656 * 1.25 = 0.05776

#   Interpretation: 
#       These estimates are useful, which is the same result as the 
#       regression model used. For the average credit card debt, 
#       the 99% prediction interval estimate for the accept offer 
#       for a person making $1,250 in spending average on a credit 
#       card is between 0 to 782.





# Q10. Suppose the goal was to predict whether a customer would 
#      accept the bank’s product offer (Acpt_Offer).   
#      Provide 2 examples of statistical modeling methods would be used.  
#      Justify this answer.  

# Example 1: Running Logistic regression to predict 
#       whether customers would accept the offer, using the independent 
#       variable of acpt_offer and the dependent variable that positively 
#       correlated with acpt_offer. Such as the variables of Income, 
#       Crdt_Crd_Avg, CD_Account, Education, Fam_Size, and Sec_Account. 
#       Then, verify the significant relationship at the 95% confidence 
#       level, and further decide whether to reject the null hypothesis.

# Example 2: Using the Decision Tree model to predict whether customers 
#       would accept the bank offer, and generate the Plot Tree to see 
#       the criteria.




