# Assignment: ASSIGNMENT 8_2 Exercise
# Name: Raghuwanshi Prashant
# Date: 2021-07-30

# Question no 1 : Complete assignment06

library(ggplot2)
theme_set(theme_minimal())
library(readxl)
## Set the working directory to the root of your DSC 520 directory

setwd("D:/MS_DataScience/DSC 520-Datastatiics.pdf/dsc520")

## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("data/r4ds/heights.csv")

## Fit a linear model using the `age` variable as the predictor and `earn` as the outcome
age_lm <-  lm(heights_df$earn ~ heights_df$age, data = heights_df)

## View the summary of your model using `summary()`
summary(age_lm)

## Creating predictions using `predict()`
age_predict_df <- data.frame(earn = predict(age_lm, newdata= as.list(heights_df$age)), age=heights_df$age)

## Plot the predictions against the original data
ggplot(data = heights_df, aes(y = earn, x = age)) +
  geom_point(color='blue') +
  geom_line(color='red',data = age_predict_df, aes(y=earn, x=age))

mean_earn <- mean(heights_df$earn)
## Corrected Sum of Squares Total
sst <- sum((mean_earn - heights_df$earn)^2)
## Corrected Sum of Squares for Model
ssm <- sum((mean_earn - age_predict_df$earn)^2)
## Residuals
residuals <- heights_df$earn - age_predict_df$earn
## Sum of Squares for Error
sse <- sum(residuals^2)
## R Squared R^2 = SSM\SST
r_squared <- ssm/sst

## Number of observations
n <- length(heights_df)
## Number of regression parameters
p <- 2
## Corrected Degrees of Freedom for Model (p-1)
dfm <- p-1
## Degrees of Freedom for Error (n-p)
dfe <- n-p
## Corrected Degrees of Freedom Total:   DFT = n - 1
dft <- n-1

## Mean of Squares for Model:   MSM = SSM / DFM
msm <- ssm/dfm
## Mean of Squares for Error:   MSE = SSE / DFE
mse <- sse/dfe
## Mean of Squares Total:   MST = SST / DFT
mst <- sst/dft
## F Statistic F = MSM/MSE
f_score <- msm/mse

## Adjusted R Squared R2 = 1 - (1 - R2)(n - 1) / (n - p)
adjusted_r_squared <- (1 - (1 - r_squared)*(n -1)/(n-p))
adjusted_r_squared
## Calculate the p-value from the F distribution
p_value <- pf(f_score, dfm, dft, lower.tail=F)

# Question no 2 : Complete assignment07

## Fit a linear model
earn_lm <-  lm(heights_df$earn ~ heights_df$age + heights_df$ed + heights_df$race + heights_df$height + heights_df$sex, data=heights_df)

## View the summary of your model
summary(earn_lm)

predicted_df <- data.frame(
  earn = predict(earn_lm, newdata= as.list(heights_df$earn)),
  ed=heights_df$ed, race=heights_df$race, height=heights_df$height,
  age=heights_df$age, sex=heights_df$sex
)
## Compute deviation (i.e. residuals)
mean_earn <- mean(heights_df$earn)
## Corrected Sum of Squares Total
sst <- sum((mean_earn - heights_df$earn)^2)
## Corrected Sum of Squares for Model
ssm <- sum((mean_earn - predicted_df$earn)^2)
## Residuals
residuals <- heights_df$earn - predicted_df$earn
## Sum of Squares for Error
sse <- sum(residuals^2)
## R Squared R^2 = SSM\SST
r_squared <- ssm/sst

## Number of observations
n <- length(heights_df)
## Number of regression parameters
p <- 8
## Corrected Degrees of Freedom for Model (p-1)
dfm <- p-1
## Degrees of Freedom for Error (n-p)
dfe <- n-p
## Corrected Degrees of Freedom Total:   DFT = n - 1
dft <- n-1

## Mean of Squares for Model:   MSM = SSM / DFM
msm <- ssm/dfm
## Mean of Squares for Error:   MSE = SSE / DFE
mse <- sse/dfe
## Mean of Squares Total:   MST = SST / DFT
mst <- sst/dft
## F Statistic F = MSM/MSE
f_score <- msm/mse

## Adjusted R Squared R2 = 1 - (1 - R2)(n - 1) / (n - p)
adjusted_r_squared <- (1 - (1 - r_squared)*(n -1)/(n-p))
adjusted_r_squared
## Calculate the p-value from the F distribution
p_value <- pf(f_score, dfm, dft, lower.tail=F)

# Question no 3 : Housing Data
## Work individually on this assignment. You are encouraged to collaborate on ideas and strategies pertinent to this assignment. Data for this assignment is focused on real estate transactions recorded from 1964 to 2016 and can be found in Housing.xlsx. 
## Using your skills in statistical correlation, multiple regression, and R programming, you are interested in the following variables: Sale Price and several other possible predictors.
## If you worked with the Housing dataset in previous week ??? you are in luck, you likely have already found any issues in the dataset and made the necessary transformations. If not, you will want to take some time looking at the data with all your new skills and identifying if you have any clean up that needs to happen.
## Complete the following:

library(readxl)
library(dplyr)

## Set the working directory to the root of your DSC 520 directory

setwd("D:/MS_DataScience/DSC 520-Datastatiics.pdf/dsc520")

## Load the `data/r4ds/heights.csv` to
housing_df <- read_excel("data/week-7-housing.xlsx", sheet = "Sheet2")
summary(housing_df)
### Explain any transformations or modifications you made to the dataset
### ANS , havent made any transformation to dataset

### Create two variables; one that will contain the variables Sale Price and Square Foot of Lot (same variables used from previous assignment on simple regression) and one that will contain Sale Price and several additional predictors of your choice. 
### Explain the basis for your additional predictor selections.
given_variable <- select(housing_df, Sale_Price, sq_ft_lot)
new_variable <- select(housing_df, Sale_Price, sq_ft_lot, building_grade, year_built, square_feet_total_living)
### ANS Here i have used year build , building grades, square_feet_total_living --i believe sales price may vary with the listed additonal variables

###Execute a summary() function on two variables defined in the previous step to compare the model results.
summary(given_variable)
summary(new_variable)

##What are the R2 and Adjusted R2 statistics? Explain what these results tell you about the overall model.
##Did the inclusion of the additional predictors help explain any large variations found in Sale Price?
given_variable_lm <-  lm(formula = Sale_Price ~ sq_ft_lot, data = given_variable)
summary(given_variable_lm)
new_variable_lm <-  lm(Sale_Price ~ sq_ft_lot + building_grade + year_built + square_feet_total_living, data=new_variable)
summary(new_variable_lm)
### ANS Both R2 and the adjusted R2 give us an idea of how many data points fall within the line of the regression equation. 
### R2 assumes that every single variable explains the variation in the dependent variable. 
### The adjusted R2 tells us the percentage of variation explained by only the independent variables that actually affect the dependent variable.
### here i have got below r-squared values by using 1 variable and 3 variable
### Multiple R-squared:  0.01435,	Adjusted R-squared:  0.01428 
### Multiple R-squared:  0.2233,	Adjusted R-squared:  0.223
### conclusion , increasing independent variables counts increases  r-squared value, r sq & R adj Sq values are same for 3 variables

### Considering the parameters of the multiple regression model you have created. 
### What are the standardized betas for each parameter and what do the values indicate?
library(QuantPsyc)
lm.beta(new_variable_lm)
### ANS The higher the absolute value of the beta coefficient, the stronger the effect. here square feet total living have
### highest beta value and sq ft lot is having lowest

### Calculate the confidence intervals for the parameters in your model and explain what the results indicate.
confint(new_variable_lm, level=0.95)
### ANS here we got the upper limit & lower limit of parameters for confidence interval== 95%

### Assess the improvement of the new model compared to your original model (simple regression model) 
### by testing whether this change is significant by performing an analysis of variance.
new_variable_aov <- anova(given_variable_lm, new_variable_lm, test="Chisq")
new_variable_aov
## ANS seems residual is value is decreasing and Pr value is leass that .0001 seems new variable model is bestfit

### Perform casewise diagnostics to identify outliers and/or influential cases, 
###  storing each function's output in a dataframe assigned to a unique variable name.
OutVals = boxplot(new_variable)$out
### ANS seems we have outliers on sales price and sq_ft_lot variabes
OutVals1 = boxplot(given_variable)$out
class(OutVals1)
library(car)
outlierTest(new_variable)
### Calculate the standardized residuals using the appropriate command, specifying those that are +-2, storing the results of large residuals in a variable you create.
given_variable_residuals = rstandard(given_variable_lm)
given_variable_residuals
plot(given_variable_lm, which = 2)
### Use the appropriate function to show the sum of large residuals.
###Which specific variables have large residuals (only cases that evaluate as TRUE)?
OutVals = boxplot(new_variable)$out
### ANS sales_price & sq_ft_lot variable is having large residual

### Investigate further by calculating the leverage, cooks distance, and covariance rations. Comment on all cases that are problematics.
new_variable_leverage <- as.data.frame(hatvalues(new_variable_lm))
### Ans leverage by observations
### sort observations by leverage, descending
new_variable_leverage[order(-new_variable_leverage['hatvalues(new_variable_lm)']), ]
### We can see that the largest leverage value is 0.0621723410. Since this isn???t greater than 2, we know that none of the observations in our dataset have high leverage
new_variable_cooksd <- cooks.distance(new_variable_lm)

# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(new_variable)
plot(new_variable_cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(new_variable_cooksd)+1, y=new_variable_cooksd, labels=ifelse(new_variable_cooksd>4/sample_size, names(new_variable_cooksd),""), col="red")  # add labels
### Removing Outliers
### influential row numbers
new_variable_influential <- as.numeric(names(new_variable_cooksd)[(new_variable_cooksd > (4/sample_size))])

new_variable_screen <- new_variable[- new_variable_influential, ]

plot3 <- ggplot(data = new_variable, aes(x = Sale_Price, y = sq_ft_lot)) +
  geom_point() + 
  geom_smooth(method = lm) +
  xlim(0, 400000) + ylim(0, 75000) + 
  ggtitle("Before")
plot4 <- ggplot(data = new_variable_screen, aes(x = Sale_Price, y = sq_ft_lot)) +
  geom_point() + 
  geom_smooth(method = lm) +
  xlim(0, 400000) + ylim(0, 75000) + 
  ggtitle("After")
library(gridExtra)
gridExtra::grid.arrange(plot3, plot4, ncol=2)

### Perform the necessary calculations to assess the assumption of independence and state if the condition is met or not.
### Checking the assumption of constant variance of residuals (Homoscedasticity)
spreadLevelPlot(new_variable_lm)
### Checking the assumption of normality of residuals
qqPlot(new_variable_lm, main="QQ Plot")
### Perform the necessary calculations to assess the assumption of no multicollinearity and state if the condition is met or not.
### Checking for multicollinearity
vif(new_variable_lm) # variance inflation factors 
sqrt(vif(new_variable_lm)) > 2 #checks if the VIF's in your model are > 2, usually indicating a problem if TRUE
### ANS It appears that the VIF for both of our predictors is less than 2, so we have met the assumption of multicollinearity for this model

### Visually check the assumptions related to the residuals using the plot() and hist() functions. Summarize what each graph is informing you of and if any anomalies are present.
plot(new_variable_lm)
### This output provides us four useful plots:
### Residuals vs Fitted Values, to check constant variance in residuals and linearity of association between predictors and outcome (look for a relatively straight line and random-looking scatterplot).
### Normal Q-Q Plot, to check the assumption of normally distributed residuals.
### Root of Standardized residuals vs Fitted values, this is very similar to number 1, where the Y axis of residuals is in a different metric.
### Residuals vs Leverage, to check if the leverage of certain observations are driving abnormal residual distributions, thus violating assumptions and biasing statistical tests. Potentially problematic points will be labelled in the plot.
