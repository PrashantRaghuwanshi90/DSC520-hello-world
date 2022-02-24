# Assignment: ASSIGNMENT 7
# Name: Raghuwanshi Prashant
# Date: 2021-07-24

# Question no 1 : Complete assignment05

library(ggplot2)
theme_set(theme_minimal())

## Set the working directory to the root of your DSC 520 directory

setwd("D:/MS_DataScience/DSC 520-Datastatiics.pdf/dsc520")

## Load the `data/r4ds/heights.csv` to

heights_df <- read.csv("data/r4ds/heights.csv")
head(heights_df)

## Using `cor()` compute correclation coefficients for
## height vs. earn
cor(heights_df$height, heights_df$earn)
### age vs. earn
cor(heights_df$age, heights_df$earn)
### ed vs. earn
cor(heights_df$ed, heights_df$earn)

## Spurious correlation
## The following is data on US spending on science, space, and technology in millions of today's dollars
## and Suicides by hanging strangulation and suffocation for the years 1999 to 2009
## Compute the correlation between these variables
tech_spending <- c(18079, 18594, 19753, 20734, 20831, 23029, 23597, 23584, 25525, 27731, 29449)
suicides <- c(5427, 5688, 6198, 6462, 6635, 7336, 7248, 7491, 8161, 8578, 9000)
cor(tech_spending, suicides)

# Question no 2 Student Survey
##As a data science intern with newly learned knowledge in skills in statistical correlation and R programming, 
##you will analyze the results of a survey recently given to college students.

##The survey data is located in this StudentSurvey.csv file.
stdsry_df <- read.csv("data/student-survey.csv")
head(stdsry_df)

###You learn that the research question being investigated is:
###Is there a significant relationship between the amount of time spent reading and the time spent watching television????
cor(stdsry_df$TimeReading, stdsry_df$TimeTV)
## Ans Negative correlationship shows no relationship between timespent in reading & watching tv

###You are also interested if there are other significant relationships that can be discovered?
library(GGally)
stdsry_mat = data.matrix(stdsry_df)
## Ans please find the other relationships between multiple variables
cor(stdsry_mat)
## Ans please find the other relationships between multiple variables in plots
GGally::ggpairs(stdsry_df)

####Use R to calculate the covariance of the Survey variables and 
####provide an explanation of why you would use this calculation and what the results indicate.
cov(stdsry_mat)
## Ans after analyzing the cov data, it seems most of covariance is showing towards negative directions.and 
## in most of the comparison the variance is showing higher negative value , seems this value is due to difference in scale of measurement
## used, however the variance for gender Vs others or time reading & time tv are showing some relevant value, seems the default scale looks ok with respective compared variables

####Examine the Survey data variables. What measurement is being used for the variables? Explain what effect changing the measurement being used for the variables would have on the covariance calculation.
##Ans we are having 4 different measures, seems time reading & time tv have same scale, 

####Would this be a problem? Explain and provide a better alternative if needed.
##Ans difference in measurement of scale of variable is a problem to determin the covariance between the variables
## to over come this issue usually we are performing standarization process

####Choose the type of correlation test to perform, explain why you chose this test, 
####and make a prediction if the test yields a positive or negative correlation?

##ANS : Correlation test is used to evaluate the association between two or more variables.
## here first i am testing the mean between variables to verify the noremal distributions
##T-TEST  (two sample test T test and paired two sample t-test)
## two sample t-test is can be performed between gender & happiness variable
## paired two sample test test can be perfromed between time reading & timetv
##variance of each group
aggregate(Happiness ~ Gender, data = stdsry_df, var)
## normality of happyness distributions
##value of the Shapiro-Wilk Test is greater than 0.05, the data is normal
shapiro.test(stdsry_df$Happiness)
shapiro.test(stdsry_df$Happiness[stdsry_df$Gender==1])
shapiro.test(stdsry_df$Happiness[stdsry_df$Gender==0])
t.test(Happiness ~ Gender, stdsry_df, var.equal=TRUE)
# here iam using Pearson correlation (r), 
##which measures a linear dependence between two variables (x and y). 
## It???s also known as a parametric correlation test because it depends to the distribution of the data. 
##It can be used only when x and y are from normal distribution. 
##The plot of y = f(x) is named the linear regression curve
cor.test(stdsry_df$TimeReading, stdsry_df$TimeTV, method=c("pearson"))
## no Relation ship, negative cor
cor.test(stdsry_df$TimeReading, stdsry_df$Gender, method=c("pearson"))
## no Relation ship, negative cor
cor.test(stdsry_df$Happiness, stdsry_df$Gender, method=c("pearson"))
## good Relation ship, positive cor
####Perform a correlation analysis of:
####  All variables
cor(stdsry_mat)
####A single correlation between two a pair of the variables
cor.test(stdsry_df$TimeReading, stdsry_df$TimeTV, method=c("pearson"))
## no Relation ship, negative cor
cor.test(stdsry_df$TimeReading, stdsry_df$Gender, method=c("pearson"))
## no Relation ship, negative cor
cor.test(stdsry_df$Happiness, stdsry_df$Gender, method=c("pearson"))
####Repeat your correlation test in step 2 but set the confidence interval at 99%
cor.test(stdsry_df$TimeReading, stdsry_df$TimeTV, method=c("pearson"))
## no Relation ship, negative cor
cor.test(stdsry_df$TimeReading, stdsry_df$Gender, method=c("pearson"))
## no Relation ship, negative cor
cor.test(stdsry_df$Happiness, stdsry_df$Gender, method=c("pearson"),conf.level = 0.99 )
## good Relation ship, positive cor
cor.test(stdsry_df$TimeReading, stdsry_df$TimeTV, method=c("pearson"),conf.level = 0.99 )
## no Relation ship, negative cor
cor.test(stdsry_df$TimeReading, stdsry_df$Gender, method=c("pearson"), conf.level = 0.99 )
####Describe what the calculations in the correlation matrix suggest about the relationship between the variables. Be specific with your explanation.
####Calculate the correlation coefficient and the coefficient of determination, describe what you conclude about the results.
cov(stdsry_mat)
### Ans after analyzing the cov data, it seems most of covariance is showing towards negative directions.and 
### in most of the comparison the variance is showing higher negative value , seems this value is due to difference in scale of measurement
### used, however the variance for gender Vs others or time reading & time tv are showing some relevant value, seems the default scale looks ok with respective compared variables
###Based on your analysis can you say that watching more TV caused students to read less? Explain.
### Ans : based on negative correlation , seems their is no relationship between reading and watching tv
####Pick three variables and perform a partial correlation, documenting which variable you are ???controlling???. 
####Explain how this changes your interpretation and explanation of the results.
library(ppcor)
pcor(stdsry_mat)
pcor.test(stdsry_df$TimeReading, stdsry_df$TimeT, stdsry_df$Happiness)
pcor.test(stdsry_df$TimeReading, stdsry_df$TimeT, stdsry_df$Gender)
### Ans time reading and time telivison is having partial correlation while controlling for the effect for gender or happinesss
