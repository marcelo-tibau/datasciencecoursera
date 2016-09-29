# Regression Models Course Project

### Executive Summary
# This paper explores the relationship between miles-per-gallon (MPG) and other variables in the mtcars dataset.The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973-74 models).
# Dataset source provided by: Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391-411.
# The performed analysis attempts to determine whether an automatic or manual transmission is better for MPG and to quantifies the MPG difference. I transcript bellow the received instructions:
# You work for Motor Trend, a magazine about the automobile industry. Looking at a data set of a collection of cars, they are interested in exploring the relationship between a set of variables and miles per gallon (MPG) (outcome). They are particularly interested in the following two questions:
# 1) "Is an automatic or manual transmission better for MPG"
# 2) "Quantify the MPG difference between automatic and manual transmissions"
# This document contains two sections: an Analysis section, where I intend to determine if there is a signficant difference between the mean MPG for automatic and manual transmission cars through a linear regression analysis and an Appendix section where I provide some exploratory analysis and visualizations.

### Analysis

# Codes to perform the data processing, loading the dataset and transforming certain variables into factors.
data("mtcars")
mtcars$cyl  <- factor(mtcars$cyl)
mtcars$vs   <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am   <- factor(mtcars$am,labels=c("Automatic","Manual"))

# Codes to model some Linear Regressions. 
## I will start with a simple Linear Regression to explore miles per gallon ~ transmission and set a null hypothesis that there is no significant difference in mpg between the two groups at alpha = .05.
n <- length(mtcars$mpg)
alpha <- .05
model1 <- lm(mpg ~ am, data = mtcars)
summary(coef(model1))
coef(summary(model1))

## I use the equation beta0 + beta1 to calculate the mpg mean for cars with manual transmissions.I consider the following: beta0/intercept as the mpg mean for cars with automatic transmissions; beta1 is the mean increase in mpg for cars with manual transmissions (am = 1); beta1/am is the mean increase in mpg for cars with manual transmissions.   
# Code to perform the 95% confidence interval for beta1:
tran_est <- coef(summary(model1))["amManual", "Estimate"]
std_err <- coef(summary(model1))["amManual", "Std. Error"]

# code to calculate the stat, using n - 2 to model with intercept and slope:
stat <- qt(1 - alpha/2, n-2)
tran_est + c(-1,1)*(std_err*stat)

# The p-value (2.850207e-04) is small and the confidence interval does not include zero. Therefore I can reject the null hypothesis in favor of the alternative hypothesis that there is a significant difference in mpg between the two groups at alpha = .05.

## Next, I intend to explore a Multiple Regression that includes all variables as predictors of mpg.Then, I will perform a stepwise model selection to pick significant predictors for the final model. 
model2 <- lm(mpg ~ ., data = mtcars)
best_model <- step(model2, direction = "both")
summary(best_model)

# The adjusted R-squared value of 0.84 is the maximum obtained considering all combinations of variables. Thereafter I can conclude that more than 84% of the variability is explained by the best_model.
# To be sure, I intend to compare this best_model with the model1 that brings only the transmission (am) as the predictor variable:
anova(model1, best_model)

# The p-value obtained for the best_model (1.688e-08) is highly significant. Therefore I can conclude that the confounder variables do contribute to the accuracy of the model.

## I intend to do some exploratory analysis to examine the residuals and finding leverage points that show any potential problems with the model.
par(mfrow=c(2, 2))
plot(best_model)
dev.copy(png, file="plot_practice1.png", height=640, width=640)
dev.off()

# As observed at the residual vs fitted plot, the residuals for the Chrysler Imperial, Fiat 128, and Toyota Corolla exert some influence on the shape of the curve.The curve is shaped slightly from normality.
# The Normal Q-Q plot consists of the points which mostly fall on the line indicating that the residuals are normally distributed. The Scale-Location plot consists of points scattered in a constant band pattern, indicating constant variance.
# Finally, there are some distinct points of interest (leverage points) in the top right of the plots that may indicate values of increased leverage of outliers.

## I will perform some statistical inference, using the t-test on the two subsets of mpg data: manual and automatic transmission. I assume that the transmission data has a normal distribution and will test the null hypothesis that the mpg distributions for manual and automatic transmissions are the same. The t-test will the performed on (mpg ~ am).
t.test(mpg ~ am, data = mtcars)

# Based on the t-test results, I reject the null hypothesis. 


## Conclusions drawn from the Analysis: 
# 1) "Is an automatic or manual transmission better for MPG"
# Cars with Manual transmission get 1.8 more miles per gallon compared to cars with Automatic transmission. (1.8 adjusted for hp, cyl, and wt).

# 2) "Quantify the MPG difference between automatic and manual transmissions"

# mpg will decrease by 2.5 for every 1000 lb increase in wt.

# mpg decreases negligibly (only 0.32) with every increase of 10 in hp.

# If number of cylinders, cyl increases from 4 to 6 and 8, mpg will decrease by a factor of 3 and 2.2 respectively (adjusted by hp, wt, and am)

### Appendix
## Boxplot of miles per gallon by transmission type
boxplot(mpg ~ am, data = mtcars, col = (c("#18aae6","#18e62c")), ylab = "Miles Per Gallon", xlab = "Transmission Type")

## Correlations
mtcars_vars <- mtcars[, c(1, 4, 6, 7, 9)]
# Code to save the original values
mar.orig <- par()$mar   
# code to set the new values
par(mar = c(1, 1, 1, 1, 1))   
pairs(mtcars_vars, panel = panel.smooth, col = 9 + mtcars$wt)

## Histogram of the correlations
library(ggplot2)
library(gridExtra)
mpg_dist <- qplot(mtcars_vars$mpg, fill = I("#bc5c52"))
hp_dist <- qplot(mtcars_vars$hp, fill = I("#e6b818"))
wt_dist <- qplot(mtcars_vars$wt, fill = I("#78ccd1"))
qsec_dist <- qplot(mtcars_vars$qsec, fill = I("#d178bd"))
am_dist <- qplot(mtcars_vars$am, fill = I("#5ade0a"))
grid.arrange(mpg_dist, hp_dist, wt_dist, qsec_dist, am_dist, ncol = 2)