### Statistical Inference Course Project

### Part 1: Simulation Exercise

## In this project I investigate the exponential distribution in R and compare it with the Central Limit Theorem.
## I also provide the explanations and codes to assure the reproducibility of the investigation. 
## This report is based at the distribution of averages of 40 exponentials and a thousand simulations.

# Codes to set the chosen parameters. The parameters for lambda, number of exponentials and simulations were instructed.
set.seed(2016)
lambda <- .2
exponumber <- 40
simunumber <- 1000
data_simulation <- matrix(rexp(simunumber*exponumber, rate = lambda), simunumber, exponumber)

# Code to plot the exponential distribution:
plot(rexp(1000, lambda), pch = 20, cex = .6, main = "The exponential distribution with rate 0.2 and 1.000 observations",  col = "#2e8130")

# Code to plot the averages of 40 exponentials in a histogram:
averages <- rowMeans(data_simulation)
hist(averages, xlab = "Averages", ylab = "Frequence", main = "Histogram from the averages of 40 exponentials", col = "#2e8130")

## 1. Sample Mean versus Theoretical Mean
# Codes to calculate the sample mean and the theoretical mean of the distribution and to plot the comparison:
sample_mean <- mean(averages)
cat("Sample Mean: ", sample_mean)

theo_mean <- 1/lambda
cat("Theoretical Mean: ", theo_mean)

hist(averages, col="#2e8130", main="Sample vs Theoretical Mean", breaks=20)
l1 <- abline(v=mean(theo_mean), lwd="4", col="#c728c9")
l2 <- abline(v=mean(sample_mean), lwd="2", col="#f3eb17")
legend(6.2, 90, c("Theoratical", "Sample"), lty = c(1, 1), lwd = c(2.5, 2.5), col = c("#c728c9", "#f3eb17"))

## 2. Sample Variance versus Theoretical Variance
# Codes to calculate the variances (both sample and theoretical):
sample_var <- var(averages)
cat("Sample Variance: ", sample_var)

theo_var <- (1/lambda)^2/exponumber
cat("Theoratical Variance: ", theo_var)

# As we can see above, the variances are very close.

## 3. Distribution analysis
# Code to create an approximate normal distribution and aligns with the the sample:
hist(averages, density = 20, breaks = 20, prob = TRUE, xlab = "40 exponentials averages", ylab = "Frequence", main = "Distribution Analysis", col = "#f0e8f0")
curve(dnorm(x, mean = sample_mean, sd = sqrt(sample_var)), col = "#e6b818", lwd=2, lty = "dotted", add = TRUE, yaxt="n")
curve(dnorm(x, mean = theo_mean, sd = sqrt(theo_var)), col = "#18aae6", lwd=2, lty = "dotted", add = TRUE, yaxt="n")

# As seen above, the histogram can be approximated with the normal distribution.
# To be really sure, I propose to investigate if the exponential distribution is approximately normal.
# Due to the Central Limit Theorem, the averages of the sample mean should follow a normal distribution.

# Code to plot the exponential distribution.
hist(averages, prob = TRUE, col = "#f0e8f0", main = "Exponential Distribution", density = 20, breaks = 20)
lines(density(averages), lwd = 2, col = "#e6b818")

# The Distribution plot and the Exponential Distribution plot are similar. Therefore, I can infer that the distribution is approximately normal.

### Part 2: Basic Inferantial Data Analysis

### Overview
## In this report I will analyse the ToothGrowth data in the R data sets package. 
## The data originally analysis the effect of vitamin C on tooth growth in Guinea Pigs.
## The study's description reports that the response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, (orange juice or ascorbic acid (a form of vitamin C and coded as VC).
## As a format, it's a data frame with 60 observations on 3 variables:

#[,1]	len	numeric	Tooth length
#[,2]	supp	factor	Supplement type (VC or OJ)
#[,3]	dose	numeric	Dose in milligrams/day

## The source is:
# C. I. Bliss (1952) The Statistics of Bioassay. Academic Press.

## As additional references: 
# McNeil, D. R. (1977) Interactive Data Analysis. New York: Wiley.
# Crampton, E. W. (1947) The growth of the odontoblast of the incisor teeth as a criterion of vitamin C intake of the guinea pig. The Journal of Nutrition 33(5): 491-504.

### Analysis

# Codes to load the necessary library and dataset. I will also provide a quick summary to show the format stated at the Overview intro.
library(ggplot2)
data("ToothGrowth")
summary(ToothGrowth)

# Codes to perform some basic exploratory data analyses.  I will focus on just a few points, which are the first few rows; the unique values of the columns and a short list of the factors.

head(ToothGrowth)

unique(ToothGrowth$len)

unique(ToothGrowth$supp)

unique(ToothGrowth$dose)

by(ToothGrowth$len, INDICES = list(ToothGrowth$supp, ToothGrowth$dose), summary)

# Codes to perform some exploratory analysis. I intend to specify what will be analyze at the header of which chunk of code.  

# Analysis of the correlation between Dosage (dose) and Supplement (supp).
average <- aggregate(len~., data = ToothGrowth, mean)
ggplot(data = ToothGrowth, aes(x = dose, y = len)) +
        geom_point(aes(group = supp, colour = supp, size = 2, alpha = .6)) +
        geom_line(data = average, aes(group = supp, colour = supp)) +
        labs(title = "Correlation between Dosage and Supplement")

# It seems that higher the dosage, the longer the tooth grows. The dosages are similar for both supplements at 2mg, but OJ has a better effect on teeth growth than VC.
# To confirm this correlation I will perform some hypothesis tests.

# Analysis of the relationship between Tooth Lenght (len) and Supplement (supp).
ggplot(aes(x = supp, y = len), data = ToothGrowth) +
        geom_boxplot(aes(fill = supp)) +
        labs(title = "Relationship between Tooth Lenght and Supplement")

# Supporting the first inference, the plot above shows that OJ has a better effect on teeth growth than VC.

# Analysis of the relationship between Tooth Lenght (len) and Dosage (dose).
ggplot(aes(x = factor(dose), y = len), data = ToothGrowth) +
        geom_boxplot(aes(fill = factor(dose))) +
        labs(title = "Relationship between Tooth Lenght and Dosage")

# Although the plot shows that higher the dosage, better the effect on teeth growth, it seems that there is no directly correlation. Therefore not supporting the first inference. 
# To check if there is so, the following analysis intends to determine if within Dosage, the Supplements have different effects on teeth growth.

# Analysis of the impact of Dosage (dose) and Supplement (supp) on Tooth Lenght (len).
ggplot(aes(x = supp, y = len), data = ToothGrowth) +
        geom_boxplot(aes(fill = supp)) +
        facet_wrap(~dose) +
        labs(title = "Impact of Dosage and Supplement on Tooth Lenght")

# The plot above shows that ideed there is a correlation between dosage and supplement on teeth growth.

# I also wish to compare tooth growth by supplement. To perform this analysis I will find the confidence interval for a 95% confidence level using the t-test technique.

# Analysis using a t-test to compare Tooth Lenght (len) and Supplement (supp).
t.test(len~supp, data = ToothGrowth)

# Since the p-value is higher than 0.05, the data is likely with a true null. To be sure I will compare tooth growth by dosage, checking at the different pairs of dose values.

# T-test using dose amounts 0.5 and 1.0
test_set_one <- subset(ToothGrowth, ToothGrowth$dose %in% c(1.0, .5))
t.test(len~dose, data = test_set_one)

# T-test using dose amounts 0.5 and 2.0
test_set_two <- subset(ToothGrowth, ToothGrowth$dose %in% c(2.0, .5))
t.test(len~dose, data = test_set_two)

# T-test using dose amounts 1.0 and 2.0
test_set_three <- subset(ToothGrowth, ToothGrowth$dose %in% c(2.0, 1.0))
t.test(len~dose, data = test_set_three)

# Given the all-negative confidence interval that doesn't include 0 of each test  and the very small p-value, I can infer that the average tooth length increases with an inceasing dosage.
# Consequently the null hypothesis can be rejected.

### Conclusions

# Assuming that the populations are independent, the variances between populations are different, a random population was used, the population was comprised of similar guinea pigs, measurement error was accounted for with significant digits, and double blind research methods were used.
# Also assuming that those assumptions are true, I may infer that there is a significant difference between tooth length and dosage levels across both supplement delivery methods. A higher dosage level consistently led to longer teeth.
# Initially it appeared that the supplement delivery method had no significant impact on tooth length, but when controlling the dose level I discovered that there was a significant difference at 0.5mg and 1.0mg, but not at 2.0mg. 
# Based on this evidence, it appears that orange juice (OJ) is a better supplement method with a larger impact on tooth length for a given dose of Vitamin C (VC), but above a maximum dose level there is no further improvement.
