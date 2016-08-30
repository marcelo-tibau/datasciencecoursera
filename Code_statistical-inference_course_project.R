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

