## Code to read the data
payments <- read.csv("payments.csv")

## Code to subset the data to NY
payments_NY <- payments[payments$Provider.State=="NY", ]

## Code to plot 1
covered_charges <- payments_NY$Average.Covered.Charges
total_payments <- payments_NY$Average.Total.Payments
Plot1 <- plot(covered_charges, total_payments, xlab = "Covered Charges Average", ylab = "Total Payments Average", title(main = "Relationship between Covered Charges and Total Payments in NY"))
with(Plot1, {
  lines(loess.smooth(covered_charges, total_payments), col="red")
  })
dev.copy2pdf(file="plot_practice1.pdf")
dev.off()


### my trash
plot(x, y, col = rep(1:3, each = 10), pch = 19)
legend("bottomright", legend = paste("Group", 1:3), col = 1:3, pch = 19, bty = "n")

Relation_Charges_Payments <- tapply(payments_NY$Average.Covered.Charges, payments_NY$Average.Total.Payments, FUN = mean)
barplot(Relation_Charges_Payments, xlab = "Payments", ylab = "Mean", title(main = "Relationship between mean covered charges and mean total payments in New York"))
dev.copy(png, file="plot_practice1.png", height=640, width=640)
dev.off()
mean1 <- mean(payments_NY$Average.Covered.Charges)
mean2 <- mean(payments_NY$Average.Total.Payments)

covered_charges <- payments_NY$Average.Covered.Charges
total_payments <- payments_NY$Average.Total.Payments