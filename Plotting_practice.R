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


## Code to read the data
payments <- read.csv("payments.csv")

## Code to subset by DRG.Definition and Provider.State and then subset by level
Subset_plot2 <- payments[, c(1, 6, 10, 11)]
Level_NY <- Subset_plot2[Subset_plot2$Provider.State=="NY",]
Level_CA <- Subset_plot2[Subset_plot2$Provider.State=="CA",]
Level_FL <- Subset_plot2[Subset_plot2$Provider.State=="FL",]
Level_IL <- Subset_plot2[Subset_plot2$Provider.State=="IL",]
Level_PA <- Subset_plot2[Subset_plot2$Provider.State=="PA",]
Level_TX <- Subset_plot2[Subset_plot2$Provider.State=="TX",]

Plot2 <- Subset_plot2

## Code to plot 2
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(Plot2, {
  plot(Level_CA$Average.Covered.Charges, Level_CA$Average.Total.Payments, xlab = "Covered Charges Average", ylab = "Total Payments Average", main = "California")+lines(loess.smooth(Level_CA$Average.Covered.Charges, Level_CA$Average.Total.Payments), col="red")
  plot(Level_FL$Average.Covered.Charges, Level_FL$Average.Total.Payments, xlab = "Covered Charges Average", ylab = "Total Payments Average", main = "Florida")+lines(loess.smooth(Level_FL$Average.Covered.Charges, Level_FL$Average.Total.Payments), col="red")
  plot(Level_IL$Average.Covered.Charges, Level_IL$Average.Total.Payments, xlab = "Covered Charges Average", ylab = "Total Payments Average", main = "Illinois")+lines(loess.smooth(Level_IL$Average.Covered.Charges, Level_IL$Average.Total.Payments), col="red")
  plot(Level_NY$Average.Covered.Charges, Level_NY$Average.Total.Payments, xlab = "Covered Charges Average", ylab = "Total Payments Average", main = "New York")+lines(loess.smooth(Level_NY$Average.Covered.Charges, Level_NY$Average.Total.Payments), col="red")
  plot(Level_PA$Average.Covered.Charges, Level_PA$Average.Total.Payments, xlab = "Covered Charges Average", ylab = "Total Payments Average", main = "Pensilvania")+lines(loess.smooth(Level_PA$Average.Covered.Charges, Level_PA$Average.Total.Payments), col="red")
  plot(Level_TX$Average.Covered.Charges, Level_TX$Average.Total.Payments, xlab = "Covered Charges Average", ylab = "Total Payments Average", main = "Texas")+lines(loess.smooth(Level_TX$Average.Covered.Charges, Level_TX$Average.Total.Payments), col="red")
  mtext("Relationship between mean covered charges and mean total payments", outer = TRUE)
})
dev.copy2pdf(file="plot_practice2.pdf")
dev.off()
  


### my trash

#Levels: CA FL IL NY PA TX
sitesToUse <- grepl("firstsite", mydata$mysitenames)
dataframeForThatSite <- mydata[sitesToUse, ]


Levels <- levels(Subset_plot2$Provider.State)

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