## Code to read and get the full dataset
eletric_power <- read.csv("household_power_consumption.txt", header=T, sep=';', na.strings="?", 
                          nrows=2075259, check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
eletric_power$Date <- as.Date(eletric_power$Date, format="%d/%m/%Y")

## Code to subset the data into data_selected
data_selected <- subset(eletric_power, subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
rm(eletric_power)

## Code to convert the dates in data_selected to the pattern defined by data_selected_time 
data_selected_time <- paste(as.Date(data$Date), data$Time)
data_selected$data_selected_time <- as.POSIXct(data_selected_time)

## Function to plot the selected data from "Global Active Power" to show the kilowatts variation
data_plot_2 <- function() {
  plot(data_selected$Global_active_power~data_selected$data_selected_time, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  dev.copy(png, file="plot2.png", height=480, width=480)
  dev.off()
}
