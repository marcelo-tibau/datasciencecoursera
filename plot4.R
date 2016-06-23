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

## Function to plot the selected data from "Global Active Power" to show the kilowatts variation per day
data_plot_4 <- function() { par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
  with(data_selected, {
    plot(Global_active_power~data_selected_time, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
    plot(Voltage~data_selected_time, type="l", 
       ylab="Voltage (volt)", xlab="")
    plot(Sub_metering_1~data_selected_time, type="l", 
       ylab="Energy sub metering", xlab="")
    lines(Sub_metering_2~data_selected_time,col='Red')
    lines(Sub_metering_3~data_selected_time,col='Blue')
    legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n", cex = .5,
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    plot(Global_reactive_power~data_selected_time, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})
  dev.copy(png, file="plot4.png", height=480, width=480)
  dev.off()
}
