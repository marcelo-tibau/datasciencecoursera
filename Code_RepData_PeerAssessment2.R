### Code to download the dataset and save it at working directory
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "./repdata%2Fdata%2FStormData.csv.bz2")

### Code to load the data
storm_data_full <- read.csv("repdata%2Fdata%2FStormData.csv.bz2")

# Question 1: Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

## Data Processing
### Code to check the names of all columns
names(storm_data_full)

### Code to create a new dataset optimized to answer the question.
storm_data_optim <- subset(storm_data_full, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0, select = c(8, 23:28))

### Code to sum the fatalities per event type and order it by the number of deceased.
storm_fatalities <- aggregate(FATALITIES ~ EVTYPE, data = storm_data_optim, sum, na.rm = TRUE)
names(storm_fatalities) <- c("EVENT_TYPE", "FATALITIES")
storm_fatalities <- storm_fatalities[order(-storm_fatalities$FATALITIES), ]

### Code to sum the injuries per event type and order it by the number of wounded.
storm_injuries <- aggregate(INJURIES ~ EVTYPE, data = storm_data_optim, sum, na.rm = TRUE)
names(storm_injuries) <- c("EVENT_TYPE", "INJURIES")
storm_injuries <- storm_injuries[order(-storm_injuries$INJURIES), ]

## Results
### Codes to load the package, build the plots and combine in multiple plots.
library(ggplot2)
p1 <- ggplot(storm_fatalities[1:10, ], aes(x = reorder(EVENT_TYPE, FATALITIES), y = FATALITIES))+
        geom_bar(stat = "identity", fill = "#cd7dd4", colour = "#cd7dd4")+
        coord_flip()+
        labs(x = "Event Type", y = "Fatalities", title = "Most Fatal Weather Events")

p2 <- ggplot(storm_injuries[1:10, ], aes(x = reorder(EVENT_TYPE, INJURIES), y = INJURIES))+
        geom_bar(stat = "identity", fill = "#7dd4c3", colour = "#7dd4c3")+
        coord_flip()+
        labs(x = "Event Type", y = "Injuries", title = "Most Woundable Weather Events")

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
dev.copy(png, file="fatalities_injuries.png", height=680, width=1040)
dev.off()

# Question 2: Across the United States, which types of events have the greatest economic consequences?
