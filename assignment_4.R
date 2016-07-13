### 1
## To read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Code to sum the total emissions per year, and then plot using the barplot in order to show the comparison.
Plot_emissions_year <- tapply(NEI$Emissions, NEI$year, FUN = sum)
barplot(Plot_emissions_year, xlab = "Year", ylab = "Emissions Rate", title(main = "Comparing PM25 emissions per year"))
dev.copy(png, file="pm_plot1.png", height=480, width=480)
dev.off()


### 2
## To read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
# Code to subset the data to Baltimore.
Subset_Baltimore <- NEI[NEI$fips=="24510", ]
# Code to sum the total emissions per year and plot the comparison.
Plot_Baltimore <- aggregate(Emissions ~ year, Subset_Baltimore, sum)
barplot(height = Plot_Baltimore$Emissions, names.arg = Plot_Baltimore$year, xlab = "Year", ylab = "Emissions Rate", title(main = "Comparing PM25 emissions per year in Baltimore"))
dev.copy(png, file="pm_plot2.png", height=480, width=480)
dev.off()
  

### 3
## To read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008?
# Code to load the package ggplot2.
library(ggplot2)
# Code to subset the data to Baltimore.
Subset_Baltimore_point <- NEI[NEI$fips=="24510", ]
# Code to subset again, this time per type.
Plot_type <- aggregate(Emissions ~ year + type, Subset_Baltimore_point, sum)
# Code to plot the four types of sources.
p <- ggplot(Plot_type, aes(year, Emissions, color = type))
p <- p + geom_line() + xlab("Year") + ylab("Emissions Rate") + ggtitle("Comparing PM25 emissions in Baltimore per type")
print(p)
dev.copy(png, file="pm_plot3.png", height=480, width=640)
dev.off()


### 4
## To read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Code to load the package ggplot2.
library(ggplot2)
## Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
# Code to merge the datasets.
Merged_datas <- merge(NEI, SCC, by="SCC")
# Code to subset the data to emissions from coal combustion-related.
Subset_Coal <- grepl("coal", Merged_datas$Short.Name, ignore.case = TRUE)
Subset_Coal_Used <- Merged_datas[Subset_Coal, ]
# Code to sum the emissions per year and plot the comparison.
Plot_Coal <- aggregate(Emissions ~ year, Subset_Coal_Used, sum)
p1 <- ggplot(Plot_Coal, aes(factor(year), Emissions))
p1 <- p1 + geom_bar(stat = "identity") + xlab("Year") + ylab("Emissions Rate") + ggtitle("Comparing Emissions from Coal")
print(p1)
dev.copy(png, file="pm_plot4.png", height=480, width=640)
dev.off()


### 5
## To read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Code to load the package ggplot2.
library(ggplot2)
## How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
# Code to merge the datasets.
Merged_datas <- merge(NEI, SCC, by="SCC")
# Code to subset the data to Baltimore and emissions from motor vehicle sources.
Subset_Baltimore_Motor <- Merged_datas[Merged_datas$fips=="24510" & Merged_datas$type=="ON-ROAD", ]
# Code to sum the emissions per year and plot the comparison.
Plot_Motor <- aggregate(Emissions ~ year, Subset_Baltimore_Motor, sum)
p2 <- ggplot(Plot_Motor, aes(factor(year), Emissions))
p2 <- p2 + geom_bar(stat = "identity") + xlab("Year") + ylab("Emissions Rate") + ggtitle("Comparing Emissions from Motor Vehicle Sources in Baltimore")
print(p2)
dev.copy(png, file="pm_plot5.png", height=480, width=640)
dev.off()


### 6
## To read the data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Code to load the package ggplot2.
library(ggplot2)
## Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
# Code to merge the datasets.
Merged_datas <- merge(NEI, SCC, by="SCC")
# Code to subset the data to Baltimore and L.A. and emissions from motor vehicle sources.
Subset_Motor <- Merged_datas[(Merged_datas$fips=="24510"|Merged_datas$fips=="06037") & Merged_datas$type=="ON-ROAD", ]
# Code to sum the emissions per year and plot the comparison.
Plot_Motor <- aggregate(Emissions ~ year + fips, Subset_Motor, sum)
Plot_Motor$fips[Plot_Motor$fips=="24510"] <- "Baltimore, MD"
Plot_Motor$fips[Plot_Motor$fips=="06037"] <- "Los Angeles, CA"
p3 <- ggplot(Plot_Motor, aes(factor(year), Emissions))
p3 <- p3 + facet_grid(. ~ fips)
p3 <- p3 + geom_bar(stat = "identity") +  xlab("Year") + ylab("Emissions Rate") + ggtitle("Comparing Emissions from Motor Vehicle Sources in Baltimore and L.A")
print(p3)
dev.copy(png, file="pm_plot6.png", height=480, width=1040)
dev.off()

