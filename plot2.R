# Second Plot for Exploratory Data Analysis Project 2

#Load Data from working fles in Current working Directory
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# This plot should answer the question Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from # 1999 to 2008?


#subset the data for baltimore
NEI_baltimore <- NEI[NEI$fips=="24510",]

#summarize data by year
total_pm2p5_tmp <-tapply(NEI_baltimore$Emissions, NEI_baltimore$year, sum)
total_pm2p5 <- data.frame(year = as.numeric(names(total_pm2p5_tmp)), polution = total_pm2p5_tmp)



# open model to plot to file
png(filename = "plot2.png")

#plot the data with axis labels
plot(names(total_pm2p5_tmp), total_pm2p5_tmp, xlab="Year", ylab="PM2.5 Emision in tons", pch=19, col="red")
#add a title
title("Total Emissions from PM2.5 in Baltimore City, Maryland")

#Add a regression line
x<-as.numeric(names(total_pm2p5_tmp))
y<-as.vector(total_pm2p5$polution)
model <- lm(y~x)
abline(model, lwd=1)

#plot the data with axis labels

#close open device
dev.off()

