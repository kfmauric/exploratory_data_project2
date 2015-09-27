# First Plot for Exploratory Data Analysis Project 2

#Load Data from working fles in Current working Directory
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# This plot should answer the question Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years
# 1999, 2002, 2005, and 2008.

#summarize data by year
total_pm2p5_tmp <-tapply(NEI$Emissions, NEI$year, sum)
total_pm2p5 <- data.frame(year = as.numeric(names(total_pm2p5_tmp)), polution = total_pm2p5_tmp)

# open model to plot to file
png(filename = "plot1.png")

#plot the data with axis labels
plot(names(total_pm2p5_tmp), total_pm2p5_tmp, xlab="Year", ylab="PM2.5 Emision in tons", pch=19, col="red")
#plot(total_pm2p5$year, total_pm2p5$polution, xlab="Year", ylab="PM2.5 Emision in tons")
#add a title
title("Total Emissions from PM2.5 in the United States")

#Add a regression line
x<-as.numeric(names(total_pm2p5_tmp))
y<-as.vector(total_pm2p5$polution)
model <- lm(y~x)
abline(model, lwd=1)

#close open device
dev.off()

