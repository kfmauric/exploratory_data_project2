# Fourth Plot for Project 2
require(reshape2)
require(ggplot2)
#Load Data from working fles in Current working Directory
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot to answer the question
#Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

#find al of the coal realates sources
coal_scc<-SCC$SCC[regexpr("Coal", SCC$EI.Sector)>0]

#subset polution data to include only coal sources
NEI_coal <- NEI[NEI$SCC %in% coal_scc,]
#Summarize with sum
#There are catagories for pollution sources that have very different distributions
#Lots of small measurements for cars do not mean forest fires are noise
total_pm2p5_tmp <-tapply(NEI_coal$Emissions, NEI_coal$year, sum)
total_pm2p5 <- data.frame(year = as.numeric(names(total_pm2p5_tmp)), polution = total_pm2p5_tmp)

#
# open model to plot to file
png(filename = "plot4.png")

#plot the data with axis labels
plot(names(total_pm2p5_tmp), total_pm2p5_tmp, xlab="Year", ylab="PM2.5 Emision in tons", pch=19, col="red", type = "o")
#plot(total_pm2p5$year, total_pm2p5$polution, xlab="Year", ylab="PM2.5 Emision in tons")
#add a title
title("Total Emissions from Coal Sources in the United States")

#Add a regression line
x<-as.numeric(names(total_pm2p5_tmp))
y<-as.vector(total_pm2p5$polution)
model <- lm(y~x)
# Not sure the regression line adds anything
#abline(model, lwd=1)

#close open device
dev.off()
