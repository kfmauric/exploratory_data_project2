#Fifth plot for project 2
#Load Data from working fles in Current working Directory
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot to answer the question

# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

#find all of the motor vehicle realates sources
#webster defined motor vehicle as an automotive vehicle not operated on rails;
#                           especially :  one with rubber tires for use on highways
# Motor vecleis to include catagories caught by regular expression below
motor_vehicle_scc<-SCC$SCC[regexpr("Mobile - On-Road", SCC$EI.Sector)>0]

#subset polution data to include only motor vehicle sources and Baltimore City
NEI_motor_vehicle <- NEI[NEI$SCC %in% motor_vehicle_scc & NEI$fips=="24510",]
total_pm2p5_tmp <-tapply(NEI_motor_vehicle$Emissions, NEI_motor_vehicle$year, sum)
total_pm2p5 <- data.frame(year = as.numeric(names(total_pm2p5_tmp)), polution = total_pm2p5_tmp)

#
# open model to plot to file
png(filename = "plot5.png")

#plot the data with axis labels
plot(names(total_pm2p5_tmp), total_pm2p5_tmp, xlab="Year", ylab="PM2.5 Emision in tons", pch=19, col="red")
#plot(total_pm2p5$year, total_pm2p5$polution, xlab="Year", ylab="PM2.5 Emision in tons")
#add a title
title("Total Emissions from Motor Vehicle Srcs. in Baltimore City, MD")

#Add a regression line
x<-as.numeric(names(total_pm2p5_tmp))
y<-as.vector(total_pm2p5$polution)
model <- lm(y~x)
abline(model, lwd=1)

#close open device
dev.off()
