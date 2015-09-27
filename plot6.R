#Load Data from working fles in Current working Directory
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


require(reshape2)
require(ggplot2)
# Plot to answer the question
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

#find all of the motor vehicle realates sources
#webster defined motor vehicle as an automotive vehicle not operated on rails;
#especially :  one with rubber tires for use on highways
# Motor vecleis to include catagories caught by regular expression below
motor_vehicle_scc<-SCC$SCC[regexpr("Mobile - On-Road", SCC$EI.Sector)>0]

#subset polution data to include only motor vehicle sources in Baltimore City and Los Angeles
NEI_motor_vehicle <- NEI[NEI$SCC %in% motor_vehicle_scc & (NEI$fips=="24510" | NEI$fips=="06037"),]

#setup factors for nice fips names
NEI_motor_vehicle$ffips <- factor(NEI_motor_vehicle$fips, c("24510", "06037"), c("Baltimore_City", "Los_Angeles_County"))
# melt data frame
mNEI_motor_vehicle <- melt(NEI_motor_vehicle, id=c("ffips", "year"))
foo<-mNEI_motor_vehicle[mNEI_motor_vehicle$variable=="Emissions",]
foo$value <- as.numeric(foo$value)
mcNEI_motor_vehicle <- dcast(foo, year+ffips~variable, mean)


# creat ggplot object with the base data and title
mplot <- ggplot(mcNEI_motor_vehicle, aes(year, Emissions, ffips)) + ggtitle("Total Emissions from Motor Vehicles")


#add points and add facets
#mplot <- mplot + geom_point()+facet_wrap(~ type, ncol=2, scales = "free_y")
mplot <- mplot + geom_point()

mplot <- mplot + facet_wrap(~ffips, scales = "fixed") + geom_point()


#add regression line
mplot <- mplot + geom_smooth(method="auto")
#
# open model to plot to file
png(filename = "plot6.png")

#plot the data with axis labels
plot(mplot)

#close open device
dev.off()
