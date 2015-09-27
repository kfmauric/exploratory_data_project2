# First Plot for Exploratory Data Analysis Project 2

#Load Data from working fles in Current working Directory
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Plot should answer the following question
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have
#seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the
#ggplot2 plotting system to make a plot answer this question.

#subset the data for baltimore
NEI_baltimore <- NEI[NEI$fips=="24510",]

#need to filter  outliers for pretty plots
# leave zero as low limit
#high limit is Q3 + 1.5*IQR
up_limit<-as.numeric(by(NEI_baltimore$Emissions, NEI_baltimore$type, function(x) quantile(x, probs = 0.75)+1.5*IQR(x)))
NEI_baltimore_testa <- NEI_baltimore[(NEI_baltimore$Emissions<=2)&(NEI_baltimore$type=="NON-ROAD"),]
NEI_baltimore_testb <- NEI_baltimore[(NEI_baltimore$Emissions<=70)&(NEI_baltimore$type=="NONPOINT"),]
NEI_baltimore_testc <- NEI_baltimore[(NEI_baltimore$Emissions<=1)&(NEI_baltimore$type=="ON-ROAD"),]
NEI_baltimore_testd <- NEI_baltimore[(NEI_baltimore$Emissions<=8)&(NEI_baltimore$type=="POINT"),]

NEI_baltimore_foot <- rbind(NEI_baltimore_testd, NEI_baltimore_testc, NEI_baltimore_testb, NEI_baltimore_testa)

# creat ggplot object with the base data and title
mplot <- ggplot(NEI_baltimore_foot, aes(year, Emissions)) + ggtitle("Total Emissions from PM2.5 in Baltimore City, MD by Type")

#filter_outliers
mplot <- mplot + geom_point()

  #add points and add facets
#mplot <- mplot + geom_point()+facet_wrap(~ type, ncol=2, scales = "free_y")
mplot <- mplot + facet_wrap(~ type, ncol=2, scales = "free_y")


#add regression line
mplot <- mplot + geom_smooth(method="lm")

# open model to plot to file
png(filename = "plot3.png")

#plot the graph
plot(mplot)

#close open device
dev.off()

