getwd()
data = read.table("household_power_consumption 2.txt", header= TRUE, sep= ";" )
head(data)

str(data)
dim(data)

data$Date <- as.Date(data$Date, "%d/%m/%y")

install.packages("dplyr")
library(dplyr)

# Data has been newly modified to the year of 2020, not 2007. 
data = subset(data, Date >= as.Date("2020-2-1") & Date <= as.Date("2020-2-2"))
data = data[complete.cases(data), ]
dateTime = paste(data$Date, data$Time)
dateTime = setNames(dateTime, "DateTime")
data = data[ , !(names(data)) %in% c("Date", "Time")]
data = cbind(dateTime, data)
names(data)[names(data) == 'dateTime'] <- "DateTime"
head(data)
data$dateTime = as.POSIXct(dateTime)
data$DateTime = as.POSIXct(dateTime)
head(data)

### Making Plots
# Plot1 

c1<-data$Global_active_power
d1<-as.numeric(as.character(c))
png("plot1.png",width=480,height=480)
hist(d1,main="Global Active Power",xlab="Global Active Power (kilowatt)", ylab="Frequency",col="red")
dev.off()

# Plot 2

png("plot2.png", width=480, height=480)
datetime <- strptime(data$DateTime, sep=" ", "%d/%m/%Y %H:%M:%S")
plot(DateTime, data, type="l", xlab="", ylab="Global Active Power (kilowatts)")
dev.off()

# Plot 3 
png("plot3.png", width=480, height=480)
datetime <- strptime(data$DateTime, sep=" ", "%d/%m/%Y %H:%M:%S")
plot(datetime, data$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering",col="green",ylim=c(0,40))
par(new=TRUE)
plot(datetime, data$Sub_metering_2, type="l", xlab="", ylab="",col="red",ylim=c(0,40))
par(new=TRUE)
plot(datetime, data$Sub_metering_3, type="l", xlab="", ylab="",col="blue",ylim=c(0,40))
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1,col=c("green","red","blue"))
dev.off()

# Plot 4

library(dplyr)

png("plot4.png", width=480, height=480)
par(mfrow=c(2,2))
plot(DateTime, d, type="l", xlab="", ylab="Global Active Power (kilowatts)")
p<-as.numeric(as.character(data$Voltage))
plot(DateTime, p, type="l", xlab="datetime", ylab="Voltage")

plot(datetime, data$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering",col="green",ylim=c(0,40))
par(new=TRUE)
plot(datetime, data$Sub_metering_2, type="l", xlab="", ylab="",col="red",ylim=c(0,40))
par(new=TRUE)
plot(datetime, data$Sub_metering_3, type="l", xlab="", ylab="",col="blue",ylim=c(0,40))
legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1,col=c("green","red","blue"),cex=0.5)

q<-as.numeric(as.character(data$Global_reactive_power))
plot(datetime, q, type="l", xlab="datetime", ylab="Global_reactive_power")
dev.off()

