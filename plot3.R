plot3 <- function () {
  ## Read in the data
  mydata <- read.table("./household_power_consumption.txt",
                       sep = ";", header = T, na.strings = "?")
  
  ## Only looking at data from 2007-02-01 and 2007-02-02
  mydata$Date<-as.Date(mydata$Date, format = "%d/%m/%Y")
  
  ## Find first and last rows of 2007-02-01 and 2007-02-02
  ## entries
  i <- which(mydata$Date == "2007-02-01")[1]
  j<- which(mydata$Date == "2007-02-02")[length(
    which(mydata$Date == "2007-02-02"))]
  
  ## Cut data to subset we want
  mydata <- mydata[i:j,]
  
  ## Make time column a date/time class
  mydata$Time <- strptime(mydata$Time, 
                          format = "%T")
  mydata$Time<-format(mydata$Time, 
                      format="%H:%M:%S")
  
  ## Create the plot
  plotLabels <- c("Sub_metering_1", "Sub_metering_2",
                  "Sub_metering_3")
  x <- c(mydata$Sub_metering_1, mydata$Sub_metering_2,
        mydata$Sub_metering_3)
  g <- gl(3, 2880, labels = plotLabels)
  plot(mydata$Sub_metering_1, type = "n", xaxt = "n", 
       xlab = "", ylab = "Energy sub metering")
  axis(1,at = c(1, nrow(mydata)/2 , nrow(mydata)),
       labels = c("Thu", "Fri", "Sat"))
  lines(x[g == "Sub_metering_1"])
  lines(x[g == "Sub_metering_2"], col = "Red")
  lines(x[g == "Sub_metering_3"], col = "Blue")
  legend("topright", plotLabels, lty = c(1,1,1), 
         col = c("Black", "Red", "Blue"))
  
  ## Copy plot to png file
  dev.copy(png, file = "plot3.png")
  dev.off()
  
  return("Done")
}