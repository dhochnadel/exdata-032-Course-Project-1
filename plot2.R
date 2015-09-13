plot2 <- function() {
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
  
  ## Plot the data for plot2
  plot(mydata$Global_active_power, type = "l", xaxt = "n", 
       xlab = "", ylab = "Global Active Power (kilowatts)")
  axis(1,at = c(1, nrow(mydata)/2 , nrow(mydata)),
       labels = c("Thu", "Fri", "Sat"))
  
  ## Copy plot to png file
  dev.copy(png, file = "plot2.png")
  dev.off()
  
  return("Done")
}