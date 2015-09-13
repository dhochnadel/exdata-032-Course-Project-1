plot1 <- function() {
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
  
  ## Create histogram
  hist(mydata$Global_active_power, col = "Red", 
       main = "Global Active Power", 
       xlab = "Global Active Power (kilowatts)")
  
  ## Copy plot to png file
  dev.copy(png, file = "plot1.png")
  dev.off()
}