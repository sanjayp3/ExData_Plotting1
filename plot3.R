
# This function generates the plot Sub_metering_1~dateTime and add legend at top right
plot3 <- function(){
  
  houseHoldData <- electricPowerConsumption()
  
  with(houseHoldData, {
    plot(Sub_metering_1~dateTime, type="l",
         ylab="Energy sub metering", xlab="")
    lines(Sub_metering_2~dateTime,col='Red')
    lines(Sub_metering_3~dateTime,col='Blue')
  })
  legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
}

# This function reads the household_power_consumption.txt , date column is converted to Date object, removed the missing data etc
electricPowerConsumption <- function(){
  
  
  houseHoldData <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
  
  ## Format date to Type Date
  houseHoldData$Date <- as.Date(houseHoldData$Date, "%d/%m/%Y")
  
  ## Filter data set from Feb. 1, 2007 to Feb. 2, 2007
  houseHoldData <- subset(houseHoldData,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
  print(dim(houseHoldData))
  
  ## Remove incomplete observation
  houseHoldData <- houseHoldData[complete.cases(houseHoldData),]
  
  ## Combine Date and Time column
  dateTime <- paste(houseHoldData$Date, houseHoldData$Time)
  
  ## Name the vector
  dateTime <- setNames(dateTime, "DateTime")
  
  ## Remove Date and Time column
  houseHoldData <- houseHoldData[ ,!(names(houseHoldData) %in% c("Date","Time"))]
  
  ## Add DateTime column
  houseHoldData <- cbind(dateTime, houseHoldData)
  
  ## Format dateTime Column
  houseHoldData$dateTime <- as.POSIXct(dateTime)
  
  #View(houseHoldData)
  
  return(houseHoldData)
  
}



