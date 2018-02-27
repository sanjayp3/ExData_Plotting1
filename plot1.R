# This function generates the plot Global Active Power (kilowatts)
# The plot1() function needs to be executed which will generate the plot1.png file of width 480px & height 480 px
plot1 <- function(){
  
  houseHoldData <- electricPowerConsumption()
  
  # Create the plot
  png("plot1.png", width=480, height=480, units="px")
  
  ## Create the histogram
  hist(houseHoldData$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")
  
  dev.off()
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

