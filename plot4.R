## plot4.R, mdponce, 8 Jun 2014
## Coursera, Exploring Data assignment #1

## plot4.R reads in the household_power_consumption.txt file and creates
## 3 plots on various data fields for two dates:  1 Feb 2007
## and 2 Feb 2007. Descriptions in comments below.

## assumption:  household_power_consumption.txt and plot3.R exist in the 
##              working directory (directory name is not specified)
## assumption:  data.table package is installed

## data source: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold
##              _power_consumption.zip

## function design
## consideration: plot4.R is part of a set of scripts that recreate certain
##                graphs. Each script is supposed to be self-contained in 
##                that each will read in the required data. The data could
##                have been cached using a separate R function, but it was
##                felt this would not have been in keeping with the stated
##                requirements.
##                Also, time was short and didn't allow for working through
##                some issues with strptime. Considering that as.POSIXct()
##                will first call strptime when appropriate and considering
##                that I could get as.POSIXct() to work, I used POSIXct().
##                However, this is less efficient and strptime would be 
##                preferable, if time allowed.

## options: plot4() will simply create the histogram in the working directory
##          plot4("yes") will output status messages to the screen and 
##               create the histogram in the working directory
##          plot4(talkative = "yes") will output status messages to the screen 
##               and create the histogram in the working directory


plot4 <- function(talkative = "no"){

  ## load the required libraries
  library(data.table)
  library(datasets)
  
  ## read the data into a data table; coercion warnings are suppressed
  if(talkative == "yes"){print("Status: reading in the data")}
  suppressWarnings(myTable <- fread("household_power_consumption.txt", 
                                    sep=";", header=TRUE, na.strings="NA", 
                                    stringsAsFactors=FALSE, verbose=FALSE,))
  
  ## convert dates from original character format to desired date class format
  if(talkative == "yes"){print("Status: formatting the dates")}
  myTable$Date <- as.Date(myTable$Date, "%d/%m/%Y")
  
  ## take the subset of interest
  if(talkative == "yes"){print("Status: taking subset of interest")}
  myPlotData <- subset(myTable, Date == "2007-02-01" | 
                         Date == "2007-02-02")
  
  ## create new dateAndTime field
  if(talkative == "yes"){print("Status: creating new Date and Time field")}
  myPlotData$dateAndTime <- as.POSIXct(paste(myPlotData$Date, myPlotData$Time), 
                                 format="%Y-%m-%d %H:%M:%S")
  
  ## coerce the field of interest to class numeric
  if(talkative == "yes"){print("Status: coercing data to numeric")}
  class(myPlotData$Voltage) <- "numeric"
  class(myPlotData$Global_active_power) <- "numeric"
  class(myPlotData$Sub_metering_1) <- "numeric"
  class(myPlotData$Sub_metering_2) <- "numeric"
  class(myPlotData$Sub_metering_3) <- "numeric"
  class(myPlotData$Global_reactive_power) <- "numeric"
  
  ## open graphic device, create line plot in png file, close graphic device
  ## garbage variable collects the text output of dev.off()
  if(talkative == "yes"){print("Status: creating a png file of the plot")}
  png(file = "plot4.png", height=480, width=480)
  
  ## set the row x col graphic counts for combined display
  ## forces the following plots to be displayed in a single png file
  ## in a 2 x 2 matrix of graphics
  par(mfrow = c(2,2))
  
  ## create the Global Active Power plot
  with(myPlotData, {
    plot(myPlotData$dateAndTime, myPlotData$Global_active_power, 
         type = "l", xlab = "", 
         ylab = "Global Active Power")
  
  ## create the Voltage plot
  plot(x = myPlotData$dateAndTime, y = myPlotData$Voltage,
       type = "l", xlab = "datetime", 
       ylab = "Voltage")
  
  ## create the submetering plot
  plot(x = myPlotData$dateAndTime, y = myPlotData$Sub_metering_1,
       type = "l", xlab = "", 
       ylab = "Energy sub metering")
  lines(myPlotData$dateAndTime, myPlotData$Sub_metering_2, col = "red")
  lines(myPlotData$dateAndTime, myPlotData$Sub_metering_3, col = "blue")
  
  legend("topright", lty=1, col = c("black", "red", "blue"), legend = 
           c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
            bty="n")
  
  ## creat the Global Reactive Power plot
  plot(x = myPlotData$dateAndTime, y = myPlotData$Global_reactive_power,
       type = "l", xlab = "datetime", 
       ylab = "Global_reactive_power")
  })
  
  garbage <- dev.off()
  if(talkative == "yes"){print("Status: completed")}
}
