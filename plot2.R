## plot2.R, mdponce, 8 Jun 2014
## Coursera, Exploring Data assignment #1

## plot2.R reads in the household_power_consumption.txt file and creates
## a line plot on the Global Active Power data for two dates:  1 Feb 2007
## and 2 Feb 2007

## assumption:  household_power_consumption.txt and plot2.R exist in the 
##              working directory (directory name is not specified)
## assumption:  data.table package is installed

## data source: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold
##              _power_consumption.zip

## function design
## consideration: plot2.R is part of a set of scripts that recreate certain
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

## options: plot2() will simply create the histogram in the working directory
##          plot2("yes") will output status messages to the screen and 
##               create the histogram in the working directory
##          plot2(talkative = "yes") will output status messages to the screen 
##               and create the histogram in the working directory


plot2 <- function(talkative = "no"){

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
  class(myPlotData$Global_active_power) <- "numeric"
  
  
  ## open graphic device, create line plot in png file, close graphic device
  ## garbage variable collects the text output of dev.off()
  if(talkative == "yes"){print("Status: creating a png file of the histogram")}
  png(file = "plot2.png", height=480, width=480)
  with(myPlotData, plot(myPlotData$dateAndTime, myPlotData$Global_active_power, 
                        type = "l", xlab = "", 
                        ylab = "Global Active Power (kilowatts)"))
  
  garbage <- dev.off()
  if(talkative == "yes"){print("Status: completed")}
}
