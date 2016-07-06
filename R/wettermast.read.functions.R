#' Read a Wettermast Hamburg data file
#' @param file path to a Wettermast Hamburg data file
#' @param na.value the value of missing values in the file. Will be set to NA.
#' @return a data frame containing the variable values and the time as POSIXct
#' @details The file has to be named like this:
#' \verb{VARNAME_YYYYMMDDHHSS-YYYYMMDDHHSS.csv}
#' This is crucial. This is the starndard naming convention for Wettermas Hamburg files.
#' The first date is the start time, the second date the end time for the series.
#' An equally spaced timestamp series is created according to the number of variable values in the file.
#' @export
read.wettermast.file = function(file,na.value = NA) {
	base <- basename(file)
	suffixsplit <- strsplit(base,"\\.")[[1]]
	name <- suffixsplit[1]
	suffix <- suffixsplit[2]
	namesplit <- strsplit(name,"_")[[1]]
	varname <- namesplit[1]
	
	daterange <- namesplit[2]
	datestart <- strsplit(daterange,"-")[[1]][1]
	dateend <- strsplit(daterange,"-")[[1]][2]
	
	# Don't get confused by the timezone!
	# The weather mast always gives dates in MEZ / CET / Central European Time (which is UTC+1)
	# For some reason, in R the definition of UTC+1 is UTC-1, .... YES, live with it :-)
	# So, time zone is UTC-1...
	timestart <- as.POSIXct(datestart, format = "%Y%m%d%H%M", tz = "UTC-1")
	timeend   <- as.POSIXct(dateend, format = "%Y%m%d%H%M", tz = "UTC-1")
	
	
	# read file
	data <- as.numeric(readLines(file))
	
	# Removs NA values
	data[data == na.value] <- NA
	
	# Add time
	time <- seq(timestart,timeend,len=length((data)))
	
	# Return dataframe
	frame <- data.frame(time,data)
	colnames(frame)[2] <- varname
	
	
	frame
}