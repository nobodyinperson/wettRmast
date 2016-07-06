#' Obtain a value at a specific time from wettermast hamburg - object
#' @export
get.value.from.wettermast = function(x,time,...) {
	time <- as.POSIXct(time,tz="UTC-1",...)
	
	x[,2][which.min(abs(as.numeric(x$time)-as.numeric(time)))]
}