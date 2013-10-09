#' Get CPU Stats 
#' @param time the amount of time in milliseconds to take the measurement
#' @author Theodore Van Rooy (\url{http://royaltyanalytics.com/})
#' @export 
#' @import lattice
#' @import httpRequest

getProcStats = function(time=1000){
  procs = scan('/proc/stats', what=character())
  
}