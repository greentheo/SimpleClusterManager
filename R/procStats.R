#' Get CPU Stats 
#' @param time the amount of time in seconds to take the measurement
#' @author Theodore Van Rooy (\url{http://royaltyanalytics.com/})
#' @export 
#' @import lattice
#' @import httpRequest

getProcStats = function(time=1){
  procsC = scan('/proc/stat', what=character())
  numprocs = length(procsC[grep("cpu.", procsC)])
  
  procsRaw = (read.table('/proc/stat', nrow=numprocs+1, row.names=1,colClasses=c("character", rep("numeric", 8))))
  procs = matrix(unlist(lapply(procsRaw, as.numeric)), nrow=nrow(procsRaw))
  memory = read.table('/proc/meminfo', nrows=7, row.names=1)
  Sys.sleep(time)
  procsRaw2 = (read.table('/proc/stat', row.names=1, nrow=numprocs+1,colClasses=c("character", rep("numeric", 8))))
  procs2 = matrix(unlist(lapply(procsRaw2, as.numeric)), , nrow=nrow(procsRaw))
  
  memory2 = read.table('/proc/meminfo', nrows=7, row.names=1,colClasses=c("character", "numeric", "character"))
  
  colnames(procs) = c("user", "nice", "system","idle", "iowait", "irq", "null1", "null2", "null3", "null4")
  colnames(procs2) = colnames(procs)
  rownames(procs) = rownames(procsRaw)
  rownames(procs2) = rownames(procsRaw)
  
  memoryDiff = memory2[,1]-memory[,1]
  names(memoryDiff) = rownames(memory)
  return(list(numprocs=numprocs,
              sumProc = as.numeric(procs2[1,])-as.numeric(procs[1,]),
              indProcs = procs2-procs,
              memoryDiff=memoryDiff,
              memory = memory2
              ))
}
#' Get Summary CPU data 
#' @param timeSum the amount of time in seconds to generate the plot over
#' @param timeInterval the amount of resolution in the plots
#' @param cont logical, whether to start a fresh count or pull existing data
#' @author Theodore Van Rooy (\url{http://royaltyanalytics.com/})
#' @export 
procSummaryData = function(timeSum, timeInterval, cont=F, decayParam=.5){
  
  if(file.exists('procSummaryUser.csv') & cont){
    procDat = read.csv('procSummaryUser.csv')
  }else{
    sumStats = getProcStats(timeInterval)
    procDat = matrix(0, nrow=sumStats$numprocs+1, 60)
    write.csv(file='procSummaryUser.csv', procDat,row.names=F)
  }
  time = Sys.time()
  while(Sys.time()-time < timeSum){
    print(Sys.time()-time)
    sumStats = getProcStats(timeInterval)
    procDat[, 1:(ncol(procDat)-1)] = procDat[,2:ncol(procDat)]
    nextLine=sumStats$indProcs[,"user"]+procDat[,(ncol(procDat)-1)]-decayParam
    procDat[,ncol(procDat)] = ifelse(nextLine<0, 0, nextLine)
      
  }
  write.csv(file='procSummaryUser.csv', procDat, row.names=F)
  
 
}
#' Make Summary CPU plots 

#' @author Theodore Van Rooy (\url{http://royaltyanalytics.com/})
#' @export 
procSummaryPlot = function(){
  if(file.exists('procSummaryUser.csv')){
    procDat = read.csv('procSummaryUser.csv')
  }
  plot(y=procDat[1, ], x=-60:-1, type="l", lwd=2, main="CPU User Time Delta", 
       ylab="User Time Delta",
       xlab="Time")
  for(i in 2:nrow(procDat)){
    lines(y=procDat[i,], x=-60:-1, col=rainbow(nrow(procDat))[i])
  }
  
}
