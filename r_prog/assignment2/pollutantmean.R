pollutantmean <- function(directory, pollutant, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  myDir <- dir(directory)
  if (length(id) > length(myDir)) {
    print("too many monitors")
    return
  } 
  else {
    useDir <- myDir[id]
    sumArray <- c()
    countArray <- c()
    for (d in useDir){
      ## relative path to csv file under current directory
      csvPath <- file.path(directory, d)
      csvData <- read.csv(csvPath)
      ## get pollutant column
      pData <- csvData[[pollutant]]
      ## get logic filter on the NAs
      badOnes <- is.na(pData)
      ## extract good from pollutant column
      goodOnes <- pData[!badOnes]
      ## avoid round, keep sum of each input
      curSum <- sum(goodOnes)
      curCount <- length(goodOnes)
      sumArray <- c(sumArray, curSum)
      countArray <- c(countArray, curCount)
    }
    result <- sum(sumArray)/sum(countArray)
    print(result) 
  }
}