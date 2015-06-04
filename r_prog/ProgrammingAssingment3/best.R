best <- function(state, outcome) {
  ## Read outcome data, 
  myData <- read.csv('./rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv', colClasses='character')
  ## for heart attack, read data column 11
  ## for heart failure, read data column 17
  ## for pneumonia, read data column 23
  ## Check that state and outcome are valid
  if (outcome == 'heart attack') {
    colIndex <- 11
  }else if (outcome == 'heart failure') {
    colIndex <- 17
  }else if (outcome == 'pneumonia') {
    colIndex <- 23
  }else {
    print('Wrong Outcome column specified')
    stop()
  }
  
  if (!state %in% myData$State){
    stop('invalid state')
    geterrmessage()
  }
  ## Return hospital name in that state with lowest 30-day death
  myData <- myData[grep(state, myData$State),]
  myData[,colIndex] <- suppressWarnings(as.numeric(myData[,colIndex]))
  ## convert data into numeric
  orderData <- myData[order(myData[, colIndex]), ]
  print(length(orderData))
  print(orderData[1, 'Hospital.Name'])
  ## rate
}