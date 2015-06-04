rankhospital <- function(state, outcome, rank = "best") {
  ## Read outcome data
  myData <- read.csv('./rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv', colClasses='character')
  ## Check that state and outcome are valid
  ## Check that state and outcome are valid
  if (outcome == 'heart attack') {
    colIndex <- 11
  }else if (outcome == 'heart failure') {
    colIndex <- 17
  }else if (outcome == 'pneumonia') {
    colIndex <- 23
  }else {
    stop('invalid outcome')
    geterrmessage()
  }
  
  if (!state %in% myData$State){
    stop('invalid state')
    geterrmessage()
  }
  ## Return hospital name in that state with the given rank
  ## Return hospital name in that state with lowest 30-day death
  myData <- myData[grep(state, myData$State),]
  myData[,colIndex] <- suppressWarnings(as.numeric(myData[,colIndex]))
  ## convert data into numeric
  cleanData <- na.omit(myData)
  
  if (rank == 'best'){
    
    orderData <- cleanData[order(cleanData[, colIndex], cleanData[, 'Hospital.Name']), ]
    print(orderData[1, 'Hospital.Name'])
  } else if (rank == 'worst'){
    
    orderData <- cleanData[order(decreasing = T ,cleanData[, colIndex], cleanData[, 'Hospital.Name']), ]
    print(orderData[1, 'Hospital.Name']) 
  } else if (rank <= length(cleanData) && rank >= 1){
    
    orderData <- cleanData[order(cleanData[, colIndex], cleanData[, 'Hospital.Name']), ]
    print(orderData[rank, 'Hospital.Name'])
  } else {
    print(NA)
  }
  ## 30-day death rate
}
