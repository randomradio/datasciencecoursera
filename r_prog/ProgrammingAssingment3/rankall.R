rankall <- function(outcome, rank = "best") {
  ## Read outcome data
  myData <- read.csv('./rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv', colClasses='character')
  states <- levels(factor(myData[, 7]))
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
  
  output <- vector()
  myData[,colIndex] <- suppressWarnings(as.numeric(myData[,colIndex]))
  ## convert data into numeric
  cleanData <- na.omit(myData)
  ## For each state, find the hospital of the given rank
  for (i in 1:length(states)){
    stateData <- cleanData[grep(states[i], cleanData$State),]
    
    if (rank == 'best'){
      orderdata <- stateData[order(stateData[, colIndex], stateData[, 2]),]
      
      output <- append(output, orderdata[1, 'Hospital.Name'])
      output <- append (output, orderdata[1, 7])
    }else if (rank == 'worst'){
      orderdata <- stateData[order(decreasing = TRUE, stateData[, colIndex], stateData[, 2]),]
      
      output <- append(output, orderdata[1, 'Hospital.Name'])
      output <- append (output, orderdata[1, 7])
    }else if (rank <= length(cleanData) && rank >= 1){
      orderdata <- stateData[order(stateData[, colIndex], stateData[, 2]),]
      
      output <- append(output, orderdata[rank, 'Hospital.Name'])
      output <- append (output, orderdata[1, 7])
    }else{
      print(NA)
    }
  }
  output <- as.data.frame(matrix(output,length(states), 2, byrow = TRUE))
  colnames(output) <- c('hospital', 'state')
  rownames(output) <- states
  return(output)
  ## (abbreviated) state name
}
