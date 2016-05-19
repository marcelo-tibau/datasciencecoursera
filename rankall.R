rankall <- function(outcomeName, num = "best") {
  # Read the outcome data 
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[,c(2,7,11,17,23)]
  
  # Check the validity of State and outcome  
  if(! (outcomeName == "heart attack" || outcomeName == "heart failure" || outcomeName == "pneumonia") ) {
    stop("invalid outcome")
  }
  
  if(class(num) == "character"){
    if (! (num == "best" || num == "worst")){
      stop("invalid number")
    }
  }
  
  # Remove columns by outcome and left the hospital name and deaths of any particular outcome
  if(outcomeName == "heart attack") {
    outcome = outcome[,c(1,2,3)]
  } else if(outcomeName == "heart failure") {
    outcome = outcome[,c(1,2,4)]
  } else if(outcomeName == "pneumonia") {
    outcome = outcome[,c(1,2,5)]
  }
  names(outcome)[3] = "Deaths"
  outcome[, 3] = suppressWarnings( as.numeric(outcome[, 3]) )
  
  # Remove rows with NA
  outcome = outcome[!is.na(outcome$Deaths),]
  
  splited = split(outcome, outcome$State)
  ans = lapply(splited, function(x, num) {
    
    # Order by deaths and hospital name
    x = x[order(x$Deaths, x$Hospital.Name),]
    
    # Return the result in terms of best or worst
    if(class(num) == "character") {
      if(num == "best") {
        return (x$Hospital.Name[1])
      }
      else if(num == "worst") {
        return (x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return (x$Hospital.Name[num])
    }
  }, num)
  
  #Return the data frame with format defined by hospital name and state abbreviation
  return ( data.frame(hospital=unlist(ans), state=names(ans)) )
  
  
}