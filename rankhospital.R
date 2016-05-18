rankhospital <- function(state, outcomeName, num = "best") {
  # Read the outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                   na.strings = "Not Available")
  outcome[,11] <- as.numeric(outcome[,11])
  outcome[,17] <- as.numeric(outcome[,17])
  outcome[,23] <- as.numeric(outcome[,23])
  
  # Check the validity of State and outcome
  if(!(state %in% outcome[,7])) {
    stop("invalid state")
  }
  if(!(outcomeName %in% c("heart attack", "heart failure", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  # Return the hospital name in one particular state with the given rank 30-day death rate
  hospitals <- split(outcome, outcome$State)[[state]]
  
  if(outcomeName == "heart attack") {
    hospitals <- hospitals[order(
      hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
      hospitals$Hospital.Name, na.last = NA), ]
    if(num == "best") {
      return(hospitals[1, 2])
    } else if(num == "worst") {
      return(hospitals[nrow(hospitals), 2])
    } else if(is.numeric(num)) {
      if(num > nrow(hospitals)) {
        return(NA)
      } else {
        return(hospitals[num, 2])
      }
    } else {
      stop("invalid num")
    }
  } else if(outcomeName == "heart failure") {
    hospitals <- hospitals[order(
      hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
      hospitals$Hospital.Name, na.last = NA), ]
    if(num == "best") {
      return(hospitals[1, 2])
    } else if(num == "worst") {
      return(hospitals[nrow(hospitals), 2])
    } else if(is.numeric(num)) {
      if(num > nrow(hospitals)) {
        return(NA)
      } else {
        return(hospitals[num, 2])
      }
    } else {
      stop("invalid num")
    }
  } else if(outcomeName == "pneumonia") {
    hospitals <- hospitals[order(
      hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
      hospitals$Hospital.Name, na.last = NA), ]
    if(num == "best") {
      return(hospitals[1, 2])
    } else if(num == "worst") {
      return(hospitals[nrow(hospitals), 2])
    } else if(is.numeric(num)) {
      if(num > nrow(hospitals)) {
        return(NA)
      } else {
        return(hospitals[num, 2])
      }
    } else {
      stop("invalid num")
    }
  }
}