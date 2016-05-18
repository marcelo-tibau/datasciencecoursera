best <- function(state, outcomeName){
  # Read the oucome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- outcome$State
  diseases <- c("heart attack","heart failure","pneumonia")
  
  # Check the validity of state and outcome
  if (! state %in% states ){
    stop("invalid state")
  }
  if (! outcomeName %in% diseases){
    stop("invalid outcome")
  }
  # Return the hospital name in one particular state with the lowest 30-day death rate
  col_id <- 0
  if (outcomeName == diseases[1]){
    col_id <- 11
  } else if (outcomeName == diseases[2]) {
    col_id <- 17
  } else if (outcomeName == diseases[3]) {
    col_id <- 23
  }
  outcome.sub <- outcome[outcome[,7] == state, ]
  outcome.sub2 <- outcome.sub[,c(2,7,col_id)]
  outcome.sub3 <- outcome.sub2[outcome.sub2[,3]!="Not Available",]
  outcome.sub3[,3] <- as.numeric(outcome.sub3[,3])
  outcome2 <- outcome.sub3
  bestrow <- outcome2[outcome2[,3] == min(outcome2[,3]),]
  as.character(bestrow[1])
}