best <- function(state, outcome){
  path = directory
  directory <- ("rprog-data-ProgAssignment3-data")
  fileName <- ("outcome-of-care-measures.csv")
  data <- read.csv(file.path(directory, fileName))
  State = data[, "State"]
  Hospital = data[, "Hospital.Name"]
  Heart_attack = data[, "11"]
  Heart_failure = data[, 17]
  Pneumonia = data[, 23]
  my_data(data.frame(Heart_attack, Heart_failure, Pneumonia, State, Hospital.Name))
  bool.state = my_data[, "State"] == state
  state.data = my_data[bool.state, ]
  state.data = na.omit(state.data)
  
  if(nrow(state.data) == 0){
    stop("Invalid state")
  }
  
  if(outcome == "Heart_attack"){
    minRate = min(state.data[,1])
    minHospital = state.data[which(state.data[,1] == minRate), Hospital.Name]
    
  }
}