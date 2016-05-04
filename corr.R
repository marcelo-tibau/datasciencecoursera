corr <- function(directory, threshold = 0){
  directory <- ("specdata")
  correl <- function(filesRead){
    data <- read.csv(file.path(directory, filesRead))
    nobs <- sum(complete.cases(data))
    if (nobs > threshold){
      return(cor(data$nitrate, data$sulfate, use = "complete.obs"))
    }
  }
  correls <- sapply(list.files(directory), correl) 
  correls <- unlist(correls[!sapply(correls, is.null)]) 
  return (correls)
}

