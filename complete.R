complete <- function(directory, id = 1:332) {
  directory <- ("specdata")
  filesFull <- function(filesRead) sum(complete.cases(read.csv(filesRead)))
  filesRead <- list.files(directory, full.names=TRUE)[id]
  data.frame(id = id, nobs = unlist(lapply(filesRead, filesFull)))
}