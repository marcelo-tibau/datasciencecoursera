pollutantmean <- function(directory, pollutant, id = 001:332){
  path = directory
  directory <- ("specdata")
  fileList = list.files(path)
  file.names = as.numeric(sub("\\.csv$","",fileList))
  selected.files = fileList[match(id,file.names)]
  Data = lapply(file.path(path, selected.files), read.csv)
  Data = do.call(rbind.data.frame,Data)
  mean(Data[,pollutant],na.rm=T)
  
}

