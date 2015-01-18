source('getfilepath.R')

complete <- function(directory, id = 1:332) {
  
  CountComplete <- function(data.frame) {
    nrow(data.frame[complete.cases(data.frame),])  
  }
  
  file.paths <- do.call(GetFilePath, list(id, directory))
  data.list <- lapply(file.paths, read.csv)
  
  data.id <- id
  data.nobs <- sapply(data.list, CountComplete)
  data.frame(list(id=data.id, nobs=data.nobs))
}