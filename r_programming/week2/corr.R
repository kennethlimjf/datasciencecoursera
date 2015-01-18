source('getfilepath.R')
source('complete.R')
options(digits=4)

corr <- function(directory, threshold = 0) {
  CalculateCorr <- function(data.frame) {
    data.complete <- data.frame[complete.cases(data.frame), ]
    cor(data.complete$sulfate, data.complete$nitrate)
  }
  
  data.complete <- complete(directory)
  id <- data.complete[data.complete$nobs > threshold,]$id
  if(length(id) > 0) {
    file.paths <- do.call(GetFilePath, list(id, directory))
    data.list <- lapply(file.paths, read.csv)  
    result <- sapply(data.list, CalculateCorr)
  } else {
    result <- vector('numeric')
  }
  result
}