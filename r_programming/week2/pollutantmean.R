source('getfilepath.R')

pollutantmean <- function(directory, pollutant, id = 1:332) {
  file.paths <- do.call(GetFilePath, list(id, directory))
  data.list <- lapply(file.paths, read.csv)
  
  data.frame <- do.call(rbind, data.list)
  round(mean(data.frame[[pollutant]], na.rm= T), 3)
}