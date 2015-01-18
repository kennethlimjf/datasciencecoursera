GetFilePath <- function(id, directory="specdata") {
  paste(directory, "/", formatC(id, width=3, flag="0"), ".csv", sep="")
}