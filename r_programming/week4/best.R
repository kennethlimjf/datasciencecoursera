best <- function(state, outcome) {
    # Define function for parsing columns
    parseColumn <- function(val) {
        as.numeric(as.character(val))
    }
  
    data <- read.csv('outcome-of-care-measures.csv')
    
    # Check state and outcome are valid
    outcomes = list('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
    if (!any(names(outcomes) == outcome)) stop("invalid outcome")
    if (!any(data$State == state)) stop("invalid state")
    
    subset.state <- data[data$State == state, ]
    
    out.col <- outcomes[[outcome]]
    subset.state$rank <- sapply(subset.state[,out.col], parseColumn)
    sorted <- subset.state[order(subset.state$rank, subset.state[,2]), ]
    hospital.name <- sorted[1, 2]
    
    return(as.character(hospital.name))
}
