rankhospital <- function(state, outcome, num = "best") {
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
    subset.state.outcome <- subset.state[complete.cases(subset.state$rank), ]
    
    # Check num is valid
    num.list = list("best"=1, "worst"=nrow(subset.state.outcome))
    if(is.character(num) && !any(names(num.list) == num)) stop("invalid num")
    if(is.numeric(num) && num > num.list[["worst"]]) return(NA)
    
    # Set num
    if(is.character(num)) num <- num.list[[num]]
    
    sorted <- subset.state.outcome[order(subset.state.outcome$rank, subset.state.outcome[,2]), ]
    hospital.name <- sorted[num, 2]

    return(as.character(hospital.name))
}
