rankall <- function(outcome, num="best"){
    # Define function for parsing columns
    parseColumn <- function(val) {
        as.numeric(as.character(val))
    }
    rankhospital <- function(data, num){
        # Check num is valid
        num.list <- list("best"=1, "worst"=nrow(data))
        if(is.character(num) && !any(names(num.list) == num)) stop("invalid num")
        if(is.numeric(num) && num > num.list[["worst"]]) return(NA)
        
        # Set num
        if(is.character(num)) num <- num.list[[num]]
        
        # Create new column for ranking
        data$rank <- sapply(data[,out.col], parseColumn)
        
        sorted <- data[order(data$rank, data[,2]), ]
        hospital.name <- sorted[num, 2]
        return(as.character(hospital.name))
    }

    data <- read.csv("outcome-of-care-measures.csv")
    
    # Check outcome is valid
    outcomes <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
    if (!any(names(outcomes) == outcome)) stop("invalid outcome")
    
    # Set column for outcome
    out.col <- outcomes[[outcome]]
    
    result <- lapply(split(data, data$State), rankhospital, num=num)
    data.frame(list(hospital=unlist(result), state=names(result)))
}