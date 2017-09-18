rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    dataold <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
    outcomes = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    
    ## Check that state and outcome are valid
    if(!state %in% dataold$State){
        stop("invalid state")
    }
    else if(!outcome %in% names(outcomes)){
        stop("invalid outcome")
    }
    
    dataold = dataold[dataold$State == state,]
    data = dataold[,c(2, outcomes[outcome])]
    names(data) = c("hospitals", outcome)
    data = data[order(data$hospitals),]
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    data.outcome = data[outcome]
    if(num == "best"){
        num = 1
    }
    else if(num == "worst"){
        num = nrow(data.outcome)-sum(is.na(data.outcome))
    }
    data = data[order(data.outcome),]
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    return(data[num,1])
}