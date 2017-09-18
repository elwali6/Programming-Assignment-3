best <- function(state, outcome){
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
    data = data[order(data[outcome]),]
    return(data[1,1])
}