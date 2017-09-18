rankall <- function(outcome, num = "best") {
    ## Read outcome data
    dataold <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
    outcomes = c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    
    ## Check that outcome is valid
    if(!outcome %in% names(outcomes)){
        stop("invalid outcome")
    }
    
    if(num == "best"){
        num = 1
    }
    
    data = dataold[,c(2, 7, outcomes[outcome])]
    names(data) = c("hospitals","states", outcome)
    data = data[order(data$hospitals),]
    data = data[order(data[outcome]),]
    data = data[order(data$states),]
    split.data = split(data, data$states)
    #results = sapply(split.data, function(x) x[num,1])
    results = sapply(split.data, function(x){
        if(num == "worst"){
            num = nrow(x[outcome])-sum(is.na(x[outcome]))
        }
        x[num,1]
    })
    
    
    data.frame(hospital=results, state=names(results), row.names=names(results))
}
