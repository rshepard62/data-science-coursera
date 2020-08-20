best <- function(state, outcome){
    ## Read outcome data
    
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ailments <- c("heart attack", "heart failure", "pneumonia")
    
    
    ## Check that state and outcome are valid
    
    if (is.element(state, outcome_data[,7]) != TRUE) {stop("Invalid state")}
    if (is.element(outcome, ailments) != TRUE) {stop("Invalid outcome")}
    
    
    ## Return hospital name in that state with lowest 30-day death rate
    
    match.state <- outcome_data[,7] == state
    hospitals <- outcome_data[match.state,2]
    
    warn_op <- getOption("warn")
    options(warn = -1)
    if (outcome == ailments[1]) {
        death_rates <- as.numeric(outcome_data[match.state,11])
    }
    
    else if (outcome == ailments[2]) {
        death_rates <- as.numeric(outcome_data[match.state,17])
    }
    
    else {
        death_rates <- as.numeric(outcome_data[match.state,23])
    }
    options(warn = warn_op)
    
    print(hospitals[which.min(death_rates)])
    
}