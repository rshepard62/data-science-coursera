rankall <- function(outcome, num){
    ## Read outcome data
    
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ailments <- c("heart attack", "heart failure", "pneumonia")
    
    
    ## Check that outcome is valid
    
    if (is.element(outcome, ailments) != TRUE) {stop("Invalid outcome")}
    
    
    ## Return hospital name in that state with the given rank 30-day death rate
    state.table <- table(outcome_data[,7])
    state.vect <- dimnames(state.table)
    state_hospitals <- NULL
    
    for(state in state.vect){
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
        
        ranking <- order(death_rates)
        hospitals.sorted <- hospitals[ranking]
        
        if(num == "best") {num <- 1}
        else if(num == "worst") {num <- length(hospitals)}
        
        if(is.element(num, ranking)){
            in_state <- hospitals.sorted[[num]]
        }
        else {print("NA")}
        
        state_hospitals <- rbind(state_hospitals, in_state)
    }
    
    data.frame(best_hospitals, state.vect)
}
