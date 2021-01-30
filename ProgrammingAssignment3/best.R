best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
    
    ##----------------------------------------------------------------------------------
    
    ## Check that state and outcome are valid
    states <- unique(outcome_data$State)
    if(state %in% states == FALSE){
        stop("invalid state")
    }
    
    # easiest way to get the outcomes is checking the data
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia") 
    if(outcome %in% valid_outcomes == FALSE){
        stop("invalid outcome")
    } else if(outcome == "heart attack"){
        outcome_rate <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    } else if(outcome == "heart failure"){
        outcome_rate <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    } else{
        outcome_rate <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    } 
    ##----------------------------------------------------------------------------------
    
    ## Return hospital name in that state with lowest 30-day death rate
    best_death_rate <- max(as.numeric(outcome_data[[outcome_rate]]), na.rm = TRUE)
    current_rate <- best_death_rate
    best_hospital <- "ZZZZZZZZZZZZZZZZZZZZZZ"
    
    for(row in 1:nrow(outcome_data)){
        if(outcome_data[row,]$State == state){
            current_rate <- as.numeric(outcome_data[row,][[outcome_rate]])
            if(is.na(current_rate)){
                next
            }
            if(current_rate < best_death_rate){
                best_death_rate <- current_rate
                best_hospital <- outcome_data[row,]$Hospital.Name
            }
            else if(current_rate == best_death_rate){
                if(outcome_data[row,]$Hospital.Name < best_hospital){
                    best_hospital <- outcome_data[row,]$Hospital.Name
                }
            }
        }
    }
    best_hospital
}