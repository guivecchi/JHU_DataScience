rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
  
    ##----------------------------------------------------------------------------------
    
    ## Check that state and outcome are valid
    states <- unique(outcome_data$State)
    
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
  
    ## For each state, find the hospital of the given rank
    states_df <- data.frame(row.names = c("hospital", "state"))
    
    for(current_state in states){
        state_data <- subset(outcome_data[c("Hospital.Name","State",outcome_rate)], outcome_data["State"] == current_state)
        state_data <- subset(state_data, state_data[outcome_rate] != "Not Available")
        state_data <- state_data[order(state_data["Hospital.Name"]),]
        state_data <- state_data[order(as.numeric(state_data[[outcome_rate]])),]
        
        if(num == "best"){
          chosen_hospital <- as.character(state_data[1,]["Hospital.Name"])
        }
        else if(num == "worst"){
          chosen_hospital <- as.character(state_data[nrow(state_data),]["Hospital.Name"])
        }
        else if(as.numeric(num) > nrow(state_data)){
          chosen_hospital <- NA
        }
        else{
          chosen_hospital <- as.character(state_data[as.numeric(num),]["Hospital.Name"])
        }
        
        state_data_frame <- data.frame(chosen_hospital, current_state)
        names(state_data_frame) <- c("hospital", "state")
        states_df <- rbind(states_df, state_data_frame)
    }
    
    ## Return a data frame with the hospital names and the (abbreviated) state name
    states_df <- states_df[order(states_df["state"]),]
    states_df
}