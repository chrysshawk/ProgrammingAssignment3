best <- function(state, outcome) {
     # Read outcome data
     measures <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
          stringsAsFactors = FALSE)

     outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
     
     # Check that state and outcome are valid, quit if false
     if (!(state %in% measures$State)) {stop("invalid state")}
     if (!(outcome %in% names(outcomes))) {stop("invalid outcome")}
     
     # Extracting relevant data to df rel_data and tidying it up
     rel_data <- measures[,c(2,7,outcomes[outcome])]
     names(rel_data) <- c("Hospital", "State", "Rate")
     rel_data <- na.omit(rel_data)
     rel_ordered <- rel_data[order(rel_data$State, rel_data$Rate, rel_data$Hospital), ]
     
     # Returning the best ranked (lowest mortality rate) hospitals by state
     best_hosp <- lapply(split(rel_ordered, rel_ordered$State), function(elt) elt[1, ])
     
     # returning the best hospital in given state
     best_hosp <- best_hosp[state]
     
     # formatting output
     best_output <- unlist(best_hosp[state])
     best_output <- as.character(best_output[1])
     return(best_output)
     
}