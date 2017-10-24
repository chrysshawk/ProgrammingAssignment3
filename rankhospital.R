rankhospital <- function(state, outcome, num = "best") {
     # Read outcome data
     measures <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
                          stringsAsFactors = FALSE)
     
     # Defining possible outcomes and associated column numbers
     outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
     
     # Check that state and outcome are valid
     if (!(state %in% measures$State)) {stop("invalid state")}
     if (!(outcome %in% names(outcomes))) {stop("invalid outcome")}
     
     # Extracting relevant data to df rel_data and tidying it up
     rel_data <- measures[,c(2,7,outcomes[outcome])]
     names(rel_data) <- c("Hospital", "State", "Rate")
     rel_data <- na.omit(rel_data)
     rel_ordered <- rel_data[order(rel_data$State, rel_data$Rate, rel_data$Hospital), ]
     
     # Returning the ranked hospitals in given state
     best_hosp <- rel_ordered[rel_ordered$State==state, ]
     
     # Setting ranking variable to numeric
     if (num == "best") {
          ranking <- 1
     } else if (is.numeric(num)) {
          ranking <- num
     } else {
          ranking <- nrow(best_hosp) # placeholder for the nrows(result table) function
     }
     
     # Selecting the defined hospital ranking
     selected_hosp <- best_hosp$Hospital[ranking]
     
     # Return hospital name in that state with the given rank 30-day death rate
     return(selected_hosp)
     
}