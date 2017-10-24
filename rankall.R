rankall <- function(outcome, num = "best") {
     # Read outcome data
     measures <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
                          stringsAsFactors = FALSE)
     
     # Defining possible outcomes and associated column numbers
     outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
     
     # Check that outcome is valid
     if (!(outcome %in% names(outcomes))) {stop("invalid outcome")}
     
     # Extracting relevant data to df rel_data and tidying it up
     rel_data <- measures[,c(2,7,outcomes[outcome])]
     names(rel_data) <- c("Hospital", "State", "Rate")
     rel_data <- na.omit(rel_data)
     rel_ordered <- rel_data[order(rel_data$State, rel_data$Rate, rel_data$Hospital), ]
     
     # NEED to do an apply / split function here to select the correct hospital rating
     
     # Return a data frame with the hospital names and the
     # (abbreviated) state name
     return(selected_hosp)
     
     
}