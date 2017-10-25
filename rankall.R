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

     # Selecting the relevant hospital in each given state
     if (num == "best") {
       num <- 1
       # ordering by state, best to worst
       rel_ordered <- rel_data[order(rel_data$State, rel_data$Rate, rel_data$Hospital), ]
     } else if (num == "worst") {
       # Changing ordering criteria (worst appears first in list)
       rel_ordered <- rel_data[order(rel_data$State, (-rel_data$Rate), rel_data$Hospital),]
       num <- 1 # this now is the worst, not the best
       message("this is the worst")
     } else {
       num <- num # no transformation
       # ordering by state, best to worst
       rel_ordered <- rel_data[order(rel_data$State, rel_data$Rate, rel_data$Hospital), ]
     }
     
     # Splitting the relevant data by state
     byState <- split(rel_ordered, rel_ordered$State)
     
     # Selecting the hospital based on input ranking variable 'num'
     selHosp <- sapply(byState, function(elt) elt[num, 1:2])

     # Transposing result (colums "Hospital" and "State") into data frame
     outHosp <- as.data.frame(t(selHosp))
     
     # Return a data frame with the hospital names and the
     return(outHosp)
     
}