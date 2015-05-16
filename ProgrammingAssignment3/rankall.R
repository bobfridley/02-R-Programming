## The following function reads the outcome-of-care-measures.csv file and
## returns a data frame with the name of each hospital in every state ranked
## for the specified outcome.
## States without a ranking will have NA as the resulting rank
## -----------------------------------------------------------------------

# map data columns to valid outcomes
outcomeMap <- data.frame(
        Outcome = c("heart attack", "heart failure", "pneumonia"),
        Column = c(11, 17, 23)
)

getRankByOutcome <- function(data, col_num, num) {
        # Array of outcomes
        # Suppress warnings produced by coercing NA as.numeric
        aOutcomes <- suppressWarnings(as.numeric(data[, col_num]))

        # Length of non-NA outcomes
        len <- dim(data[!is.na(aOutcomes), ])[1]

        # Get or Set the rank of outcomes by hospital
        if (num == "best") {
                result <- getRank(data, aOutcomes, 1)
        } else if (num == "worst") {
                result <- getRank(data, aOutcomes, len)
        } else if (num > len) {
                result <- NA
        } else {
                result <- getRank(data, aOutcomes, num)
        }

        return(result)
}

getRank <- function(data, outcomes, num) {
        result <- data[, 2][order(outcomes, data[, 2])[num]]

        return(result)
}

rankall <- function(outcome, num = "best") {
        # Read outcome data
        data <- read.csv("./outcome-of-care-measures.csv",
                colClasses = "character")
        # Array of unique states sorted ascending
        aStates <- sort(unique(data$State))
        # Length of aStates
        aLength <- length(aStates)
        # Empty array of hospitals with length of aStates
        # Will be populated as rankings are set
        aHospitals <- rep("", aLength)

        # Check for valid outcome
        if (!(outcome %in% outcomeMap[, 1])) {
                stop("invalid outcome")
        } else {
                # Get column number for outcome data using the dfMap data frame
                nCol <- outcomeMap$Column[outcomeMap$Outcome == outcome]

                # Loop through each state returning ranked hospital
                for(i in 1:aLength) {
                        # Data frame of hospitals by state
                        dfState <- data[data$State == aStates[i], ]
                        # Hospital matching state, outcome and num
                        aHospitals[i] <- getRankByOutcome(dfState, nCol, num) 
                }
        }

        # create the data frame to return
        result <- data.frame(hospitals = aHospitals, state = aStates)

        return(result)
}