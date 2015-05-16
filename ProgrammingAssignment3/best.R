## The following function reads the outcome-of-care-measures.csv file and
## returns a character vector with the name of the hospital that has the
## best (i.e. lowest) 30-day mortality for the specified outcome
## in that state.
##
## Sample run-time examples
## -----------------------------------------------------------------------
## > source("best.R")
## > best("TX", "heart attack")
## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
## > best("TX", "heart failure")
## [1] "FORT DUNCAN MEDICAL CENTER"
## > best("MD", "heart attack")
## [1] "JOHNS HOPKINS HOSPITAL, THE"
## > best("MD", "pneumonia")
## [1] "GREATER BALTIMORE MEDICAL CENTER"
## > best("BB", "heart attack")
## Error in best("BB", "heart attack") : invalid in state
## > best("NY", "hert attack")
## Error in best("NY", "hert attack") : invalid outcome

best <- function(state, outcome) {
        # map valid outcomes to data columns
        oList <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)

        # check for valid outcome
        if (!(outcome %in% names(oList)))
                stop("invalid outcome")

        # Read outcome data
        outcomeData <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")

        # check for valid state
        if (!(state %in% outcomeData$State))
                stop("invalid in state")

        # Set outcome data for state
        sData <- subset(outcomeData, outcomeData$State == state)

        # Get hospitals by name in state
        hsData <- sData$Hospital.Name

        # Get column number for outcome data using the 
        # oList mapping list
        oCol <- unlist(oList[outcome])

        # Get mortality rate data for outcome
        mortalityRate <- sData[, oCol]

        # Bind hospital data with outcome data
        oData <- cbind(hsData, mortalityRate)

        # Eliminate rows with no available data
        hData <- subset(oData, oData[, 2] != "Not Available")

        # Create matrix of hospitals with minimum mortality rate in state
        lowestMR <- subset(hData, as.numeric(hData[, 2]) == min(as.numeric(hData[, 2])))

        # Sort outcome data in ascending order where
        # multiple results are returned or single row is returned
        bData <- sort(lowestMR[, 1], decreasing = FALSE)

        # Return 1st row
        retVal <- bData[[1]]

        return (retVal)
}