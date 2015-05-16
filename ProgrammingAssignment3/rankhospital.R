## The following function reads the outcome-of-care-measures.csv file and
## returns a character vector with the name of the hospital that has the
## best, worst or specific 30-day mortality ranking for the specified outcome
## in that state.
##
## if a specific ranking (as integer) is passed and is greater than
## the number of rows returned for that state, 'NA' is returned
## -----------------------------------------------------------------------

setwd("/Users/bobfridley/Documents/Coursera/R-wd")

rankhospital <- function(state, outcome, num = "best") {
        if (num == "best") {
                return (best(state, outcome))
        }

        # map valid outcomes to data columns
        dfMap <- data.frame(
                Outcome = c("heart attack", "heart failure", "pneumonia"),
                ColNumber = c(11, 17, 23)
        )

        # check for valid outcome
        if (!(outcome %in% dfMap[, 1]))
                stop("invalid outcome")

        # Get column number for outcome data using the dfMap data frame
        colNumber <- dfMap$ColNumber[dfMap$Outcome == outcome]
        colName <- dfMap$ColName[dfMap$Outcome == outcome]

        # Read outcome data
        outcomeData <- read.csv("./outcome-of-care-measures.csv",
                colClasses = "character")

        # check for valid 'state' argument
        if (!(tolower(state) %in% tolower(outcomeData$State)))
                stop("invalid state")

        # rename 'outcome' argument
        names(outcomeData)[colNumber] <- "Rate"

        # extract only needed columns from outcomeData
        oData <- subset(outcomeData, outcomeData$State == state
                & outcomeData$Rate != "Not Available",
                select = c("Hospital.Name", "Rate")
        )

        # sort data frame by Rate and Hospital.Name ascending
        oData <- oData[order(oData$Rate, oData$Hospital.Name), ]

        # number of rows in data frame
        nRows <- nrow(oData)

        if (!is.character(num) && as.numeric(num) > nRows) {
                return ("NA")
        }

        # add sequentially indexed column to data frame
        # to represent ranking of hospitals by outcome
        oData$Rank <- seq.int(1, nRows, by = 1)

        if (!is.character(num)) {
                nRank <- num
        } else if(num == "worst") {
                nRank <- nRows
        }

        oData <- subset(oData, oData$Rank == nRank)

        return (oData[[1]])
}