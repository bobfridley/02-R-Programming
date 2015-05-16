complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

        # create empty data frame
        df <- data.frame(i = integer(), nobs = integer())

        for (i in id) {
                # set path of each data file
                path <- file.path(directory, paste(sprintf("%03d",
                        as.numeric(i)),
                        ".csv", sep=""))

                # get data from each file
                data <- read.csv(path)

                # create data frame
                nd <- data.frame(id = i,
                        nobs = nrow(na.omit(data)))

                # bind data frame "nd" with data frame "df"
                df <- rbind(df, nd)
        }

        return (df)
}