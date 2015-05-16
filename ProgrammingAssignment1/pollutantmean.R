pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

        m <- c()

        for (i in id) {
                # read csv data file identified in filelist by 'i'
                path <- file.path(directory, paste(sprintf("%03d",
                        as.numeric(i)),
                        ".csv", sep=""))

                # read data file
                data <- read.csv(path)

                # identify missing data
                b <- is.na(data[[pollutant]])

                # store pollutant data in vector
                m <- c(m, data[[pollutant]][!b])
        }

        # return mean of pollutant, rounded to 3 digits
        return (round(mean(m), digits=3))
}