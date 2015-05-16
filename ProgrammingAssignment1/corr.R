corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations

        # create empty correlations vector
        # allows for default = 0
        cr <- numeric(0)

        # create data frame using all files (id=1:332)
        df <- complete(directory)
        # apply threshold
        df <- df[df$nobs > threshold, ]

        for (id in df$id) {
                # set path of each data file
                path <- file.path(directory, paste(sprintf("%03d",
                        as.numeric(id)),
                        ".csv", sep=""))

                # get data from each file
                data <- read.csv(path)

                # get correlations for each file
                cr <- c(cr, cor(data$sulfate,
                        data$nitrate,
                        use = "pairwise.complete.obs"))
        }

        return (cr)
}