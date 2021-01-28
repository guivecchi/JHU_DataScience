pollutantmean <- function(directory, pollutant, id = 1:332){
    data <- NA
    
    for(i in id){
        if(i < 10){
            df <- read.csv(paste(directory, "/00", i, ".csv", sep=""))
        } else if(i < 100){
            df <- read.csv(paste(directory, "/0", i, ".csv", sep=""))
        } else{
            df <- read.csv(paste(directory, "/", i, ".csv", sep=""))
        }
        data <- rbind(data, df)
    }
    mean(data[[pollutant]], na.rm = TRUE)
}