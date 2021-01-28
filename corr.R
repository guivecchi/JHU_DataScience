corr <- function(directory, threshold = 0){
    correlations <- c()
    files <- list.files(path=paste("./", directory, sep = ""), pattern="*.csv", full.names=TRUE, recursive=FALSE)
    
    id = 1
    for(file in files){
        if(complete(directory, id)$nobs > threshold){
            df <- read.csv(file)
            correlations <- c(correlations, cor(df$sulfate, df$nitrate, use = "complete.obs"))
        }
        id <- id + 1
    }
    
    correlations
}