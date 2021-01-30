complete <- function(directory, id = 1:332){
  id_vec <- c()
  nobs <- c()
  for(i in id){
      id_vec <- c(id_vec, i)
      if(i < 10){
        df <- read.csv(paste(directory, "/00", i, ".csv", sep=""))
      } else if(i < 100){
        df <- read.csv(paste(directory, "/0", i, ".csv", sep=""))
      } else{
        df <- read.csv(paste(directory, "/", i, ".csv", sep=""))
      }
      nob_id <- sum(!is.na(df$nitrate) & !is.na(df$sulfate))
      nobs <- c(nobs, nob_id)
  }
  output <- data.frame(id_vec, nobs)
  colnames(output)[1] <- "id"
  output
}