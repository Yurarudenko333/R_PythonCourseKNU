pmean <- function(directory, pollutant, id=1:332) {
  if(pollutant=="sulfate"){
    x <- 2
  }
  if(pollutant=="nitrate"){
    x <- 3
  }
  l <- c()
  for (i in id){
    if(i<10){
      specdata <- read.csv(paste(directory, "/", "00", as.character(i), ".csv", sep=''), header = T, sep = ",")
      l <- c(l,mean(specdata[,x], na.rm = T))
    } else if(i<100){
      specdata <- read.csv(paste(directory, "/", "0", as.character(i), ".csv", sep=''), header = T, sep = ",")
      l <- c(l,mean(specdata[,x], na.rm = T))
    }
    else{
      specdata <- read.csv(paste(directory, "/", as.character(i), ".csv", sep=''), header = T, sep = ",")
      l <- c(l,mean(specdata[,x], na.rm = T))
    }
  }
  mean(l, na.rm = T)
  
}

complete <- function(directory, id) {
  col1 <- c()
  col2 <- c()
  for (i in id){
    if(i<10){
      specdata <- read.csv(paste(directory, "/", "00", as.character(i), ".csv", sep=''), header = T, sep = ",")
      col1 <- c(col1,i)
      col2 <- c(col2, nrow(na.omit(specdata)))
    } else if(i<100){
      specdata <- read.csv(paste(directory, "/", "0", as.character(i), ".csv", sep=''), header = T, sep = ",")
      col1 <- c(col1,i)
      col2 <- c(col2, nrow(na.omit(specdata)))
    }
    else{
      specdata <- read.csv(paste(directory, "/", as.character(i), ".csv", sep=''), header = T, sep = ",")
      col1 <- c(col1,i)
      col2 <- c(col2, nrow(na.omit(specdata)))
    }
  }
  data.frame(id=col1, nobs=col2)
  
}

corr <- function(directory, threshold=0) {
  if(threshold>=332){
    return(numeric(0))
  }
  index <- (threshold+1):332
  l <- c()
  for (i in index){
    if(i<10){
      specdata <- read.csv(paste(directory, "/", "00", as.character(i), ".csv", sep=''), header = T, sep = ",")
      specdata <- na.omit(specdata)
      l <- c(l, cor(specdata$sulfate, specdata$nitrate))
    } else if(i<100){
      specdata <- read.csv(paste(directory, "/", "0", as.character(i), ".csv", sep=''), header = T, sep = ",")
      specdata <- na.omit(specdata)
      l <- c(l, cor(specdata$sulfate, specdata$nitrate))
    }
    else{
      specdata <- read.csv(paste(directory, "/", as.character(i), ".csv", sep=''), header = T, sep = ",")
      specdata <- na.omit(specdata)
      l <- c(l, cor(specdata$sulfate, specdata$nitrate))
    }
  }
  l
  
}

