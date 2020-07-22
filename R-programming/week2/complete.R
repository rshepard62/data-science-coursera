complete <- function(directory, id = 1:332) {
    nobs <- NULL
    filenames <- matrix(character(length(id)),length(id),1)
    
    for(i in id) {
        
        if(i < 10){
            filenames[i] <- paste(getwd(),"/",directory,"/00",i,".csv",sep="")
        }
        else if(i >= 10 && i < 100){
            filenames[i] <- paste(getwd(),"/",directory,"/0",i,".csv",sep="")
        }
        else{
            filenames[i] <- paste(getwd(),"/",directory,"/",i,".csv",sep="")
        }
        
        temp.data <- read.csv(filenames[i])
        good.temp.data <- complete.cases(temp.data)
        temp.data <- temp.data[good.temp.data,]
        nobs <- c(nobs,(nrow(temp.data)))
        
    }
    
    complete.data <- data.frame(id,nobs)
    
    complete.data
    
}