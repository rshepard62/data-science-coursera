pollutantmean <- function(directory, pollutant, id = 1:332) {
    agg.data <- NULL
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
        agg.data <- c(agg.data,temp.data[[pollutant]])
        
    }
    
    good <- complete.cases(agg.data)
    good.agg.data <- agg.data[good]
    pollutantmean.value <- mean(good.agg.data)
    
    print(pollutantmean.value)
    
}



