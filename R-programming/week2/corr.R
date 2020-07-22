corr <- function(directory, threshold = 0) {
    compl <- complete(directory)
    filenames <- matrix(character(332),332,1)
    agg.data <- NULL
    meets.id <- NULL
    agg.data.meets <- NULL
    
    for(i in 1:332) {
        
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
        agg.data <- rbind(agg.data,temp.data)
        
        if(compl[i,"nobs"] >= threshold){
            meets.id <- c(meets.id, compl[[i,"id"]])
        }
        
    }
    
    filenames.meets <- matrix(character(length(meets.id)),length(meets.id),1)
    correlation <- NULL
    
    if(length(meets.id) != 0){
        
        for(i in meets.id) {
        
            if(i < 10){
                filenames.meets[i] <- paste(getwd(),"/",directory,"/00",i,".csv",sep="")
            }
            else if(i >= 10 && i < 100){
                filenames.meets[i] <- paste(getwd(),"/",directory,"/0",i,".csv",sep="")
            }
            else{
                filenames.meets[i] <- paste(getwd(),"/",directory,"/",i,".csv",sep="")
            }
        
        temp.data.meets <- read.csv(filenames[i])
        good.temp.data.meets <- complete.cases(temp.data.meets)
        temp.data.meets <- temp.data.meets[good.temp.data.meets,]
        correlation[i] <- cor(temp.data.meets$sulfate,temp.data.meets$nitrate)
        }
    
    good.correlation <- complete.cases(correlation)
    correlation <- correlation[good.correlation]
    
    }
    
    else{numeric()}
}