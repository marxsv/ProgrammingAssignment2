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
    
    c <- numeric()
    
    for(k in 1:length(id)){
        
        if(id[k] < 10){
            dir <- paste(directory,"/00",id[k],".csv",sep="")}
        else if(id[k]<100){
            dir <-  paste(directory,"/0",id[k],".csv",sep="")}
        else {
            dir <- paste(directory,"/",id[k],".csv",sep="")}
        
        mon <- read.csv(dir)
        c[k] <- sum(complete.cases(mon))
    }
    
    comp = data.frame(id = id,nobs = c)
    
    print(comp)
}