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
        setwd(directory)
        files<-dir()
        NAnot<-function(x){
                sum(!is.na(x))
        }
        tablefinal<-NULL
        for (i in id){
                table<-read.csv(files[i])                
                table$nobs<-with(table,sulfate+nitrate)
                if(sum(table$nobs,na.rm=T)>0){
                tableall<-aggregate(nobs~ID,table,NAnot)
                }
                else {tableall<-c(i,0)}
                tablefinal<-rbind(tablefinal,tableall)
                
                
                
                         
                
                
        }
        
        tablefinal
        
 }