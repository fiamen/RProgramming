
submit

pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        setwd(directory)
        files<-dir()
        tableall<-NULL?se
        for (i in id){
                table<-read.csv(files[i])
                tableall<-rbind(table,tableall)            
                
                              
        }
        
        if (pollutant=="sulfate"){
                mean(tableall$sulfate,na.rm=TRUE)
        }
        
        else if (pollutant=="nitrate"){
                mean(tableall$nitrate,na.rm=TRUE)
        }
        
       else {" not a valid argument pollutant"}
       
       setwd("C:/BI/R/coursera/CourseraRProgramming/scripts/RProgramming")
        
}