corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        
        ## require the function complete to test the files for completeness
        source("complete.R")
        

        
        ## creates a the table test with nobs value for each file
        test<-complete(directory)
        
        ##set directory to location of files
        ##setwd(directory)
        
        ## list of file on the working directory
        files<-dir()
        
        ## creates a vector reference to completed files greater than the threshold
        ref<-NULL
        for (i in 1:332){
                if(test[i,2]>threshold){
                    ref<-c(ref,i)    
                }
        }       
        ## calculates correlation for completed files within the threshold
        corre<-NULL
        for (i in ref){
                
                table<-read.csv(files[i])
                table$nobs<-with(table,sulfate+nitrate)
                
                ## creates a table with complete records
                incomplete <- is.na(table$nobs)
                tableC<-table[!incomplete,]
                
                ## calculates the correlation between sulfate and nitrate variable
                correlation<-cor(tableC$sulfate,tableC$nitrate)
                
                ## creates a vector of correlation values
                corre<-c(correlation,corre)           
                
             
        }
           
        setwd("../")
        
        ## prints the correlation vector to the console     
          corre      
                
        
               
                
        }
        
        
        
        
        
