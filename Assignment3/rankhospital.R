


rankhospital <- function(state, outcome, num = "best") {
        ##read outcome data with measures of outcomes of care 
        Outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        
        ## Check that state is valid
        st<-unique(Outcome$State)
        styes<-which(st==state)
        if(length(styes)==0) stop("invalid state") 
        
        
        ## Check that outcome is valid
        out<-c("heart attack","heart failure","pneumonia")
        outyes<-which(outcome==out)
        if(length(outyes)==0) stop("invalid outcome") 
        
        
        ## Check that num is valid
        if(!(num=="best"|num=="worst"|is.numeric(num))) stop ("valid num argument are best,worst or a positive number")
        
        
        
        ## subset the table to hospital name and lowest 30 day death rate variable
        narrow<-Outcome[Outcome$State==state,c(2,11,17,23)]
        
        ## creates  a variable to associate outcome variable
        n<-outyes+1
        
        ## this function order the table in from the lowest to the highest and in alphabetical order
        ordert<-function(n){
                narrow[,n]<-as.numeric(narrow[,n])
                ordered<-narrow[order(narrow[,n],narrow[,1],na.last=NA),]          
                if(num=="best") num<-1
                if (num=="worst") num<-nrow(ordered)
                ordered[num,1] 
                
        }
        ## Return hospital name in that state with lowest 30-day death rate and suppress warning message
        suppressWarnings(ordert(n))
}