rankall <- function(outcome, num = "best") {
        
        ##read outcome data with measures of outcomes of care 
        Outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        
        ## Check that outcome is valid
        out<-c("heart attack","heart failure","pneumonia")
        outyes<-which(outcome==out)
        if(length(outyes)==0) stop("invalid outcome") 
        
        
        ## Check that num is valid
        if(!(num=="best"|num=="worst"|is.numeric(num))) stop ("valid num argument are best,worst or a positive number")
       
        
        
        ## creates  a variable to associate outcome variable
        p<-c(11,17,23)
        n<-p[outyes]
        
        
        ## this function order the table in from the lowest to the highest and in alphabetical order
        ordert<-function(n,state){
                ## subset the table to hospital name and lowest 30 day death rate variable
                narrow<-Outcome[Outcome$State==state,c(2,7,n)]
                narrow[,3]<-suppressWarnings( as.numeric(narrow[,3]))
                ordered<-narrow[order(narrow[,3],narrow[,1],na.last=NA),]          
                if(num=="best") num<-1
                if (num=="worst") num<-nrow(ordered)
                ordered[num,c(1,2)] 
        }        
        
        ## For each state, find the hospital of the given rank
        
        st<-unique(Outcome$State)
        
        newtable<-NULL
        for (i in st){
                table<-ordert(n,i)
                colnames(table)<-c("hospital","state")
                if(is.na(table)) table<-c(NA,i)
                else newtable<-rbind(newtable,table)
        }        
        
        
        ## Return a data frame with the hospital names and the (abbreviated) state name               
        newtable[order(newtable$state),]      
                
                
                
                
        
        
        
       
}