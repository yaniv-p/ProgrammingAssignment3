best <- function(state, outcome) {
        ## Read outcome data
        outcomedb <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if (!(state %in% unique(outcomedb$State)) ) stop("invalid state")
        
        if (outcome =="heart attack" ) {
                NamedId=11
        } else if (outcome =="heart failure" ){
                NamedId=17
        } else if (outcome =="pneumonia" ) {
                NamedId=23
        } else stop("invalid outcome")
                
        #Return hospital name in that state with lowest 30-day death rate
        
        b<-(outcomedb[[NamedId]] != 'Not Available') & (outcomedb[[7]] == state) # flag vector for the choosen state and without NA
        
        m<-cbind(outcomedb[[2]][b],outcomedb[[NamedId]][b]) #build a Matrix based on the flag vector
        m<-m[order(as.numeric(m[,2],m[,1])),] # order the matrix by the value and the Hospitol name
        m[1,1] # return the first entry (lower value)
}