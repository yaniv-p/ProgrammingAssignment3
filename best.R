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
        
        #convert to numeric
        outcomedb[,NamedId]<-as.numeric(outcomedb[,NamedId])
        
        
                                                                                          
        #Return hospital name in that state with lowest 30-day death rate
        #b<-(!is.na(outcomedb[,11])) & (outcomedb[,7] == state)
        
}