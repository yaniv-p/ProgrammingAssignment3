rankhospital <- function(state, outcome, num = 'best') {
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
                
## Return hospital name in that state with the given rank 30-day death rate
        
        b<-(outcomedb[[NamedId]] != 'Not Available') & (outcomedb[[7]] == state) # flag vector for the choosen state and without NA
        
        m<-cbind(outcomedb[[2]][b],outcomedb[[NamedId]][b]) #build a Matrix based on the flag vector so we can order it 
        element_order<-order(as.numeric(m[,2]),m[,1]) # will retuen a vector with the elemants id in the requered order of value and the Hospitol name
        v<-m[element_order,1] # take the Hospital name from the ordered matrix
        if (num == 'best') {
          i=1 
        } else if (num == 'worst'){
          i=length(v)
        } else i=num
        v[i] 
}