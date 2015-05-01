rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomedb <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that num and outcome are valid
                
        if (outcome =="heart attack" ) {
                NamedId=11
        } else if (outcome =="heart failure" ){
                NamedId=17
        } else if (outcome =="pneumonia" ) {
                NamedId=23
        } else stop("invalid outcome")

        if (num !='best' && num !='worst' && !is.numeric(num)  ) stop("invalid num")
                
        ## For each state, find the hospital of the given rank  
        
        b<-(outcomedb[[NamedId]] != 'Not Available') # flag vector for entries without NA
        #create a data frame for enries without NA that have Hospitol, dead rate and state
        df<-data.frame(hospital=outcomedb[[2]][b],rate=as.numeric(outcomedb[[NamedId]][b]), State=outcomedb[[7]][b])
        #order the data.frame per rate and Hospital name
        df1<-df[order(df$rate,df$hospital),]
        #create a list that have data frame per state
        l<-split(df1,df1$State)

        l1<-lapply(l,function(x) {
                                        if (num == 'best') {
                                                i=1 
                                        } else if (num == 'worst'){
                                                i=dim(x)[1]
                                        } else i=num
                                        data.frame(hospital=as.character(x[i,1]),state=as.character(x[1,3]))
                                }
                )
        c1<-NULL
        for (i in 1:length(l1)) { c1<-rbind(c1,(l1[[i]]))}        
        c1
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}