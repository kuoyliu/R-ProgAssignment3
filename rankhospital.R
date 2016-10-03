rankhospital <- function(state, outcome, num = "best") {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        sdata <- data[data$State == state, ]
        if(length(sdata[, 1]) == 0) {
                return(paste("Error in best(",'"',state,'",', '"',outcome,'"',") : invalid state"))
        }
        if(outcome == "heart attack") {
                disease <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if(outcome == "heart failure") {
                disease <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if(outcome == "pneumonia") {
                disease <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                return(paste("Error in best(",'"',state,'",', '"',outcome,'"',") : invalid outcome"))
        }
        
        ## Return hospital name in that state with the given rank
        subdata <- subset(sdata, , select = c("Hospital.Name", disease))
        colnames(subdata) <- c("name", "disease")
        dnadata <- subdata[complete.cases(as.numeric(subdata$disease)), ]
        rdata <- dnadata[order(as.numeric(dnadata$disease), dnadata$name), ]
        
        if(num == "best") {
                num <- 1
        } else if(num == "worst") {
                num <- as.numeric(length(rdata))
        }
        ## 30-day death rate
        return(rdata[num, 1])
}