rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if(outcome == "heart attack") {
                disease <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if(outcome == "heart failure") {
                disease <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else if(outcome == "pneumonia") {
                disease <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        } else {
                return(paste("Error in best(",'"',state,'",', '"',outcome,'"',") : invalid outcome"))
        }
        
        ## For each state, find the hospital of the given rank
        state <- unique(data$State)
        save_data <- data.frame()
        if(num == "best") {
                nnum <- 1
        } else if(is.numeric(num)) {
                nnum <- num
        }
        state <- state[order(state)]
        for(s in state) {
                sdata <- data[data$State == s, ]
                subdata <- subset(sdata, , select = c("Hospital.Name", "State", disease))
                colnames(subdata) <- c("hospital", "states", "disease")
                dnadata <- subdata[complete.cases(as.numeric(subdata$disease)), ]
                rdata <- dnadata[order(as.numeric(dnadata$disease), dnadata$hospital, dnadata$states), ]
                rdata <- rdata[,-3]
                if(num == "worst") {
                        nnum <- nrow(rdata)
                }
                if(is.na(rdata[nnum,]$states)) {
                        rdata[nnum,]$states <- s
                }
                save_data <- rbind(save_data, rdata[nnum,])
        }
        
        ## Return a data frame with the hospital names and the
        ## (abberviated) state name
        return(save_data)
}