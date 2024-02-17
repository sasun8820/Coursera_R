# Name: Jeonseo David Lee 
# Course: R Programming 
# *** This project is dealt with major hospital data in the US in 2013 downloaded from the
# Hospital Compare website and aims to deliver how health care had been implemented 
# to the overall US citizens. ***

#### Q1. Plot the 30-day mortality rates for heart attack ####

outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

# To see the column/index number
colnames(outcome) 
outcome[ , 11] = as.numeric(outcome[, 11], na.rm = TRUE)

hist(outcome[, 11], xlab="Heart Attack Mortality Rates",
     main = "30-Day Mortality Rates for Heart Attack")


#### Q2. Finding the best hospital in a state ####

# outcome can be either heart attack, heart failure, or pneumonia 
# First, I made a new data frame only consisting of the desired variables 
data = data.frame(cbind(outcome[, 2],
                        outcome[, 7],
                        outcome[, 11],
                        outcome[, 17],
                        outcome[, 23]), stringsAsFactors = FALSE)
colnames(data) = c("Hospital Name", "State", "Heart Attack",
                   "Heart Failure", "Psenumonia")
head(data)

# Assigned a new function called "best"  

best = function(state, outcome){
     if (!outcome %in% c("Heart Attack", "Heart Failure", "Psenumonia")){
          stop("invalid outcome")
     }
     if (!state %in% data$State) {
          stop("invalid state")
     }
 
     # Retrieve the row aligning with the respective "state"
     new_state = data[(data[, "State"]== state), ]
     
     # outcome in numeric values   
     new_state[, outcome] = as.numeric(new_state[ , outcome])
     
     # outcome in an ascending order
     new_state = new_state[order(new_state[ , outcome]), ]
     
     new_state = new_state[!is.na(new_state[, outcome]), ]
     
          #new_state[order(new_state[, outcome]) , ]

     # Time to organize by outcomes
     if(outcome == "Heart Attack"){
          hName = new_state[new_state[ , "Heart Attack"] == min(new_state[, "Heart Attack"]) , 1]
     }
     if(outcome == "Heart Failure") {
          hName = new_state[new_state[ , "Heart Failure"] == min(new_state[, "Heart Failure"]) , 1]
     }
     if(outcome == "Psenumonia"){
          hName = new_state[new_state[ , "Psenumonia"] == min(new_state[, "Psenumonia"]) , 1]
     }
     
     sort(hName)[1]
     # or coulud be as follow: hName = new_state[new_state[, outcome] == min(new_state[,outcome]), 1]
     
print(hName)
}
best("MD", "Heart Attack")    

#### Q3. Ranking hospitals by outcome in a state ####

# Assign the new function 
outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")

data3 = data.frame(cbind(outcome[, 2],
                         outcome[, 7],
                         outcome[, 11],
                         outcome[, 17],
                         outcome[, 23]), stringsAsFactors = FALSE)
colnames(data3) = c("Hospital Name", "State", "Heart Attack",
                    "Heart Failure", "Psenumonia")

head(data3)

rankhospital = function(state, outcome, num= "best") {
     # Check the validity 
     if (!outcome %in% c("Heart Attack", "Heart Failure", "Psenumonia")){
          stop("invalid outcome")
     }
     
     if (!state %in% data3$State) {
          stop("invalid state")
     }
  
     if (is.character(num)){
          if(!num %in% c("best", "worst"))
               stop("invalid rank")
     }
     
     # Filter by state 
     new_data = data3[(data3[, "State"] == state), ]
     
     # Outcomes in numerics 
     new_data[ , outcome] = as.numeric(new_data[, outcome])
     # Remove NA in outcomes
     new_data = new_data[!is.na(new_data[, outcome]), ]
     
     # Define num in "best", "worst", or any integer in a rank 
     if (num == "best") {
          num = 1
     }
     if (num == "worst"){
          num = nrow(new_data) # to find rows, do not apply fliter in the data set
     }
     if (is.numeric(num)){
          if (num > length(new_data[, "Hospital Name"])){ 
               return(NA)}}
     
     new_data = new_data[order(new_data[, outcome], new_data[ ,"Hospital Name"]), ]
     
     Hname = new_data[num, 1]
     
     print(Hname)
}

rankhospital("TX", "Heart Failure", 4)
rankhospital("MD", "Heart Attack", "worst")


#### 4. Ranking hospitals in all states #### 
outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
colnames(outcome)
head(outcome)

rankall = function(outcome, num) { 
     if(!outcome %in% c("Heart Attack", "Heart Failure", "Psenumonia")) {
          stop("invalid outcome")
     }
     
     if (is.character(num)) {
          if (!num %in% c("best", "worst")) {
               stop("invalid rank")
          }
     }
     
     new_df = data.frame(hospital = character(), state = character())
     
          
     data = data.frame(cbind(outcome[, 2],
                              outcome[, 7],
                              outcome[, 11],
                              outcome[, 17],
                              outcome[, 23]), stringsAsFactors = FALSE)
     colnames(data) = c("Hospital Name", "State", "Heart Attack",
                              "Heart Failure", "Psenumonia")
     head(data)
     # by state
     for (i in 1:length(data$State)) {
     # num 
          if (num == "best") {
               rnum = 1
          }
          if (num == "worst") {
               rnum = nrow(data)
          }
          if (is.numeric(num)){
               if (num > length(data[, "Hospital Name"])){ 
                    return(NA)}
          }
        
          # Return w/ filtering 
          new_data = data[order(data[, outcome], data[, "Hospital Name"]), ]
          Hos_name = new_data[rnum, 1] 
          Sta_name = new_data[rnum, 2]
          
     }
     new_df = rbind(new_df, 
                    data.frame(hospital = Hos_name, 
                               state = Sta_name))
     new_df
}

rankall("Heart Attack", 20)
