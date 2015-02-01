
source("base.R")


##\function rankhospital(state, outcome, num = "best")
#
#\param state: state name.
#\param outcome: outcome name.
#\param num: the ranking of a hospital in that state for that outcome.
# The num argument can take values "best", "worst", or an integer indicating the 
# ranking (smaller numbers are better). If the number given by num is larger 
# than the number of hospitals in that state, then the function should return NA.
#
#\return hospital name in that state with the given rank (num) 30-day death rate
#
#i.e.- rankhospital("MD", "heart failure", 5)
#      would return a character vector containing the name of the hospital with 
#      the 5th lowest 30-day death rate for heart failure.
##
rankhospital <- function(state, outcome, num = "best") 
{ 
  ret_value <- character();
  HOSPITAL_COL_NAME <- "Hospital.Name";
  STATE_COL_NAME <- "State";
  
  ## 1) Check that state and outcome are valid
  validate_state(state);
  outcome_col <- validate_and_get_outcome(outcome);
  
  ## 2) Read outcome data
  outcome_data_frame <-read.csv(get_outcome_filename(), colClasses="character");
  outcome_data_frame[, outcome_col$col_number] <- 
    as.numeric(outcome_data_frame[, outcome_col$col_number]); 
  outcome_data_frame <- na.omit(outcome_data_frame);
  outcome_data_frame <- subset(outcome_data_frame, State == state, 
                          select=c(HOSPITAL_COL_NAME, outcome_col$col_name));
  HOSPITAL_COL_NUMBER=1;
  OUTCOME_COL_NUMBER=2;
  
  #order & get output (step by step)
  #getting oredered row-index vector
  retvalue_ordered_rowindex<-(order(outcome_data_frame[[outcome_col$col_name]],
        outcome_data_frame[[HOSPITAL_COL_NAME]], na.last = TRUE));
  
  #browser();
  #selecting the index position
  if(num=="best")
  {
    num=1;
  }
  else if(num=="worst")
  {
    num = length(retvalue_ordered_rowindex);
  }
  
  #selecting the value to return
  if(num > length(retvalue_ordered_rowindex))
  {
    return(NA);
  }
  else
  {
    retvalue_row_number <- retvalue_ordered_rowindex[num];
    ret_value <- outcome_data_frame[retvalue_row_number, HOSPITAL_COL_NUMBER]; 
    if(!is.character(ret_value)){
      stop("Bad output, not a character vector.");
    }  
    return(ret_value);
  }
  
}

