
source("base.R")

##-------------------------------------------------------------------------

## best <- function(state, outcome) 
## 
## Return hospital name in that state with lowest 30-day death rate:
##
# Hospitals that do not have data on a particular outcome should be excluded 
# from the set of hospitals when deciding the rankings
#
# Handling ties. If there is a tie for the best hospital for a given outcome, 
# then the hospital names should be sorted in alphabetical order and the first 
# hospital in that set should be chosen (i.e. if hospitals "b", "c", and "f" 
# are tied for best, then hospital "b" should be returned).
##
##\param state: state name
##\param outcome: outcome name
##
best <- function(state, outcome) {

  ## 1) Check that state and outcome are valid
  validate_state(state);
  outcome_col <- validate_and_get_outcome(outcome);
    
  ## 2) Read outcome data
  ret_value <- character();
  HOSPITAL_COL_NAME <- "Hospital.Name";
  STATE_COL_NAME <- "State";
  
#browser();
  outcome_data_frame <-read.csv(get_outcome_filename(), colClasses="character");
  outcome_data_frame[, outcome_col$col_number] <- 
    as.numeric(outcome_data_frame[, outcome_col$col_number]);#numeric column !
  outcome_data_frame <- na.omit(outcome_data_frame);
  
  ## 3) Return hospital name in that state with lowest 30-day death rate
  outcome_data_frame <- subset(outcome_data_frame, State == state, 
           select=c(HOSPITAL_COL_NAME, outcome_col$col_name));
  min_value <- min(outcome_data_frame[[outcome_col$col_name]],na.rm = TRUE);
  ret_value <- 
    outcome_data_frame$Hospital.Name[outcome_data_frame[[outcome_col$col_name]] 
                                   == min_value];

#browser();
#   min_col_as_string <- outcome_data_frame[[outcome_col$col_name]];
#   min_col_as_numeric <- as.numeric(min_col_as_string);
#   min_value <- min(min_col_as_numeric,na.rm = TRUE);   #as.numeric !!!
#   ret_value <- outcome_data_frame$Hospital.Name[as.numeric
#               (outcome_data_frame[[outcome_col$col_name]]) 
#               == as.character(min_value)];
#browser();


  if(!is.character(ret_value)){
    stop("Bad output, not a character vector.");
  }
  ret_value <- sort(ret_value, decreasing = FALSE, na.last = NA)[1];
  return(ret_value);
}
