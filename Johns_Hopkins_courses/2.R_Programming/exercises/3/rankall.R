
source("base.R")


##\function rankall(outcome, num = "best")
#
#\param outcome: outcome name.

#\param num: the ranking of a hospital in that state for that outcome. The num
#argument can take values "best", "worst", or an integer indicating the ranking
#(smaller numbers are better). If the number given by num is larger than the
#number of hospitals in that state, then the function should return NA.
#
#\return a 2-column data frame (hospital name, SS name) containing the hospital
#        in each state that has the ranking specified in num for the given
#        outcome.  
#
#        The function should return a value for every state (some may be NA).
#        Hospitals that do not have data on a particular outcome should be
#        excluded from the set of hospitals when deciding the rankings.
#
#  i.e.: print(head(rankall("heart attack", 20), 10));
#
#         hospital                              state
#   AK   <NA>                                     AK
#   AL   D W MCMILLAN MEMORIAL HOSPITAL           AL
#   AR   ARKANSAS METHODIST MEDICAL CENTER        AR
#
##
##\warning For the purpose of this part of the assignment the assignment 
##        (and for eficiency), your function should NOT call the rankhospital 
##        function from the previous section.
##

rankall <- function(outcome, num = "best") 
{
  ## 1) Input validation
  outcome_col <- validate_and_get_outcome(outcome);
  
  HOSPITAL_COL_NAME <- "Hospital.Name";
  STATE_COL_NAME <- "State";
  OUTCOME_COL_NAME <- outcome_col$col_name; 
  OUTPUT_HOSPITAL_COL_NAME <- "hospital";
  OUTPUT_STATE_COL_NAME <- "state";
  
  ## 2) Read outcome data
  outcome_data_frame <-read.csv(get_outcome_filename(), colClasses="character");
  outcome_data_frame[, outcome_col$col_number] <- 
    as.numeric(outcome_data_frame[, outcome_col$col_number]); 
  outcome_data_frame <- na.omit(outcome_data_frame);
  outcome_data_frame <- 
    subset(outcome_data_frame, 
          select=c(STATE_COL_NAME, HOSPITAL_COL_NAME, outcome_col$col_name));
  STATE_COL_NUMBER <- 1;
  HOSPITAL_COL_NUMBER <- 2;
  OUTCOME_COL_NUMBER <- 3;

  
  ## 3) For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  # 3.1) split_by _state = 
  #
  #   i.e. split_by _state[["WY"]] =
  #
  #   $WY
  #          State    Hospital.Name                     Hospital.30.Day.Death...
  #   4634    WY       SHERIDAN MEMORIAL HOSPITAL            18.3
  #   4638    WY       WYOMING MEDICAL CENTER                14.9
  #   4639    WY       CHEYENNE REGIONAL MEDICAL CENTER      15.3
  
  split_by_state <- split(outcome_data_frame, 
                          outcome_data_frame[[STATE_COL_NAME]]);
  
  # 3.2) order(split_by_state)  
  #
  #browser();
  ordered_by_state <- 
    lapply(X = split_by_state, 
           FUN = value_at_num, 
           outcome_col_name = OUTCOME_COL_NAME, 
           hospital_col_name = HOSPITAL_COL_NAME,
           num = num);

  # 3.3) For each (already ordered) State, get the values at colums 
  #   "Hospital.Name" and "State", and create a data.frame with all of them.
  #
  # i.e. (ordered for num=4), state_index = 3
  #
  #   Hospital.Name State
  #   106                 ALASKA NATIVE MEDICAL CENTER    AK
  #   31                            GEORGIANA HOSPITAL    AL
  #   232 VA CENTRAL AR. VETERANS HEALTHCARE SYSTEM LR    AR
  #
  total_ret_value <- data.frame();
  next_ret_value <- data.frame();
  for(state_index in 1:length(ordered_by_state))
  {
    #\warning: NA case: create void data.frame for the State value.
    if(is.na(ordered_by_state[[state_index]]))
    {
     #browser();
     next_ret_value <- rbind(c(NA, names(split_by_state[state_index])));
     colnames(next_ret_value)<-c(HOSPITAL_COL_NAME, STATE_COL_NAME)
    }
    else
    {
      next_ret_value <- 
                                      #row in split_by_state [[state_index]]
      split_by_state [[state_index]] [ordered_by_state[[state_index]], 
                                      #cols in split_by_state [[state_index]]
                                     c(HOSPITAL_COL_NUMBER, STATE_COL_NUMBER)];
    }
    total_ret_value <- rbind(total_ret_value, next_ret_value); 
  }
  
  # finally change row & col names to match the expected output.
  rownames(total_ret_value) <- total_ret_value[[STATE_COL_NAME]];
  colnames(total_ret_value) <- 
    c(OUTPUT_HOSPITAL_COL_NAME, OUTPUT_STATE_COL_NAME);
  
  return (total_ret_value);

}

##-------------------------------------------------------------------------

value_at_num <- function(input_data_set, 
                         outcome_col_name, 
                         hospital_col_name, 
                         num = "best") 
#\param input_data_set: list, data.fram, ...
#\param num: see 'num 'input param at rankall function.
{   
  ordered_vector <- order(input_data_set[[outcome_col_name]],
                          input_data_set[[hospital_col_name]],
                          na.last = TRUE);
  
  #selecting the index position
  if(is.character(num))
  {
    if(num=="best")
    {
      return (ordered_vector[1]);
    }
    else if(num=="worst")
    {
      return (ordered_vector[length(ordered_vector)]);
    }
  }#end if is.character()
  else if(is.numeric(num))
  {
    #selecting the value to return
    if(num > length(ordered_vector))
    {
      return(NA);
    }
    else
    {
      return (ordered_vector[num]);
    }
  }#end if is.numeric()
  else
  {
   stop(cat("Invalid num (", num, ")")); 
  }
}#end value_at_num()

