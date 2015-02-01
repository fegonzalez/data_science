## base code included along the asssigment exercises

##-------------------------------------------------------------------------

get_outcome_filename <- function()
{ return ("outcome-of-care-measures.csv"); } #DB 19

##-------------------------------------------------------------------------
get_hospital_filename <- function()
{ return ("hospital-data.csv"); } #DB 11

##-------------------------------------------------------------------------
get_valid_outcomes <- function()
{
  return(c("heart attack", "heart failure", "pneumonia"));
}

##-------------------------------------------------------------------------

##functionvalidate_and_get_outcome <- function(outcome)
#
#return a list {column index,  name of the outcome} regarding the received
# outcome type.
#
# input values: "heart attack", "heart failure", "pneumonia"
# output values:
# 11, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
# 17, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
# 23, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
validate_and_get_outcome <- function(outcome)
{  
  CONST_11 <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack";
  CONST_17 <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure";
  CONST_23 <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia";
  
  pos <- match(outcome, get_valid_outcomes());
  switch(as.character(pos),
         "1"={
           retval <- list(col_number = 11, col_name = CONST_11);
         },
         "2"={
           retval <- list(col_number = 17, col_name = CONST_17);
         },
         "3"={
           retval <- list(col_number = 23, col_name = CONST_23);
         },
{ 
  stop("invalid outcome");
}
  )

return(retval);
}

##-------------------------------------------------------------------------

validate_state <- function(state)
{
  if( ! is.element(state, state.abb))
  {
    stop("invalid state");
  }
}

##-------------------------------------------------------------------------

