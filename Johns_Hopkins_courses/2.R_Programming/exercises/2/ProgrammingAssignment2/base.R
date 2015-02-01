## base code included along the asssigment exercises

##-------------------------------------------------------------------------

get_outcome_filename <- function()
{ return "outcome-of-care-measures.csv"; }

##-------------------------------------------------------------------------
get_hospital_filename <- function()
{ return "hospital-data.csv"; }

##-------------------------------------------------------------------------
get_valid_outcomes <- function()
{
  return(c("heart attack", "heart failure", "pneumonia"));
}

