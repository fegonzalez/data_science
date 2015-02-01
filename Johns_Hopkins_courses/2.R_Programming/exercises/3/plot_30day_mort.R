#plot_30day_mort: 1 Plot the 30-day mortality rates for heart attack (column 11)
plot_30day_mort <- function()
{
  # Read the outcome data into R via the read.csv function and look at the first
  # few rows.
  outcome<-read.csv("outcome-of-care-measures.csv", colClasses = "character");
  head(outcome)
  
  # Make a simple histogram of the 30-day death rates from heart attack (col 11)
  aux_col <- as.numeric(outcome[,11]); 
  hist(aux_col);
  
  estados_col <- (aux[,7]);
  
  return(outcome);
}## plot_30day_mort
