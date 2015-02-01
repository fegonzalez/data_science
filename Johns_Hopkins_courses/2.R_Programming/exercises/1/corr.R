# Write a function that takes a directory of data files and a threshold for
# complete cases and calculates the correlation between sulfate and nitrate for
# monitor locations where the number of completely observed cases (on all
# variables) is greater than the threshold. The function should return a vector
# of correlations for the monitors that meet the threshold requirement. If no
# monitors meet the threshold requirement, then the function should return a
# numeric vector of length 0. A prototype of this function follows


## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
##
## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0
##
## Return a numeric vector of correlations
corr <- function(directory, threshold = 0) 
{
  stopifnot(threshold>=0); #assert
  ret_value <- c();
  the_file_list <- list.files(directory);
  
  ## (for the threshold-valid files only) calculate the correlation
  completed_files_list <- complete(directory, 1:length(list.files(directory)));
  #       id nobs
  #   1   1  117
  #   2   2 1041
  #   ...
  for  (loop_id in 1:nrow(completed_files_list))
  {
    if(completed_files_list[loop_id, "nobs"] < threshold)
    { 
      next;
    }

    filename <- paste(directory, the_file_list[loop_id], sep = "/");
    #print(filename);
    stopifnot(file.exists(filename)); #assert
    raw_data = read.csv(filename);
    valid_data <- na.omit(raw_data);
    ret_value <- c(ret_value, cor(valid_data$sulfate, valid_data$nitrate));

}##end_for
  
  return (ret_value);
}
