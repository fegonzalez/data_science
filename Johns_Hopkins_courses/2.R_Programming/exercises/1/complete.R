
# Write a function that reads a directory full of files and reports the number
# of completely observed cases in each data file. The function should return a
# data frame where the first column is the name of the file and the second
# column is the number of complete cases. A prototype of this function follows

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases

complete <- function(directory, id = 1:332) 
{
  ret_value = NA;
  id_vector <- c();
  nobs_vector <- c();
  the_file_list <- list.files(directory);
  
  for  (loop_id in id)
  { 
    #setting filename optimized (because directory only have these files and 
    #their name follows the 'ids')
    filename <- paste(directory, the_file_list[loop_id], sep = "/");
    #print(filename);
    stopifnot(file.exists(filename)); #assert
    
    ## For each 'id' file get the number of non-na rows in the file 
    raw_data = read.csv(filename);
    valid_data <- na.omit(raw_data)
    id_vector <- c(id_vector, loop_id);
    nobs_vector <- c(nobs_vector, nrow(valid_data));
  }
  
  ret_value <- data.frame(id_vector, nobs_vector);
  names(ret_value) <- c("id", "nobs")
  return (ret_value);
  
}#end function

