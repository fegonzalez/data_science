# Write a function named 'pollutantmean' that calculates the mean of a
# pollutant (sulfate or nitrate) across a specified list of monitors. The
# function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and
# 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors'
# particulate matter data from the directory specified in the 'directory'
# argument and returns the mean of the pollutant across all of the monitors,
# ignoring any missing values coded as NA. A prototype of the function is as
# follows


#test unit .- 
##
##source("pollutantmean.R")
# 
# init_time <- Sys.time(); pollutantmean(input_dir, "sulfate"); 
# elapsed_time=Sys.time()-init_time;
# RESULT was
# [1] 3.189369
# > elapsed_time
# Time difference of 4.177871 secs
#
# > init_time <- Sys.time(); pollutantmean(input_dir, "nitrate");
# elapsed_time=Sys.time()-init_time;
# [1] 1.702932
# > elapsed_time
# Time difference of 4.141918 secs
##
##
##\param 'directory' is a character vector of length 1 indicating
## the location of the CSV files
##
##\param 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".
##
##\param 'id' is an integer vector indicating the monitor ID numbers
## to be used
##
##\return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
pollutantmean <- function(directory, pollutant, id = 1:332)
{
  stopifnot((pollutant=="sulfate")||(pollutant=="nitrate"));

  all_valid_values = NA; #because we are going to concat all the column values

  for  (loop_id in id)
  { 
    #setting filename
    if(loop_id<10)
    {
      filename <- paste("00", loop_id, sep="");
      filename <- paste(filename, "csv", sep=".");
      filename <- paste(directory, filename, sep = "/");
    }
    else if(loop_id<100)
    {
      filename <- paste("0", loop_id, sep="");
      filename <- paste(filename, "csv", sep=".");
      filename <- paste(directory, filename, sep = "/");
    }
    else
    {
      filename <- paste(directory, paste(loop_id,"csv",sep="."),sep = "/"); 
    }
    #print(filename)
    stopifnot(file.exists(filename)); #assert
    
    
    #mean calculation:
    #1) create a vector with all the values of 'pollutant' in all the 'id' files
    #2) return the mean of this vector
    out <- tryCatch(
    {
      data = read.csv(filename);
      
      #\warning Optimize this: How to use an string as a data frame's column name
      for (next_name in colnames(data))
      { 
          if(next_name==pollutant)
          {
            aux_vector <- data[[pollutant]];
            all_valid_values<-c(all_valid_values, aux_vector[!is.na(aux_vector)])
            # all_valid_values <- c(all_valid_values, data[[pollutant]])
            # all_valid_values <- mean(data[[pollutant]], na.rm = TRUE);
                        
            #stopifnot(1==0); #assert
            break;
          }
      }      
    },

    error=function(error_cond) {
      message(paste("Error while processing the file: \"", filename, "\""));
      message(error_cond);
      # Choose a return value in case of warning
      return(NULL)
    },
    
    warning=function(warning_cond) {
      message(paste("warning while processing the file: \"", filename, "\""));
      message(warning_cond)
      # Choose a return value in case of warning
      return(NA)
    },
    finally={
    #close(filename); # read.csv can not be closed?? 
    }
    )#end of try-catch     

  }#end for
  
  ret_value <- mean(all_valid_values, na.rm = T)
    
  return (ret_value);
}#end pollutantmean


#end of file 
