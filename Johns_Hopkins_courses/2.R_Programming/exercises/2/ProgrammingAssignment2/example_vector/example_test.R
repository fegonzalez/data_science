############################################################################
## example_test.R .- unitary test for "example.R"
##
## TEST STEPS 
##
## pre) Assumption: this test is executed form the "ProgrammingAssignment2" 
##      directory, and all the files described are (or will be created) in that 
##      directory:
##
## pre) Check that the file "example_test.OK" already exists.
##
## pre) Check that the file "example.R" already exists.
##      
## 1) execute this ("example_test.R") R script 
##    The file "example_test.last" will be created.
##
## 2) If "example_test.OK" already exists, check that files "example_test.last"
##    and "example_test.OK" are equals;
##    otherwise check that the content of "example_test.last" is as expected,
##    and then copy that file to "example_test.OK"
##
############################################################################
##
##      File summary:
##
##      example.R: source code file to be tested.
##      example_test.R: this R script, test code.
##      example_test.OK: expected result of the execution of the test
##      example_test.last: last test result obtained.                      
##
############################################################################

#source("example.R")
source("test_r.R")

##
## begin test code
##
makevector_test <- function()
{
  options(warn = 2) # Convert warnings to errors
  
  # INFO setting the output file or the test
  # All the messages will be printed to that file
  filename<-"example_test.last";
  file_conn<-file(filename);
  sink(file_conn, type=c("output", "message"));
  
  
  
  tryCatch(
{
## create an object of the type (class) makeVector
print("my_makevector_x  <- makeVector(1:5)")
my_makevector_x  <- makeVector(1:5)  

## test "get()" method of the object
print("my_makevector_x$get()")
print(my_makevector_x$get())
# [1] 1 2 3 4 5

## test "getmean()" method of the object (NULL expected because it wasn't set
## yet)
print("my_makevector_x$getmean()")
print(my_makevector_x$getmean())
# NULL

## test "setmean()" method
## WARNING: the way the code is, any number passed will be STORED as the mean,
## so, the proper way to set the mean is by calling cachemean() instead.
print("my_makevector_x$setmean(4)");
my_makevector_x$setmean(4)

## test getmean() method 
print("my_makevector_x$getmean()")
print(my_makevector_x$getmean())
# [1] 4

## test cachemean() function: 
## INFO: mean was already set -> returning stored mean instead of calculating
## it.
print("cachemean(my_makevector_x)")
print(cachemean(my_makevector_x))
# getting cached data
# [1] 4

cachemean(c(1,2))

## test the mean calculation via cachemean() function, proper way to set the
## mean
print("my_makevector_x$set(1:4)");
my_makevector_x$set(1:4)
print("my_makevector_x$get()")
print(my_makevector_x$get())
# [1] 1 2 3 4
print("cachemean(my_makevector_x)")
print(cachemean(my_makevector_x))
# [1] 2.5
print("my_makevector_x$getmean()")
print(my_makevector_x$getmean())
# [1] 2.5
},

error=function(error_cond) 
{
  error_msg <- paste("Test error: <<", error_cond, ">>");
  print(error_msg);
  message(error_msg);
},

finally=
{
 
  ## restore workspace to the standard output (messages will be normally printed)
  if(sink.number())
    {sink();}
  close(file_conn);
  #file.show(filename);
  options(warn = 0) # default value
}

)#end of try-catch

} #end makevector_test


##
## end test code
##
