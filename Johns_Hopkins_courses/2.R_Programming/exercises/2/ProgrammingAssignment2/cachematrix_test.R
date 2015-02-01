############################################################################
## cachematrix_test.R .- unitary test for "cachematrix.R"
##
## TEST STEPS 
##
## pre) Assumption: this test is executed form the "ProgrammingAssignment2" 
##      directory, and all the files described are (or will be created) in that 
##      directory:
##
## pre) Check that the file "cachematrix.R" already exists.
##      
## warning) If the file "cachematrix_test.OK" already exists, do not erase nor 
##          change it.
##
## 1) execute this ("cachematrix_test.R") R script ...
##    ... the file "cachematrix_test.last" will be created.
##
## 2) If "cachematrix_test.OK" already exists, check that files
##    "cachematrix_test.last" and "cachematrix_test.OK" are equals; otherwise
##    check that the content of "cachematrix_test.last" is as expected, and
##    then copy that file to "cachematrix_test.OK"
##
############################################################################
##
##      File summary:
##
##      cachematrix.R: source code file to be tested.
##      cachematrix_test.R: this R script, test code.
##      cachematrix_test.OK: expected result of the execution of the test
##      cachematrix_test.last: last test result obtained.                      
##
############################################################################

source("cachematrix.R")


##
## begin test code
##
makeCacheMatrix_test <- function()
{
 
  #options(warn = 2) # Convert warnings to errors
  
  # INFO setting the output file or the test
  # All the messages will be printed to that file
  filename<-"cachematrix_test.last";
  file_conn<-file(filename);
  sink(file_conn, type=c("output", "message"));
  
 tryCatch(
 {
   ## test 1: void matrix
   print("test 1: void matrix");
   print("my_cachematrix <- makeCacheMatrix()")
   my_cachematrix <- makeCacheMatrix();
   print("my_cachematrix$get()");
   print(my_cachematrix$get());
   print("my_cachematrix$getInverse()");
   print(my_cachematrix$getInverse());  
   cacheSolve(my_cachematrix);
   
  ## test 2: square invertible matrixdim = 3 3
  print("test 2: square invertible matrix, dim = 3 3");
  m33 <- matrix(c(8,3,4,1,5,9,6,7,2), nrow = 3, ncol = 3);
  
  print("my_cachematrix$set(m33)");
  my_cachematrix$set(m33);
  print("test 2.1.- New calculation: cacheSolve(my_cache matrix);");
  cacheSolve(my_cachematrix);
  print("my_cachematrix$get();");
  print(my_cachematrix$get());
  print("my_cachematrix$getInverse();");
  print(my_cachematrix$getInverse());
  print("test 2.2.- Already calculated: cacheSolve(my_cache matrix);");
  cacheSolve(my_cachematrix);
  #assert that makeCacheMatrix calculates the same inverse-matrix as a 
  #regular matrix
  if(identical(my_cachematrix$getInverse(), solve(m33)))
  {
    print("identical(my_cachematrix$getInverse(), solve(m33))");
  }
  else
  {
    print("FAIL: not identical (my_cachematrix$getInverse(), solve(m33))");
  }
  
  ## test 3: singular (non-invertible) matrix
  print("test 3: singular (non-invertible) matrix");
  my_cachematrix$set(matrix(c(1,0,1,0), nrow=2, ncol=2));
  print("my_cachematrix$get();");
  print(my_cachematrix$get());
  print("my_cachematrix$getInverse();");
  print(my_cachematrix$getInverse());
  print("Singular matrix: cacheSolve(my_cache matrix); (set to NULL");
  cacheSolve(my_cachematrix);
  print("Inverse value set to NULL.");
  print("my_cachematrix$get();");
  print(my_cachematrix$get());
  print("my_cachematrix$getInverse();");
  print(my_cachematrix$getInverse());
  
  
  ## test 4: non-square (non-invertible) matrix, dim = 1 3
  print("test 4: non-square (non-invertible) matrix, dim = 1 3");
  m13 <- matrix(c(8,3,4), nrow = 1, ncol = 3);
  print("my_cachematrix$set(matrix(c(8,3,4), nrow = 1, ncol = 3))");
  my_cachematrix$set(m13);
  print("test 4.1.- New calculation: cacheSolve(my_cache matrix);");
  cacheSolve(my_cachematrix);
  print("my_cachematrix$get();");
  print(my_cachematrix$get());
  print("my_cachematrix$getInverse();");
  print(my_cachematrix$getInverse());
  print("test 4.2.- Already calculated: cacheSolve(my_cache matrix);");
  cacheSolve(my_cachematrix);
  
  
  print("test 5: invalid data (not a matrix)");
  tryCatch({my_cachematrix$set(c(1,2));}, error = function(e) e);
  print("... so the stored matrix didn't change.");
  print("my_cachematrix$get()");
  print(my_cachematrix$get());
  print("my_cachematrix$getInverse()");
  print(my_cachematrix$getInverse());  
  print("Singular matrix: cacheSolve(my_cache matrix)");
  cacheSolve(c(1,2));
  
  
}, ##end of trycatch

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

} #end makeCacheMatrix_test


##
## end test code
##
