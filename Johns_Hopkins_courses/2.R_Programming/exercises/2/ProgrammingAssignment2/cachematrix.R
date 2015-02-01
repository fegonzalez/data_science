## Put comments here that give an overall description of what your
## functions do


## Brief description:

# makeCacheMatrix: given a regular 'matrix' object, creates a special "matrix"
# object that cache its inverse for optimization purpose.
#
# Warning: The inverse matrix value is reset (NULL) after any matrix update
# (construction & set), and must be calculated & set using the cacheSolve()
# function.


## Detailed description:

# makeCacheMatrix$get: returns the matrix value.
 
# makeCacheMatrix$set: change the value of the matrix after its construction
# and initialise (NULL) the value of the inverse matrix.
 
# makeCacheMatrix$getInverse: returns the inverse of the matrix if the inverse
# has already been calculated; returns NULL, otherwise.
  
# makeCacheMatrix$setInverse: set the stored cache-value of the inverse of the
# matrix. 
# Warning: only substitutes the stored value with the received one, the actual
# calculation will be made in cacheSolve() function).

# cacheSolve: given a valid 'makeCacheMatrix' object,
#
# return the stored value of the inverse of the matrix, if that value already
# exists;
#
# set to NULL the value of the inverse of the matrix, if the received matrix is
# not a squared matrix, and return NULL.
#
# calculate the inverse of the matrix (solve) and set that value to the
# received matrix, and return the new value.
#
# Warning: is assumed that the matrix supplied is always invertible, otherwise
# an assertion will raise stopping the execution.


## Usage
# 
# makeCacheMatrix(m = matrix()) 
# get()
# set(m)
# getInverse()
# cacheSolve(cm)


## Arguments
# 
# m: a valid 'matrix' object. (a makeCacheMatrix is not valid)
# cm: a valid 'makeCacheMatrix' object.

## Examples
# 
## void matrix
# my_cm_33 <- makeCacheMatrix();
# my_makevector_x$get();
# my_makevector_x$getInverse();
# 
## square invertible matrix
# m <- matrix(c(8,3,4,1,5,9,6,7,2), nrow = 3, ncol = 3);
# my_makevector_x$set(m)
# cachesolve(my_cm_33);
# my_makevector_x$get();
# my_makevector_x$getInverse();


##===========================================================================


## Write a short comment describing this function

# makeCacheMatrix: given a regular 'matrix' object, creates a special "matrix"
# object that cache its inverse for optimization purpose.
#
# Warning: The inverse matrix value is reset (NULL) after any matrix update
# (construction & set), and must be calculated & set using the cacheSolve()
# function.

makeCacheMatrix <- function(x = matrix()) 
{
  ## stopifnot(is.matrix(x));
  
  if(!is.matrix(x)){
    stop("Bad input, not a matrix.");
  }
  
  the_inverse <- NULL; #default value
  
  set <- function(new_value) 
  {
    if(!is.matrix(new_value))
    {
      stop("Bad input, not a matrix.");
    }
    x <<- new_value;
    the_inverse <<- NULL; #default value
  }

  get <- function() 
  {
    return (x);
  }
  
  setInverse <- function(new_value) 
  {
    the_inverse <<- new_value;
  }
  
  getInverse <- function() 
  {
    return (the_inverse);
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse);

}


##---------------------------------------------------------------------------

## Write a short comment describing this function

# cacheSolve: given a valid 'makeCacheMatrix' object,
#
# case 1) inverse already exists: return the stored value of the inverse of the
# matrix without any calculation;
#
# case 2) not squared matrix: set to NULL the value of the inverse of the
# matrix and return NULL;
#
# case 3) invertible matrix: calculate the inverse of the received matrix 
# (solve), set that value to the received matrix, and return the new value.
#
# error case) If the matrix is singular (non-invertible): capture & show the
# error message, set to NULL the value of the inverse of the matrix, and return
# NULL;
#
cacheSolve <- function(x, ...) 
{
 data <- x$get();
 
 #data validation
 if(!is.matrix(data))
 {
   stop("Bad input, not a matrix.");
 }
 
 
 inverse_value <- x$getInverse(); 

 # case 1)
 if(!is.null(inverse_value))
 {
  print("using cached value.");
  ## Return a matrix that is the inverse of 'x'
  return (inverse_value);
 }

 ##browser();

 # case 2)
 squared_matrix <- (ncol(data) == nrow(data));
 if(!squared_matrix)
 {
   print("non-squared matrix");
   x$setInverse(NULL);
   return (NULL);
 }
 
 options(warn = 2) # Convert warnings to errors
 tryCatch(
   # case 3)
  {
    print("invertible matrix been solved");
    x$setInverse(solve(data, ...));
  },
   #error case)
  error=function(error_cond) 
  {
    error_msg <- paste("cacheSolve error: <<", error_cond, ">>");
    error_msg <- paste(error_msg, "setting the inverse to NULL");
    print(error_msg);
    message(error_msg);
    x$setInverse(NULL);
  },
  finally=
  {
    options(warn = 0) # default value
    
    ## Return a matrix that is the inverse of 'x'
    return(x$getInverse());
  }
 )#trycatch
 
} ## end of cacheSolve
