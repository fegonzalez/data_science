
## class = function = list of public methods 
## (See "list" definition inside)
MyTestClass<- function(x = integer()) 
{
  
  # public methods: set, get
  # MUST be included in the "list" at the end.
  # MUST be defined before the "list".
  
  set <- function(new_value) 
  {
    x <<- doble(new_value);
    y <<- new_value;
    cat("x = ", x, " ; y = ", y, "\n");
  }
  
  get <- function() 
  {
    return (x);
  }
  
  
  # private method: 
  # MUST NOT be included in "list" 
  # MUST be defined before the "list". 
  doble  <- function(x)
  {2*x;}
  
  
  # class = list of public methods
  list(set = set, 
      get = get);
 
  
} # end of the "class" MyTestClass

  
