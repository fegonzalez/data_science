##############################################################################
## unit-test script
##############################################################################
##
## test "complete.R": execute script "complete_test.R" : The file
## "complete_test.last" is created.
##
##############################################################################

source("complete.R")

filename<-"complete_test.last";
file_conn<-file(filename);
sink(file_conn);

## complete("specdata", 1)
result <- complete("specdata", 1)
#out_buffer <- c("complete(\"specdata\", 1)");
print("complete(\"specdata\", 1)");
print(result);
##   id nobs
## 1  1  117


## complete("specdata", 3)
print("complete(\"specdata\", 3)");
print(complete("specdata", 3));
##   id nobs
## 1  3  243


## complete("specdata", c(2, 4, 8, 10, 12))
print("complete(\"specdata\", c(2, 4, 8, 10, 12))");
print(complete("specdata", c(2, 4, 8, 10, 12)));
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192≥
## 4 10  148
## 5 12   96


## complete("specdata", 30:25)
print("complete(\"specdata\", 30:25)");
print(complete("specdata", 30:25))
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463



## restore workspace
if(sink.number())
{sink();}
close(file_conn);

#file.show(filename)
