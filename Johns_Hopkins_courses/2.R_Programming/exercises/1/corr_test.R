source("corr.R")
source("complete.R")

#unit test output
filename<-"corr_test.last";
file_conn<-file(filename);
sink(file_conn);

##
## test steps
##

print("Test.- corr(\"specdata\", 150)");
cr <- corr("specdata", 150);
test_result <- head(cr);
print(test_result);
## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
test_result <- summary(cr)
print(test_result);
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630


print("Test.- corr(\"specdata\", 400)");
cr <- corr("specdata", 400)
test_result <- head(cr);
print(test_result);
## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
test_result <- summary(cr)
print(test_result);
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630


print("Test.- corr(\"specdata\", 5000)");
cr <- corr("specdata", 5000)
test_result <- summary(cr)
print(test_result);
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
test_result <- length(cr)
print(test_result);
## [1] 0


print("Test.- corr(\"specdata\")");
cr <- corr("specdata")
test_result <- summary(cr)
print(test_result);
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
test_result <- length(cr)
print(test_result);
## [1] 323


## restore workspace
if(sink.number())
{sink();}
close(file_conn);
