##############################################################################
## unit-test script
##############################################################################
##
## test "pollutantmean.R": execute script "pollutantmean_test.R" : The file
## "pollutantmean_test.last" is created.
##
## store expected results: once we get the expected results, execute the script
## "make_success.sh":
##       cp pollutantmean_test.last pollutantmean_test.OK
##
## verify expected results after any code change: execute script "make_diff.sh":
##       diff pollutantmean_test.last pollutantmean_test.OK
##############################################################################

source("pollutantmean.R")

filename<-"pollutantmean_test.last";
file_conn<-file(filename)

result <- pollutantmean("specdata", "sulfate", 1:10);
out_buffer <- c("pollutantmean(\"specdata\", \"sulfate\", 1:10)", 
                sprintf("%.3f", result))
##[1] 4.064128
result <- pollutantmean("specdata", "nitrate", 70:72);
out_buffer <- c(out_buffer, "pollutantmean(\"specdata\", \"nitrate\", 70:72)", 
                sprintf("%.3f", result))
##[1] 1.706
result <- pollutantmean("specdata", "nitrate", 23);
out_buffer <- c(out_buffer, "pollutantmean(\"specdata\", \"nitrate\", 23)", 
                sprintf("%.3f", result))
##[1] 1.281

print(out_buffer);
writeLines(out_buffer, file_conn, sep="\n");
close(file_conn);
#file.show(filename)
