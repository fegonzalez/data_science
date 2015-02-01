############################################################################
## best_test.R .- unitary test for "best.R"
##
## TEST STEPS 
##
## In R: source best_test.R
##
##       make_best_test()
##       ... the file "best_test.last" will be created.
##
##       If "best_test.OK" already exists, check that files "best_test.last"
##       and "best_test.OK" are equals; otherwise check that the content of
##       "best_test.last" is as expected, and then copy that file to
##       "best_test.OK"
##
############################################################################
##
##      File summary:
##
##      best.R: source code file to be tested.
##      best_test.R: this R script, test code.
##      best_test.OK: expected result of the execution of the test
##      best_test.last: last test result obtained.                      
##
############################################################################

source("best.R")
source("rankhospital.R")
source("rankall.R")

##
## begin test code
##
make_best_test <- function()
{
 
  #options(warn = 2) # Convert warnings to errors
  
  # INFO setting the output file or the test
  # All the messages will be printed to that file
  filename<-"best_test.last";
  file_conn<-file(filename);
  sink(file_conn, type=c("output", "message"));
  
 tryCatch(
 {
  ## test 1: Finding the best hospital in a state
  print("## test 1: Finding the best hospital in a state");
  print("best(\"TX\", \"heart attack\")")
  print(best("TX", "heart attack"));
#   > system.time(expr = best("TX", "heart attack"))
#   user  system elapsed 
#   0.465   0.004   0.470 
  #[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
  
  print("best(\"TX\", \"heart failure");
  print(best("TX", "heart failure"));
  #[1] "FORT DUNCAN MEDICAL CENTER"
  print("best(\"MD\", \"heart attack");
  print(best("MD", "heart attack"));
  #[1] "JOHNS HOPKINS HOSPITAL, THE"
  print("best(\"MD\", \"pneumonia");
  print(best("MD", "pneumonia"));
  #[1] "GREATER BALTIMORE MEDICAL CENTER"

  tryCatch(
  {
    print("best(\"BB\", \"heart attack");
    print(best("BB", "heart attack"));
    #Error in best("BB", "heart attack") : invalid state
  }, 
  error = function(e) { print(e); } ); 
  
  tryCatch(
  { 
    print("best(\"NY\", \"hert attack");
    print(best("NY", "hert attack"));
    #Error in best("NY", "hert attack") : invalid outcome
  },
  error = function(e) {print(e);} );
   
  
  ## test 2: Ranking hospitals by outcome in a state
  print("## test 2: Ranking hospitals by outcome in a state");
  print("rankhospital(\"TX\", \"heart failure\", 4)");
  print(rankhospital("TX", "heart failure", 4));
  #[1] "DETAR HOSPITAL NAVARRO"
  print("rankhospital(\"MD\", \"heart attack\", \"worst\")");
  print(rankhospital("MD", "heart attack", "worst"));
  #[1] "HARFORD MEMORIAL HOSPITAL"
  print("rankhospital(\"TX\", \"heart failure\", best)");
  print(rankhospital("TX", "heart failure", "best"));
  #[1] "FORT DUNCAN MEDICAL CENTER"
  print("rankhospital(\"TX\", \"heart failure\", best)");
  print(rankhospital("TX", "heart failure")); #default value
  #[1] "FORT DUNCAN MEDICAL CENTER"
  print("rankhospital(\"MN\", \"heart attack\", 5000)");
  print(rankhospital("MN", "heart attack", 5000));
  #[1] NA
  tryCatch(
  {
    print("rankhospital(\"mN\", \"heart attack\", 5000)");
    print(rankhospital("mN", "heart attack", 5000));
    #Error in rankhospital("mN", "heart attack") : invalid state
  }, 
  error = function(e) { print(e); } ); 
  tryCatch(
  { 
    print("rankhospital(\"MN\", \"hert attack\", 5000)");
    print(rankhospital("MN", "hert attack", 5000));
    #Error in best("NY", "hert attack") : invalid outcome
  },
  error = function(e) {print(e);} );


  ## test 3: Ranking hospitals in all states
  print("tail(rankall(\"pneumonia\", \"worst\"), 3))");
  print(tail(rankall("pneumonia", "worst"), 3));
# hospital state
# WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
# WV PLATEAU MEDICAL CENTER WV
# WY NORTH BIG HORN HOSPITAL DISTRICT WY

  print("tail(rankall(\"heart failure\"), 10))");
  print(tail(rankall("heart failure"), 10));
# hospital state
# TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
# TX FORT DUNCAN MEDICAL CENTER TX
# UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
# VA SENTARA POTOMAC HOSPITAL VA
# VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
# VT SPRINGFIELD HOSPITAL VT
# WA HARBORVIEW MEDICAL CENTER WA
# WI AURORA ST LUKES MEDICAL CENTER WI
# WV FAIRMONT GENERAL HOSPITAL WV
# WY CHEYENNE VA MEDICAL CENTER WY

  print("head(rankall(\"heart attack\", 20), 10))");
  print(head(rankall("heart attack", 20), 10));
#         hospital                              state
#   AK   <NA>                                     AK
#   AL   D W MCMILLAN MEMORIAL HOSPITAL           AL
#   AR   ARKANSAS METHODIST MEDICAL CENTER        AR
#   AZ   JOHN C LINCOLN DEER VALLEY HOSPITAL      AZ
#   CA   SHERMAN OAKS HOSPITAL                    CA
#   CO   SKY RIDGE MEDICAL CENTER                 CO
#   CT   MIDSTATE MEDICAL CENTER                  CT
#   DC   <NA>                                     DC
#   DE   <NA>                                     DE
#   FL   SOUTH FLORIDA BAPTIST HOSPITAL           FL


  
}, ##end of the trycatch of make_best_test()

error=function(error_cond) 
{
  error_msg <- paste("Test error: <<", error_cond, ">>");
  print(error_msg);
  message(error_msg);
},

finally=
{  
  ## restore workspace to the standard output (messages will be normally
  ## printed)
  if(sink.number())
  {sink();}
  close(file_conn);
  options(warn = 0) # default value
  
  file.show(filename);
}

  )#end of try-catch 

} #end make_best_test


##
## end test code
##
