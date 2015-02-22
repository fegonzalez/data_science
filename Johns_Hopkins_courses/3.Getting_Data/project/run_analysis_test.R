############################################################################
## run_analysis_test.R .- unitary test for "run_analysis.R"
##
############################################################################
## TEST STEPS
##
## pre) Assumption: this test is executed in the same directory where the file
## "run_analysis.R" is. All the files described are (or will be created) in
## that directory:
##
## pre) Check that the file "run_analysis.R" already exists.
##
## warning) If the file "run_analysis_test.OK" already exists, do not erase nor
##          change it.
##
## 1) execute this  R script ("make_test()" ...
##    ... the file "run_analysis_test.last" will be created.
##
## 2) If "run_analysis_test.OK" already exists, check that files
##    "run_analysis_test.last" and "run_analysis_test.OK" are equals; otherwise
##    check that the content of "run_analysis_test.last" is as expected, and
##    then copy that file to "run_analysis_test.OK"
##
############################################################################
##
##      File summary:
##
##      run_analysis.R: source code file to be tested.
##      run_analysis_test.R: this R script, test code.
##      run_analysis_test.OK: expected result of the execution of the test
##      run_analysis_test.last: last test result obtained.
##
############################################################################

source("run_analysis.R")

##
## begin test code
##
make_test <- function()
{
    #options(warn = 2) # Convert warnings to errors
    # INFO setting the output file or the test
    # All the messages will be printed to that file
    filename<-"run_analysis_test.last";
    file_conn<-file(filename);
    sink(file_conn, type=c("output", "message"));

    tryCatch({
        my_memdata <- struct_memdata();
        get_source_data();
        source2memory(my_memdata, FALSE);

        step1(my_memdata, FALSE);
        print("Step 1.- Checking dim(tidydata_full)");
        print(my_memdata$expected_step1_ncol_tidydata_full() ==
                  ncol(my_memdata$get_tidydata_full()));
        print(my_memdata$expected_nrow_tidydata_full() ==
                  nrow(my_memdata$get_tidydata_full()));
        print(colnames(my_memdata$get_tidydata_full()));

        steps4and2(my_memdata, FALSE);
        print("Steps 4 & 2.- std & mean data selected & named");
        print(my_memdata$expected_step2_ncol_tidydata_full() ==
                  ncol(my_memdata$get_tidydata_full()));
        print(my_memdata$expected_nrow_tidydata_full() ==
                  nrow(my_memdata$get_tidydata_full()));
        print(colnames(my_memdata$get_tidydata_full()));

        print("Steps 3.- Uses descriptive activity names to");
        tidy_colnames <- colnames(my_memdata$get_tidydata_full());
        print(head((my_memdata$get_tidydata_full()$subject_activity), 3));
        step3(my_memdata, FALSE);
        print(my_memdata$expected_step2_ncol_tidydata_full() ==
                  ncol(my_memdata$get_tidydata_full()));
        print(my_memdata$expected_nrow_tidydata_full() ==
                  nrow(my_memdata$get_tidydata_full()));

        print(head((my_memdata$get_tidydata_full()$subject_activity), 3));
        print(tail((my_memdata$get_tidydata_full()$subject_activity), 3));

    }, error=function(error_cond) {
        error_msg <- paste("Test error: <<", error_cond, ">>");
        print(error_msg);
        message(error_msg);

    }, finally={

        ## restore workspace to the standard output (messages will be normally
        ## printed)
        if(sink.number())
            {sink();}
        close(file_conn);
       #file.show(filename);
        options(warn = 0) # default value
    }#end of finally
  )#end of try-catch

} #end make_run_analysis_test


##
## end test code
##
