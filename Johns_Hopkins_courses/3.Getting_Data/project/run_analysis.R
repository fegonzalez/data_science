################################################################################
##
## $ADD_TO_DOC
##
##
## INFO.- Project execution
##
##
## System Requirements:
##
## - R
## - Active Internet connection (at least for the first execution)
##
##
## Note.- Let's call "RWW" the directory where the input scrip stands.
##
## Input files:
##
##     RWW/run_analysis.R
##
## Execution steps:
##
##     1) Open a R session.
##     2) Set the working directory to the one where "run_analysis.R" stands.
##     3) In the R console type:
##
##                source("run_analysis.R")
##                run_analysis()
##
## Output files:
##
##     RWW/project_solution.txt
##
##     View: to view the file with a proper format, use an "excel-type"
##     application. Open the file selecting blank-space as separator.
##
## Additional notes.-
##
##     First execution:
##
##     For the first execution of the project's script, or when the source data
##     is not found in the expected place (see bellow), the source data is
##     automatically downloaded and installed in the system.
##
##     a) RWW/data directory is created.
##     b) Source file downloaded: RWW/data/projectfiles_dataset.zip
##     c) source data unzipped:   RWW/data/UCI HAR Dataset/
##
##     Therefore, for this first time, the total execution time will include
##     these operations.
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


################################################################################
## You should create one R script called run_analysis.R that does the following.
##
## 1.- Merges the training and the test sets to create one data set.
##
## 2.- Extracts only the measurements on the mean and standard deviation
##     (std) for each measurement.
##
## 3.- Uses descriptive activity names to name the activities in the data set
##
## 4.- Appropriately labels the data set with descriptive variable names.
##
## 5.- From the data set in step 4, creates a second, independent tidy data set
## with the average of each variable for each activity and each subject.
##
##      My Hint:                activity_1   ... activity_n
##
##                subject_1     avg(value)  ...
##                     ...
##                subject_30    avg(value)  ...
##
################################################################################



## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## $ADD_TO_DOC
##
## NOTICE.- Source data path.
##
## Given "RWW" as the current R working directory, upon executing
## 'get_source_data()' function, all the required source data is downloaded
## from the Internet (projectfiles_dataset.zip), and then unzipped to the
## directory "RWW/data/UCI HAR Dataset"
##
## From now, all the mentions to files will be referred as relative to
## "RWW/data/UCI HAR Dataset".- (e.g. "test/subject_test.txt" refers to
## "RWW/data/UCI HAR Dataset/test/subject_test.txt")
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## $ADD_TO_DOC
##
## 1.- List of source files used to create the tidy data.
##
## "train/subject_train.txt" : 7352 observations; 1 variables
## "train/X_train.txt"       : 7352 observations; 561 variables
## "train/y_train.txt"       : 7352 observations; 1 variables
##
## "test/subject_test.txt"   : 2947 observations; 1 variables
## "test/X_test.txt"         : 2947 observations; 561 variables
## "test/y_test.txt"         : 2947 observations; 1 variables
##
## "features.txt"            : 561 observations; 2 vars
##
## "activity_labels.txt"     : 6 observations; 2 vars
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## $ADD_TO_DOC
##
## 2.- Process to transform the source data into the tidy data.
##
## In this point is explained which part of the final tidy data-set comes from
## which part/file of the source data files.
##
##
## The next diagram briefly shows the process explained bellow:
##
## From source data files ...
##
##                           (  features.txt   )
## (train/subject_train.txt) (train/X_train.txt) (train/y_train.txt)
## (test/subject_test.txt  ) (test/X_test.txt  ) (test/y_test.txt)
##
##
## ... to tidy data (data.frame)
##
## ("subject_id") ( feature_names ) ("subject_activity")
##           (a1) (  ... a2 ...   ) (a3)
##           (b1) (  ... b2 ...   ) (b3)
##
##
## 2.1) Tidy dataset from up to bottom (rows = observations)
##
## Section a) Training observations: 7352 observations ; rows 1-7352
## Section b) Test observations:     2947 observations ; rows 7353-10299
##
##
## 2.2) Tidy dataset from left to right (columns = variables)
##
## Section 1) subject variable:
##
## colnumber: 1
##
## colname: "subject_id"
##
## observations: the first 7352 observations, let's call them "section a"
## observations, are an ordered copy (R's rbind) of the observations in
## "train/subject_train.txt" file; the next 2947 observations, let's call them
## "section b" observations, are an ordered copy of the observations in
## "test/subject_test.txt" file.
##
## Source files: "train/subject_train.txt" , "test/subject_test.txt".
##
## Section 2) X-observations
##
## colnumber: 2 to 562
##
## colname: each column name (from left to right), corresponds to an
## observation of the second variable (from up to bottom), in the source file
## "features.txt". (e.g. column-2 name is "tBodyAcc-mean()-X")
##
## observations: "section a" observations are an ordered copy of the
## observations in "train/X_train.txt" file; "section b" observations are an
## ordered copy of the observations in "test/X_test.txt" file.
##
## Source files: "features.txt", "train/X_train.txt", "test/X_test.txt".
##
## Section 3) Y-observations
##
## colnumber: 563
##
## colname: "subject_activity"
##
## observations: "section a" observations are an ordered copy of the
## observations in "train/y_train.txt" file; "section b" observations are an
## ordered copy of the observations in "test/y_test.txt" file.
## Finally the values of theses observations are changed from numbers (1-6), to
## the descriptive labels following "activity_labels.txt"
## Source files: "train/Y_train.txt", "test/Y_test., "activity_labels.txt"
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 require(data.table);
 require(reshape2);   ## melt & dcast



## -----------------------------------------------------------------------------

run_analysis <- function(DEBUG_MODE = FALSE)
{
    the_memdata <- struct_memdata();

    if(DEBUG_MODE)
    {
        print(system.time(retval <- solve(the_memdata, DEBUG_MODE)));
    }
    else
    {
        retval <- solve(the_memdata,DEBUG_MODE);
    }
}

## -----------------------------------------------------------------------------

solve <- function(my_memdata, DEBUG_MODE=FALSE)
{
    get_source_data();
    source2memory(my_memdata, FALSE);
    mem2tidy(my_memdata, FALSE);
    step5(my_memdata, FALSE);
    write_solution(my_memdata, DEBUG_MODE);
    write_codebook(my_memdata, DEBUG_MODE);
}

## -----------------------------------------------------------------------------

################################################################################
## \function write_solution(my_memdata, DEBUG_MODE)
## \brief write the solution data produced at step5() into a file according to
## the project instruction:
##
## output file: "project_solution.txt" in the same directory that run_analysis.R
##
################################################################################
## $ADD_TO_DOC
##
## NOTICE.- Output file:
##
## Name: "project_solution.txt"
##
## Directory: in the same directory that run_analysis.R.
##
## View: to view the file with a proper format, use an "excel-type"
## application. Open the file selecting blank-space as separator.
##
## Format: one header row, followed by 180 observations; where var1 .. var68
## are the names of each measure "avg(tBodyAcc-mean()-X), ..., "
## fBodyBodyGyroJerkMag-std()). It will look like this:
##
## subject_id | subject_activity | avg(var1) | ...  | avg(var68)
## -------------------------------------------------------------
##     1      | LAYING           | value     | ...  | value
##     1      | SITTING | value  | ...       | value
##     ...
##     1      | WALKING_UPSTAIRS | value     | ...  | value
##     2      | LAYING           | value     | ...  | value
##     ...
##     6      | WALKING_UPSTAIRS | value     | ...  | value
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_solution <- function(my_memdata, DEBUG_MODE)
{
    outputfile <- "project_solution.txt";
    write.table(my_memdata$get_tidydata_avg(),
                outputfile,
                sep=" ",
                row.name=FALSE);

    if(DEBUG_MODE)
    {
        checksolution <- fread(outputfile,
                               sep=" ",
                               header=TRUE);

        stopifnot(identical(ncol(checksolution),
                            ncol(my_memdata$get_tidydata_avg())));

        stopifnot(identical(nrow(checksolution),
                            nrow(my_memdata$get_tidydata_avg())));

        stopifnot(identical(colnames(checksolution),
                            colnames(my_memdata$get_tidydata_avg())));
        print("head(checksolution), 1");
        print(head(checksolution), 1);
        print("head(my_memdata$get_tidydata_avg()), 1)");
        print(head(my_memdata$get_tidydata_avg()), 1);
        ## stopifnot(identical(checksolution, my_memdata$get_tidydata_avg()));
    }
}

## -----------------------------------------------------------------------------

##\input: original source data stored in memory:
## my_memdata$data_subjecttrain <- data.table();
## my_memdata$data_ytrain <- data.table();
## my_memdata$data_xtrain <- data.table();
## my_memdata$data_subjecttest <- data.table();
## my_memdata$data_ytest <- data.table();
## my_memdata$data_xtest <- data.table();
## my_memdata$data_features <- data.table();
## my_memdata$data_labels <- data.table();
##
##\output: my_memdata$tidydata_full (data.table): tidy data-set in this form:
##
## my_memdata$tidydata_full =
## ("subject_id"    )(col names = data_features)("subject_activity")
## (data_subjecttrain)(data_xtrain              )(data_ytrain       )
## (data_subjecttest )(data_xtest               )(data_ytest        )
##
## Result of merging the input data tidy according to the steps below:
##
## 1.- Merges the training and the test sets to create one data set.
##
## 2.- Extracts only the measurements on the mean and standard deviation
##     (std) for each measurement.
##
##     My Hint: column names = row values from "features.txt"
##          extract only column names with the sub-string "mean" or "std" in it.
##
## 3.- Uses descriptive activity names to name the activities in the data set
##
## 4.- Appropriately labels the data set with descriptive variable names.

mem2tidy <- function(my_memdata, DEBUG_MODE=FALSE)
{
    step1(my_memdata, FALSE);
    steps4and2(my_memdata, DEBUG_MODE); #join 4 & 2: implementation decision
    step3(my_memdata, DEBUG_MODE);
}


##------------------------------------------------------------------------------

## 1.- Merges the training and the test sets to create one data set.
step1 <- function(my_memdata, DEBUG_MODE=FALSE)
{

    ## bind rows
    aux_dfsubject <- rbind(my_memdata$get_data_subjecttrain(),
                           my_memdata$get_data_subjecttest());
    aux_dfx <- rbind(my_memdata$get_data_xtrain(), my_memdata$get_data_xtest());
    aux_dfy <- rbind(my_memdata$get_data_ytrain(), my_memdata$get_data_ytest());

    ## bind cols
    tidy_input_dataset <- cbind(aux_dfsubject, aux_dfx, aux_dfy);

    ## store tidy data
    my_memdata$set_tidydata_full(tidy_input_dataset);

    stopifnot(my_memdata$expected_step1_ncol_tidydata_full() ==
              ncol(my_memdata$get_tidydata_full()));
    stopifnot(my_memdata$expected_nrow_tidydata_full() ==
                  nrow(my_memdata$get_tidydata_full()));
}

##------------------------------------------------------------------------------

## \function steps4and2(my_memdata, DEBUG_MODE=FALSE)
## \brief Execute step 4 & 2 (in this order) of the project instructions.
## \info The combination of these two steps is an implementation decision.
##
## 4.- Appropriately labels the data set with descriptive variable names.
## my_memdata$tidydata_full =
## ("subject_id"    )(col names = data_features)("subject_activity")
## (data_subjecttrain)(data_xtrain              )(data_ytrain       )
## (data_subjecttest )(data_xtest               )(data_ytest        )
##
## 2.- Extracts only the measurements on the mean and standard deviation
##     (std) for each measurement. (see NOTICE bellow)
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## $ADD_TO_DOC
##
## NOTICE.- step 2
##
## According to point 2 at the instructions of the project, "Extracts only
## the measurements on the mean and standard deviation for each
## measurement.", and to the data-set's document features_info.txt,
##    "These signals were used to estimate variables of the feature vector for
##     each pattern:
##     tBodyAcc-XYZ
##     ...
##     fBodyGyroJerkMag",
## it has been considered that the measurements to include in the project are
## only "mean()" and "std()" for these signals, that gives a total of 66
## variables, 33 for each mean and std.
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

steps4and2 <- function(my_memdata, DEBUG_MODE=FALSE)
{
    ## 4
    subject_names <- c("subject_id");
    features_names <- (as.data.frame(my_memdata$get_data_features())[, 2]);
    ## features_names <- my_memdata$get_data_features()$V2;  # valid too
    activity_names <- c("subject_activity");
    new_colnames <- c(subject_names, features_names, activity_names);


    ## WARNING class(tidy_input_dataset) must be data.frame  to
    ## tidy_input_dataset[wantedvars] work removing false columns.
    ## That operation does not works over data.table().

    tidy_input_dataset <- as.data.frame(my_memdata$get_tidydata_full());
    setnames(tidy_input_dataset, new_colnames);

    ## 2
    MEAN_STD_PATTERN <- "mean\\()|std\\()"
    ## MEAN_STD_PATTERN <- "mean\\(\\)|std\\(\\)"
    wantedvars <- grepl(MEAN_STD_PATTERN,
                        x=colnames(tidy_input_dataset),
                        ignore.case=FALSE);

    #preserve first & last columns
    wantedvars[1] <- TRUE;
    wantedvars[length(wantedvars)] <- TRUE;

    ## finally: store the changes made

    if(DEBUG_MODE)
    {
        print(ncol(tidy_input_dataset));
        print(nrow(tidy_input_dataset));
        print(ncol(my_memdata$get_tidydata_full()));
        print(nrow(my_memdata$get_tidydata_full()));
    }

    stopifnot(my_memdata$expected_step1_ncol_tidydata_full() ==
                  ncol(my_memdata$get_tidydata_full()));
    stopifnot(my_memdata$expected_nrow_tidydata_full() ==
                  nrow(my_memdata$get_tidydata_full()));

    tidy_input_dataset <- tidy_input_dataset[wantedvars];
    if(DEBUG_MODE)
    {
        print(ncol(tidy_input_dataset));
        print(nrow(tidy_input_dataset));
        print(colnames(tidy_input_dataset));
    }


    my_memdata$set_tidydata_full(as.data.table(tidy_input_dataset));

    stopifnot(my_memdata$expected_step2_ncol_tidydata_full() ==
              ncol(my_memdata$get_tidydata_full()));
    stopifnot(my_memdata$expected_nrow_tidydata_full() ==
                  nrow(my_memdata$get_tidydata_full()));

    if(DEBUG_MODE)
    {
        print(ncol(my_memdata$get_tidydata_full()));
        print(nrow(my_memdata$get_tidydata_full()));
        print(colnames(my_memdata$get_tidydata_full()));
    }
}


################################################################################
##\function step3((my_memdata, DEBUG_MODE)
##\brief Execute step 3 of the project: 3.- Uses descriptive activity names to
## name the activities in the data set
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## $ADD_TO_DOC
##
## Giving the descriptive values (WALKING, ...) read from the source file
## "activity_labels.txt", replace in the column $subject_activity of the tidy
## data set, each numerical value (1, 2, .., 6) with the correspondence
## descriptive value in that file.
##
## e.g. Given "activity_labels.txt"equal to
## 1 WALKING
## 2 WALKING_UPSTAIRS
## 3 WALKING_DOWNSTAIRS
## 4 SITTING
##
## e.g. Given tidydata_full$subject_activity equal to
## 1
## 1
## 2
## 3
## 3
## after execute step3(), tidydata_full$subject_activity will be equal to
## WALKING
## WALKING
## WALKING_UPSTAIRS
## WALKING_DOWNSTAIRS
## WALKING_DOWNSTAIRS
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step3 <- function(my_memdata, DEBUG_MODE=FALSE)
{
    dt <- my_memdata$get_tidydata_full();
    for(loopi in 1:nrow(my_memdata$get_data_labels()))
    {
        dt$subject_activity[dt$subject_activity ==
                                my_memdata$get_data_labels()$V1[loopi]] <-
                                    my_memdata$get_data_labels()$V2[loopi];

        my_memdata$set_tidydata_full(dt);
    }
}


################################################################################
## step5() From the data set in step 4, creates a second, independent tidy data
## set with the average of each variable for each activity and each subject.
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## $ADD_TO_DOC
##
## NOTICE.- Final data format:
##
## The final data set will be a table like the following, with 180 observations;
## where var1 .. var68 are the names of each measure (tBodyAcc-mean()-X, ...)
##
## subject_id | subject_activity | avg(var1) | ...  | avg(var68)
## -------------------------------------------------------------
##     1      | LAYING           | value     | ...  | value
##     1      | SITTING | value  | ...       | value
##     ...
##     1      | WALKING_UPSTAIRS | value     | ...  | value
##     2      | LAYING           | value     | ...  | value
##     ...
##     6      | WALKING_UPSTAIRS | value     | ...  | value
##
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

step5 <- function(my_memdata, DEBUG_MODE=FALSE)
{
    dt <- my_memdata$get_tidydata_full();
    dt <- melt(dt, id=c("subject_id", "subject_activity"));

    if(DEBUG_MODE){
        system.time({
            avgdt <- dcast.data.table(dt,
                                      subject_id+subject_activity ~variable,
                                      mean);
        });}
    else{
        avgdt <- dcast.data.table(dt,
                                  subject_id+subject_activity ~variable,
                                  mean);
        }


    #\warning Make descriptive colnames: avg(variable_name)
    old_colnames <- colnames(avgdt);
    new_colnames <- colnames(avgdt);
    new_colnames <- gsub("^*", "avg(", new_colnames);
    new_colnames <- gsub("*$", ")", new_colnames);
    new_colnames[1:2] <- old_colnames[1:2];
    setnames(avgdt, new_colnames);


    stopifnot(my_memdata$expected_ncol_tidydata_avg() ==
              ncol(avgdt));
    stopifnot(my_memdata$expected_nrow_tidydata_avg() ==
              nrow(avgdt));

    ## finally: store the changes made
    my_memdata$set_tidydata_avg(avgdt);
    stopifnot(my_memdata$expected_ncol_tidydata_avg() ==
              ncol(my_memdata$get_tidydata_avg()));
    stopifnot(my_memdata$expected_nrow_tidydata_avg() ==
              nrow(my_memdata$get_tidydata_avg()));

    if(DEBUG_MODE)
    {
        print(head(my_memdata$get_tidydata_avg()));
        print(tail(my_memdata$get_tidydata_avg()));
        print(colnames(my_memdata$get_tidydata_avg()));
        print(ncol(my_memdata$get_tidydata_avg()));
        print(nrow(my_memdata$get_tidydata_avg()));
    }

}


## -----------------------------------------------------------------------------

## \function source2memory()
## Read the source files and stores its content into memory at "my_memdata"
source2memory <- function(my_memdata, DEBUG_MODE=FALSE)
{
    ## input validation
    source_dir <- "./data/UCI HAR Dataset/";
    source_subjecttrain <- paste(source_dir, "train/subject_train.txt", sep="");
    source_xtrain <- paste(source_dir, "train/X_train.txt", sep="");
    source_ytrain <- paste(source_dir, "train/Y_train.txt", sep="");
    source_subjecttest <- paste(source_dir, "test/subject_test.txt", sep="");
    source_xtest <- paste(source_dir, "test/X_test.txt", sep="");
    source_ytest <- paste(source_dir, "test/Y_test.txt", sep="");
    source_features <- paste(source_dir, "features.txt", sep="");
    source_activity_labels <- paste(source_dir, "activity_labels.txt", sep="");
    stopifnot(file.exists(source_subjecttrain));
    stopifnot(file.exists(source_xtrain));
    stopifnot(file.exists(source_ytrain));
    stopifnot(file.exists(source_subjecttest));
    stopifnot(file.exists(source_xtest));
    stopifnot(file.exists(source_ytest));
    stopifnot(file.exists(source_features));
    stopifnot(file.exists(source_activity_labels));

    ## loading source files into memory data
    NTRAIN_OBSERVATIONS <- 7352;
    NTEST_OBSERVATIONS <- 2947;


    ## \warning Some files must be read with "read.table" due to a bug in fread
    ## when there are blank spaces before the first column of data. Then the
    ## reading time is too long:
    ## user  system elapsed
    ## 0.471   0.013   0.485   ## USING fread()
    ## 25.952   0.098  26.050  ## USING read.table()
    ##
    ## To solve this issue during the development phase, intermediate files are
    ## created to allow the use of fread() on them instead of using the
    ## original source files.

    ## \warning The FORMAT of the data is not identical in both files due to
    ## storage format of the values ( 2.5717778e-001 vs 0.25717778) but the
    ## values of both files has been checked to be identical:
    ## identical(summary(data_fread), summary(data_readtable))
    dev_source_xtrain <- "./data/development_X_train.txt";
    dev_source_xtest <- "./data/development_X_test.txt";

    ## -------------------------------------------------------------
    ##Using internal functions to allow quick change of strategy
    ## -------------------------------------------------------------

    read_data_xtrain <- function(){
        if(file.exists(dev_source_xtrain)){
            return(fread(dev_source_xtrain, sep=" "));
        }
        else{
            retval <- read.table(source_xtrain,
                                 header=FALSE,
                                 numerals="no.loss",
                                 colClasses="double",
                                 nrows=NTRAIN_OBSERVATIONS);
            retval <- as.data.table(retval);
            write.table(retval, file=dev_source_xtrain,
                        row.names=FALSE, col.names=FALSE);
            return (retval);
        }
    }

    ## -------------------------------------------------------------

    read_data_xtest <- function(){
        if(file.exists(dev_source_xtest)){
            return(fread(dev_source_xtest, sep=" "));
        }
        else{
            retval <- read.table(source_xtest,
                                 header=FALSE,
                                 numerals="no.loss",
                                 colClasses="double",
                                 nrows=NTEST_OBSERVATIONS);
            retval <- as.data.table(retval);
            write.table(retval, file=dev_source_xtest,
                        row.names=FALSE, col.names=FALSE);
            return (retval);
        }
    }

    ## -------------------------------------------------------------

    if(DEBUG_MODE){
        print("source2memory(): Reading data from files");
        print(system.time(data_subjecttrain <- fread(source_subjecttrain)));
        print(system.time(data_ytrain <- fread(source_ytrain)));
        print(system.time(data_subjecttest <- fread(source_subjecttest)));
        print(system.time(data_ytest <- fread(source_ytest)));
        print(system.time(data_features <- fread(source_features, sep=" ")));
        print(system.time(data_labels<-fread(source_activity_labels,sep=" ")));
        print(system.time(data_xtest <- read_data_xtest()));
        print(system.time(data_xtrain <- read_data_xtrain()));
    }
    else{
        data_subjecttrain <- fread(source_subjecttrain);
        data_ytrain <- fread(source_ytrain);
        data_subjecttest <- fread(source_subjecttest);
        data_ytest <- fread(source_ytest);
        data_features <- fread(source_features, sep=" ");
        data_labels<-fread(source_activity_labels,sep=" ");
        data_xtest <- read_data_xtest();
        data_xtrain <- read_data_xtrain();
    }

    ## stores into memory
    my_memdata$set_data_subjecttrain(data_subjecttrain);
    my_memdata$set_data_ytrain(data_ytrain);
    my_memdata$set_data_xtrain(data_xtrain);
    my_memdata$set_data_subjecttest(data_subjecttest);
    my_memdata$set_data_ytest(data_ytest);
    my_memdata$set_data_xtest(data_xtest);
    my_memdata$set_data_features(data_features);
    my_memdata$set_data_labels(data_labels);

    stopifnot(identical(my_memdata$get_data_subjecttrain(),
                        data_subjecttrain));
    stopifnot(identical(my_memdata$get_data_ytrain(), data_ytrain));
    stopifnot(identical(my_memdata$get_data_xtrain(), data_xtrain));
    stopifnot(identical(my_memdata$get_data_subjecttest(),
                        data_subjecttest));
    stopifnot(identical(my_memdata$get_data_ytest(), data_ytest));
    stopifnot(identical(my_memdata$get_data_xtest(), data_xtest));
    stopifnot(identical(my_memdata$get_data_features(), data_features));
    stopifnot(identical(my_memdata$get_data_labels(), data_labels));
}


## -----------------------------------------------------------------------------

##\function get_source_data()
## Given "RWW" as the current R working directory, this functions download,
## unzip and stores the source data ready to used in the directory named
## "RWW/data/UCI HAR Dataset":
get_source_data<- function()
{
    if(!file.exists("./data")){dir.create("./data")}
    dest_file_name <- "projectfiles_dataset.zip";
    dest_file_path <- paste("./data/", dest_file_name, sep="");
    file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip";
    dest_method <- "curl";

    ## download
    if(!file.exists(dest_file_path))
    {
        download.file(file_url, destfile=dest_file_path, method=dest_method);
    }

    ## unzip
    if(!file.exists("./data/UCI HAR Dataset/"))
    {
        require(utils);
        unzip(dest_file_path, exdir="./data/");
    }
}

## -----------------------------------------------------------------------------
##\class struct_memdata

write_codebook <- function(my_memdata, DEBUG_MODE)
{
    outputfile <- "codebook.md";
    write.table(colnames(my_memdata$get_tidydata_avg()),
                outputfile,
                sep="\n",
                row.name=FALSE);

    ## varnames <- colnames(my_memdata$get_tidydata_avg());
    ## variable_name
    ## var_description
    ## data_type
    ## range

}

## -----------------------------------------------------------------------------
##\class struct_memdata
## Data structure that contains:
## 1) the data loaded from the source files;
## 2) the intermediate data created;
## 3) the final data to report as solution
## This data is accessible for reading/writing through the get/set methods
## -----------------------------------------------------------------------------
struct_memdata<- function()
{

  ## stopifnot(is.matrix(x));

    data_subjecttrain <- data.table();
    data_ytrain <- data.table();
    data_xtrain <- data.table();
    data_subjecttest <- data.table();
    data_ytest <- data.table();
    data_xtest <- data.table();
    data_features <- data.table();
    data_labels <- data.table();

    ## \param tidydata_full: created form the source data:
    ## ("subject_id"    )(col names = data_features)("subject_activity")
    ## (data_subjecttrain)(data_xtrain              )(data_ytrain       )
    ## (data_subjecttest )(data_xtest               )(data_ytest        )
    tidydata_full <- data.table();

    ## \param tidydata_avg: created from 'tidydata_full', but only with the
    ## average of each variable for each activity and each subject.
    tidydata_avg <- data.table();

    get_data_subjecttrain <- function() {return(data_subjecttrain);}
    get_data_ytrain <- function() {return(data_ytrain);}
    get_data_xtrain <- function() {return(data_xtrain);}
    get_data_subjecttest <- function() {return(data_subjecttest);}
    get_data_ytest <- function() {return(data_ytest);}
    get_data_xtest <- function() {return(data_xtest);}
    get_data_features <- function() {return(data_features);}
    get_data_labels <- function() {return(data_labels);}
    get_tidydata_full <- function() {return(tidydata_full);}
    get_tidydata_avg <- function() {return(tidydata_avg);}


    set_data_subjecttrain<- function(new_value)
    {
        stopifnot(is.data.table(new_value));
        data_subjecttrain <<- new_value;
    }

    set_data_ytrain<- function(new_value)
    {
        stopifnot(is.data.table(new_value));
        data_ytrain <<- new_value;
    }

    set_data_xtrain<- function(new_value)
    {
        stopifnot(is.data.table(new_value));
        data_xtrain <<- new_value;
    }

    set_data_subjecttest<- function(new_value)
    {
        stopifnot(is.data.table(new_value));
        data_subjecttest <<- new_value;
    }

    set_data_ytest<- function(new_value)
    {
        stopifnot(is.data.table(new_value));
        data_ytest <<- new_value;
    }

    set_data_xtest<- function(new_value)
    {
        stopifnot(is.data.table(new_value));
        data_xtest <<- new_value;
    }

    set_data_features<- function(new_value)
    {
        stopifnot(is.data.table(new_value));
        data_features <<- new_value;
    }

    set_data_labels<- function(new_value)
    {
        stopifnot(is.data.table(new_value));
        data_labels <<- new_value;
    }

    set_tidydata_full <- function(new_value)
    {
        stopifnot(is.data.table(new_value));
        tidydata_full <<- new_value;
    }

    set_tidydata_avg <- function(new_value)
    {
        stopifnot(is.data.table(new_value));
        tidydata_avg <<- new_value;
    }

    expected_step1_ncol_tidydata_full <- function() {return(563);}
    expected_nrow_tidydata_full <- function() {return(10299);}
    expected_step2_ncol_tidydata_full <- function() {return(68);}
    expected_ncol_tidydata_avg <- function() {return(68);}
    expected_nrow_tidydata_avg <- function() {return(180);}


    list(get_data_subjecttrain = get_data_subjecttrain,
         get_data_ytrain = get_data_ytrain,
         get_data_xtrain = get_data_xtrain,
         get_data_subjecttest = get_data_subjecttest,
         get_data_ytest = get_data_ytest,
         get_data_xtest = get_data_xtest,
         get_data_features = get_data_features,
         get_data_labels = get_data_labels,
         get_tidydata_full = get_tidydata_full,
         get_tidydata_avg = get_tidydata_avg,

         expected_step1_ncol_tidydata_full = expected_step1_ncol_tidydata_full,
         expected_nrow_tidydata_full = expected_nrow_tidydata_full,
         expected_step2_ncol_tidydata_full = expected_step2_ncol_tidydata_full,
         expected_ncol_tidydata_avg = expected_ncol_tidydata_avg,
         expected_nrow_tidydata_avg = expected_nrow_tidydata_avg,

         set_data_subjecttrain = set_data_subjecttrain,
         set_data_ytrain = set_data_ytrain,
         set_data_xtrain = set_data_xtrain,
         set_data_subjecttest = set_data_subjecttest,
         set_data_ytest = set_data_ytest,
         set_data_xtest = set_data_xtest,
         set_data_features = set_data_features,
         set_data_labels = set_data_labels,
         set_tidydata_full = set_tidydata_full,
         set_tidydata_avg = set_tidydata_avg);

}

## -----------------------------------------------------------------------------

