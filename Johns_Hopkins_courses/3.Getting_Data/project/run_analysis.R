################################################################################
##
## You should create one R script called run_analysis.R that does the following.
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


require(data.table);



##------------------------------------------------------------------------------
## step1() Merges the training and the test sets to create one data set.
##------------------------------------------------------------------------------
## 1.  import all files (except the inertial signals)
## 2.  add the values from 'Features.txt' to 'X_test and X_train as column names
## 3.  add column names to subject_train, subject_test, Y_test and Y_train
## 4.  convert the values in Y_test and Y-train so that they correspond to
##     those in 'activety_labels.txt'
## 5.  Combine columns from subject_train, Y_train and X_train
## 6.  Combine columns from subject_test, Y_test and X_test
## 7.  Combine the rows from the previous two tables
## 8.  tidy the table


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
## ("subject_id2") ( feature_names ) ("subject_activity")
##            (a1) (  ... a2 ...   ) (a3)
##            (b1) (  ... b2 ...   ) (b3)
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
##
## Source files: "train/Y_train.txt", "test/Y_test.
##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



## -----------------------------------------------------------------------------

################################################################################
##
## You should create one R script called run_analysis.R that does the following.
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
solve <- function()
{
    the_memdata <- struct_memdata();
    get_source_data();
    step1(the_memdata);

    ## tidydata <- source2tidy();
}



##------------------------------------------------------------------------------
## 1.- Merges the training and the test sets to create one data set.
##
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
## ("subject_id2") ( feature_names ) ("subject_activity")
##            (a1) (  ... a2 ...   ) (a3)
##            (b1) (  ... b2 ...   ) (b3)
##
## -----------------------------------------------------------------------------

step1<- function(my_memdata)
{
    ## input validation
    stopifnot(!is.struct_memdata(my_memdata));
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
    DEBUG_MODE <- TRUE;

    ## \warning Some files must be read with "read.table" due to a bug in fread
    ## when there are blank spaces before the first column of data. Then the
    ## rading time is too long:
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
            write.table(retval, file=dev_source_xtest,
                        row.names=FALSE, col.names=FALSE);
            return (retval);
        }
    }

    ## -------------------------------------------------------------

    if(DEBUG_MODE){
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

    my_memdata$set_data_subjecttrain(data_subjecttrain);
    my_memdata$set_data_ytrain(data_ytrain);
    my_memdata$set_data_xtrain(data_xtrain);
    my_memdata$set_data_subjecttest(data_subjecttest);
    my_memdata$set_data_ytest(data_ytest);
    my_memdata$set_data_xtest(data_xtest);
    my_memdata$set_data_features(data_features);
    my_memdata$set_data_labels(data_labels);

    print(system.time(
    {
    stopifnot(identical(my_memdata$get_data_subjecttrain(), data_subjecttrain));
    stopifnot(identical(my_memdata$get_data_ytrain(), data_ytrain));
    stopifnot(identical(my_memdata$get_data_xtrain(), data_xtrain));
    stopifnot(identical(my_memdata$get_data_subjecttest(), data_subjecttest));
    stopifnot(identical(my_memdata$get_data_ytest(), data_ytest));
    stopifnot(identical(my_memdata$get_data_xtest(), data_xtest));
    stopifnot(identical(my_memdata$get_data_features(), data_features));
    stopifnot(identical(my_memdata$get_data_labels(), data_labels));
    }));
}

##------------------------------------------------------------------------------
## step5() From the data set in step 4, creates a second, independent tidy data
## set with the average of each variable for each activity and each subject.
## -----------------------------------------------------------------------------

## Please upload your data set as a txt file created with write.table() using
## row.name=FALSE (do not cut and paste a dataset directly into the text box,
## as this may cause errors saving your submission).
step5 <- function()
{
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

    ## ]
    ## \

    get_data_subjecttrain <- function() {return(data_subjecttrain);}
    get_data_ytrain <- function() {return(data_ytrain);}
    get_data_xtrain <- function() {return(data_xtrain);}
    get_data_subjecttest <- function() {return(data_subjecttest);}
    get_data_ytest <- function() {return(data_ytest);}
    get_data_xtest <- function() {return(data_xtest);}
    get_data_features <- function() {return(data_features);}
    get_data_labels <- function() {return(data_labels);}

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


    list(get_data_subjecttrain = get_data_subjecttrain,
         get_data_ytrain = get_data_ytrain,
         get_data_xtrain = get_data_xtrain,
         get_data_subjecttest = get_data_subjecttest,
         get_data_ytest = get_data_ytest,
         get_data_xtest = get_data_xtest,
         get_data_features = get_data_features,
         get_data_labels = get_data_labels,
         set_data_subjecttrain = set_data_subjecttrain,
         set_data_ytrain = set_data_ytrain,
         set_data_xtrain = set_data_xtrain,
         set_data_subjecttest = set_data_subjecttest,
         set_data_ytest = set_data_ytest,
         set_data_xtest = set_data_xtest,
         set_data_features = set_data_features,
         set_data_labels = set_data_labels);

}


