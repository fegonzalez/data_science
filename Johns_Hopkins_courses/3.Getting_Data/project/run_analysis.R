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
################################################################################


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

"train/subject_train.txt" : 7352 observations; 1 variables
"train/X_train.txt"       : 7352 observations; 561 variables
"train/y_train.txt"       : 7352 observations; 1 variables

"test/subject_test.txt"   : 2947 observations; 1 variables
"test/X_test.txt"         : 2947 observations; 561 variables
"test/y_test.txt"         : 2947 observations; 1 variables

"features.txt"            : 561 observations; 2 vars

"activity_labels.txt"     : 6 observations; 2 vars


## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## $ADD_TO_DOC
##
## 2.- Process to transform the source data into the tidy data.
##
## In this point is explained which part of the final tidy data-set comes from
## which part/file of the source data files.


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


## 2.1) Tidy dataset from up to bottom (rows = observations)
##
## Section a) Training observations: 7352 observations ; rows 1-7352
## Section b) Test observations:     2947 observations ; rows 7353-10299


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
## Source files: "train/Y_train.txt", "test/Y_test.txt".


##
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





step1<- function()
{

    stopifnot(file.exists("./data"));
    stopifnot(file.exists("./data/UCI HAR Dataset/train/X_train.txt"));
    stopifnot(file.exists("./data/UCI HAR Dataset/train/y_train.txt"));



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

solve <- function()
{
    get_source_data();

    ## tidydata <- source2tidy();
}

## -----------------------------------------------------------------------------

get_source_data<- function() {
    if(!file.exists("./data")){dir.create("./data")}
    dest_file_name <- "projectfiles_dataset.zip";
    dest_file_path <- paste("./data/", dest_file_name, sep="");
    file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip";
    dest_method <- "curl";

    ## downloading
    if(!file.exists(dest_file_path))
    {
        download.file(file_url, destfile=dest_file_path, method=dest_method);
    }

    ## unzipping
    if(!file.exists("./data/UCI HAR Dataset/"))
    {
        require(utils);
        unzip(dest_file_path, exdir="./data/");
    }

    ## require(data.table);
    ## retval <- fread(dest_file_path, sep=",");
    ## return(retval);

}

## -----------------------------------------------------------------------------


