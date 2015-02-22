## "xtrain.txt"
##   2.5717778e-001 -2.3285230e-002 -1.4653762e-002
##   2.8602671e-001 -1.3163359e-002 -1.1908252e-001

## "xtest.txt"
##  2.8858451e-001 -2.0294171e-002 -1.3290514e-001
##  2.7841883e-001 -1.6410568e-002 -1.2352019e-001

## "ytrain.txt"
## 5
## 5

## "ytest.txt"
## 2
## 4



## \todo preserve the 'double' format upon loading the files:
## expected: 2.5717778e-001
## obtained: 0.2571778
## identical(2.5717778e-001, 0.2571778)
## [1] FALSE
test <- function()
{
    require(data.table); ## fread(dest_file_path, sep=" ");

    source_xtrain <- "./test/xtrain.txt";
    source_xtest <- "./test/xtest.txt";
    source_ytrain <- "./test/ytrain.txt";
    source_ytest <- "./test/ytest.txt";

    df_xtrain <- read.table(source_xtrain, header=FALSE,
                               colClasses="double", numerals="no.loss");
    ## print(df_xtrain);
    ##          V1          V2          V3
    ## 1 0.2571778 -0.02328523 -0.01465376
    ## 2 0.2860267 -0.01316336 -0.11908252

    df_xtest<-read.table(source_xtest, header=FALSE,
                         colClasses="double", numerals="no.loss");
    ## print(df_xtest);
    ##          V1          V2         V3
    ## 1 0.2885845 -0.02029417 -0.1329051
    ## 2 0.2784188 -0.01641057 -0.1235202
    ##
    ## is.double(df_xtest[1,2])
    ## [1] TRUE

    df_ytrain <- read.table(source_ytrain, header=FALSE,
                               colClasses="double", numerals="no.loss");
    df_ytest<-read.table(source_ytest, header=FALSE,
                         colClasses="double", numerals="no.loss");

    ## browser();

    dfx <- rbind(df_xtrain, df_xtest);
    dfy <- rbind(df_ytrain, df_ytest);
    aux_dfsubject <- c(1,4,24,30);
    data_features <- data.table(c(1,2,3),
        c("tBodyAcc-mean()-Z", "fBodyBodyGyroJerkMag-meanFreq()", "tBodyAcc-std()-X"));
    tidy_input_dataset <- cbind(aux_dfsubject, dfx, dfy);
    print(tidy_input_dataset);
    ##          V1          V2          V3 V1
    ## 1 0.2571778 -0.02328523 -0.01465376  5
    ## 2 0.2860267 -0.01316336 -0.11908252  5
    ## 3 0.2885845 -0.02029417 -0.13290514  2
    ## 4 0.2784188 -0.01641057 -0.12352019  4


    ## default colnames: V1 V2 ...

    ## ACTION: setting my colnames
    subject_names <- c("subject_id");

    ## the next 2 are identical
    ## features_names <- as.vector(data_features$V2);
    features_names <- (as.data.frame(data_features)[, 2]);
    data_labels <- c("subject_activity");
    new_colnames <- c(subject_names, features_names, data_labels);
    setnames(tidy_input_dataset, new_colnames)
    print(tidy_input_dataset);
    ## colnames(retval)

    ## [1] "subject_id" "tBodyAcc-mean()-Z" "fBodyBodyGyroJerkMag-meanFreq()"
    ## [4] "tBodyAcc-std()-X" "subject_activity"


    ## ACTION:
    ## 2.- Extracts only the measurements on the mean and standard deviation
    ##     (std) for each measurement.
    ## mean_std_pattern <- "[Mm]ean\\(\\)|std\\(\\)";
    mean_std_pattern <- "mean\\()|std\\()"
    wantedvars <- grepl(mean_std_pattern,
                                 x=colnames(tidy_input_dataset),
                                 ignore.case=FALSE)

    wantedvars[1] <- TRUE; #preserve first & las colums
    print(wantedvars);
    wantedvars[length(wantedvars)] <- TRUE;
    print(wantedvars);

    print(tidy_input_dataset);
    ## print(tidy_input_dataset[wantedvars]);
    tidy_input_dataset <- tidy_input_dataset[wantedvars];
    print(tidy_input_dataset);
    return(tidy_input_dataset);

    ## CONSOLE grep

    ## pattern <- "std"

    ## unix >>grep "std" features.txt |wc
    ##       33      66     779               # ->   33 valores

}

step3 <- function()
{
    require(data.table); ## fread(dest_file_path, sep=" ");
    dt <- data.table(c(1,2,3,4,5,6), c(1,1,2,1,3,3));
    print(dt);

    source_dir <- "./data/UCI HAR Dataset/";
    source_activity_labels <- paste(source_dir, "activity_labels.txt", sep="");
    source_activity_labels <- paste(source_dir, "activity_labels.txt", sep="");
    data_labels<-fread(source_activity_labels,sep=" ");
    print(data_labels);

    new_colnames <- c("index", "subject_activity");
    setnames(dt, new_colnames);
    print(dt);


    for(loopi in 1:nrow(data_labels))
    {
        print(loopi);
        print(data_labels$V1[loopi]);
        print(data_labels$V2[loopi]);
        dt$subject_activity[dt$subject_activity==data_labels$V1[loopi]] <-
            data_labels$V2[loopi];
    }

    return(dt);
}
