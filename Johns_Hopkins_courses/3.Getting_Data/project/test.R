## "xtraining.txt"
##   2.5717778e-001 -2.3285230e-002 -1.4653762e-002
##   2.8602671e-001 -1.3163359e-002 -1.1908252e-001

## "xtest.txt"
##  2.8858451e-001 -2.0294171e-002 -1.3290514e-001
##  2.7841883e-001 -1.6410568e-002 -1.2352019e-001

## "ytraining.txt"
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

    source_xtraining <- "./xtraining.txt";
    source_xtest <- "./xtest.txt";
    source_ytraining <- "./ytraining.txt";
    source_ytest <- "./ytest.txt";

    df_xtraining <- read.table(source_xtraining, header=FALSE,
                               colClasses="double", numerals="no.loss");
    ## print(df_xtraining);
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

    df_ytraining <- read.table(source_ytraining, header=FALSE,
                               colClasses="double", numerals="no.loss");
    df_ytest<-read.table(source_ytest, header=FALSE,
                         colClasses="double", numerals="no.loss");

    dfx <- rbind(df_xtraining, df_xtest);
    dfy <- rbind(df_ytraining, df_ytest);
    tidy_input_dataset <- cbind(dfx, dfy);
    ## print(tidy_input_dataset);
    ##          V1          V2          V3 V1
    ## 1 0.2571778 -0.02328523 -0.01465376  5
    ## 2 0.2860267 -0.01316336 -0.11908252  5
    ## 3 0.2885845 -0.02029417 -0.13290514  2
    ## 4 0.2784188 -0.01641057 -0.12352019  4

    return(tidy_input_dataset);
}
