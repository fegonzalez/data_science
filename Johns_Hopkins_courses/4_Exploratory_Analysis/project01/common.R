
require(data.table);
require(dplyr);
require(lubridate);


## -----------------------------------------------------------------------------

##\function get_source_data()
## Given "RWW" as the current R working directory, this function download,
## unzip and stores the source data ready to used this way:
## "RWW/data/household_power_consumption.txt"
get_source_data<- function()
{
    SOURCE_DATA <- "./data/household_power_consumption.txt";
    if(file.exists(SOURCE_DATA))
        return(0);

    if(!file.exists("./data")){dir.create("./data")}
    dest_file_name <- "household_power_consumption.zip";
    dest_file_path <- paste("./data/", dest_file_name, sep="");
    file_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip";
    dest_method <- "curl";

    ## download
    if(!file.exists(dest_file_path))
    {
        download.file(file_url, destfile=dest_file_path, method=dest_method);
    }

    ## unzip
    if(!file.exists(SOURCE_DATA))
    {
        require(utils);
        unzip(dest_file_path, exdir="./data/");
    }
    stopifnot(file.exists(SOURCE_DATA));
}

## -----------------------------------------------------------------------------

## \function get_power_data()
##
## \return a dataset of 2880 observations and 10 variables, (dplyr::tbl_df
## object). The observations correspond to the data from the dates 2007-02-01
## and 2007-02-02 of the source file (./data/household_power_consumption.txt).
## The variables correspond to the nine variables of the source file, plus a
## 10th variable (date_time) with the Date & Time in R format:
## date_time: composition of Date & Time variables is POSIXct format
##            e.g. : "2007-02-01 00:01:00 CET"
get_power_data<- function(DEBUG_MODE=FALSE)
{
    SOURCE_DATA <- "./data/household_power_consumption.txt";
    stopifnot(file.exists(SOURCE_DATA));
    if(DEBUG_MODE)
    {
        ## checking data size
        finfo <- file.info(SOURCE_DATA);
        cat("INFO: sizeof(input dataset) = ", finfo$size/(2^20), "MB", "\n");
    }

    ## 1) read the file (only the required rows)

    ##\warning Implementation optimization: avoid reading all the file
    ## Getting info by O.S. commands (does not require load the file with R)
    ## unix >>grep -nm1 "1/2/2007" household_power_consumption.txt
    ## 66638:1/2/2007;00:00:00;0.326;
    ## unix >>grep -nm1 "2/2/2007" household_power_consumption.txt
    ## 68078:2/2/2007;00:00:00;1.314;0.000;242.770;5.400;0.000;0.000;18.000
    ## unix >>grep -nm1 "3/2/2007" household_power_consumption.txt
    ## 69518:3/2/2007;00:00:00;3.614;0.106;240.990;15.000;0.000;1.000;18.000
    DAY1_FILE_FORMAT <- "1/2/2007";
    DAY2_FILE_FORMAT <- "2/2/2007";
    DAY1_FIRSTLINE_INFILE <- 66638;
    DAY2_FIRSTLINE_INFILE <- 68078;
    DAY3_FIRSTLINE_INFILE <- 69518;
    DAY1_LASTLINE_INFILE <- DAY2_FIRSTLINE_INFILE - 1;
    DAY2_LASTLINE_INFILE <- DAY3_FIRSTLINE_INFILE - 1;
    if(DEBUG_MODE)
    {
        cat("INFO: read lines from ", DAY1_FIRSTLINE_INFILE,
            " to ", DAY2_LASTLINE_INFILE, "(",
            DAY2_LASTLINE_INFILE-DAY1_FIRSTLINE_INFILE+1, "lines)", "\n");
    }
    retdata_names <- fread(SOURCE_DATA, nrows=1);
    retdata <- fread(SOURCE_DATA, sep=";", na.strings="?",
                     skip=DAY1_FIRSTLINE_INFILE-1,
                     nrows=DAY2_LASTLINE_INFILE-DAY1_FIRSTLINE_INFILE+1);
    setnames(retdata, colnames(retdata_names));
    stopifnot(nrow(retdata)==DAY2_LASTLINE_INFILE-DAY1_FIRSTLINE_INFILE+1);
    retdata <- tbl_df(as.data.frame(retdata));

    ## 2) Add a column with the Date & Time in R format ("POSIXct")

    ##\info Date formats
    ##
    ## a) source file: "1/2/2007" -> DD/MM/YYYY
    ## unix >>grep -m1 "2007" household_power_consumption.txt
    ## Time formats
    ## a) source file: "17:24:00" -> HH:MM:SS

    ##Error: `mutate` does not support `POSIXlt` results
    poxis_date <-as.POSIXct(strptime(paste(retdata$Date,retdata$Time),
                                     "%d/%m/%Y %H:%M:%S"));
    retdata <- mutate(retdata, date_time = poxis_date);
    return (retdata);
}

## -----------------------------------------------------------------------------
