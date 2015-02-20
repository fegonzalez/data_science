
## --------------------------------------------------------------------------

download_csv_file <- function()
{
    url_value <-
        "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv";
    url_method <- "wget";
    dest_file_name<- "cameras.csv";
    download.file(url=url_value, destfile=dest_file_name, method=url_method);
}

## --------------------------------------------------------------------------

download_excel_file <- function()
{
    url_value <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx";
    url_method <- "wget";
    dest_file_name<- "gov_NGAP.xlsx";
    download.file(url=url_value, destfile=dest_file_name, method=url_method);
    return (dest_file_name);
}

## --------------------------------------------------------------------------

download_xml_file <- function(dest_file_name = "restaurants.xml")
{
    ## browser();
    url_value <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml";
    url_method <- "wget";
    download.file(url=url_value, destfile=dest_file_name, method=url_method);
    return (dest_file_name);
}
## --------------------------------------------------------------------------

download_idahohousing <- function(dest_file_name = "idaho_housing.xml")
{
    url_value <-
        "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv";
    url_method <- "wget";
    download.file(url=url_value, destfile=dest_file_name, method=url_method);
    return (dest_file_name);
}

## --------------------------------------------------------------------------

load_xml <- function()
{
    dest_file_name <- "restaurants.xml";

    if( ! is.element(dest_file_name, list.files()))
    {
        download_xml_file(dest_file_name);
    }

    library(XML);
    doc <- xmlTreeParse(dest_file_name, useInternalNodes=TRUE)
    return(doc);
}

## --------------------------------------------------------------------------

load_idaho_housing <- function()
{
    file_name <- "idaho_housing.xml";
    if( ! is.element(file_name, list.files()))
    {
        download_idahohousing(file_name);
    }

    library(data.table);
    ## tiempo <- system.time(csv_file <- read.csv(file_name));
    ## cat("\nread.csv: \n", tiempo)
    ## tiempo <-
    ##  system.time(table_file <- read.table(file_name, sep=",", header=TRUE));
    ## cat("\nread.table: \n", tiempo)

    tiempo <-
        system.time(dtfile<- fread(file_name,sep="auto",header=TRUE));
    cat("\nfread: \n", tiempo);
    cat("\n\n");

    ## read.csv:
    ##  2.666 0.026 2.691 0 0
    ## read.table:
    ##  2.544 0.032 2.575 0 0
    ## fread:
    ##  0.107 0.007 0.115 0 0


    return(dtfile);
}

## --------------------------------------------------------------------------

load_cameras_csv<- function()
{
    library(data.table);
    file_name <- "cameras.csv";
    tiempo <- system.time(csv_file <- read.csv(file_name));
    cat("\nread.csv: \n", tiempo)

    #dim(csv_file);
    tiempo <-
        system.time(table_file <- read.table(file_name, sep=",", header=TRUE));
    cat("\nread.table: \n", tiempo)

    #dim(table_file);
    ## data.table
    tiempo <-
        system.time(dtfile<- fread(file_name,sep="auto",header=TRUE));
    cat("\nfread: \n", tiempo);
    cat("\n\n");
    #dim(datatable_file_1);
    ## tables();
    return(dtfile);
}

## --------------------------------------------------------------------------

## Q1) How many properties are worth $1,000,000 or more?
## colname "VAL"
## colnum =  37
q1 <- function()
{
    download_excel_file();
    dtfile <- load_cameras_csv();
    setkey(dtfile,VAL)
    dtfile[, .N, by=VAL]
    ## Solution = 53
}

## --------------------------------------------------------------------------

## Q3) What's the value of ' sum(dat$Zip*dat$Ext,na.rm=T) '
q3 <- function()
{
    library(xlsx);
    dest_file_name <- download_excel_file();
    dat <- read.xlsx("gov_NGAP.xlsx", sheetIndex=1,
                     rowIndex=18:23, colIndex=7:15);
    sum(dat$Zip*dat$Ext,na.rm=T);
    ## Solution = 36534720
}

## --------------------------------------------------------------------------

## Q4) Read the XML data on Baltimore restaurants from here:
## https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml
## How many restaurants have zipcode 21231?
q4 <- function()
{
    doc <- load_xml(); # xml doc
    my_root_node <- xmlRoot(doc) # root node
    xmlName(my_root_node);
    xpathApply(my_root_node, "//zipcode", xmlValue)
    sum(milistanodos=="21231")
    ## Solution = 127
}


## --------------------------------------------------------------------------

## Q5) Which of the following is the fastest way to calculate the average value
## of the variable "pwgtp15"
q5 <- function()
{
    DT <- load_idaho_housing();

    tiempo <-
        system.time(DT[,mean(pwgtp15),by=SEX]);
    cat("\nDT[,mean(pwgtp15),by=SEX]: ", tiempo);

    tiempo <-system.time(
        {
            mean(DT$pwgtp15,by=DT$SEX);
        })
    cat("\nmean(DT$pwgtp15,by=DT$SEX): ", tiempo);

    cat("\n\n");

    return(DT);
    ## Solution = 53
}
<
