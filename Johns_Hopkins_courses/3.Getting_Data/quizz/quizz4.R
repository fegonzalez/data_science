
## QUIZZ 4

## -----------------------------------------------------------------------------

require(data.table);

## -----------------------------------------------------------------------------

q1_get_data<- function()
{
    if(!file.exists("./data")){dir.create("./data")}

    ## code book
    dest_file_name <- "quizz4.q1.codebook.pdf";
    dest_file_path <- paste("./data/", dest_file_name, sep="");
    file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf";
    dest_method <- "wget";
    if(!file.exists(dest_file_path))
    {
        download.file(file_url, destfile=dest_file_path, method=dest_method);
    }

    ## source data
    dest_file_name <- "quizz4.q1.sourcedata.csv"
    dest_file_path <- paste("./data/", dest_file_name, sep="");
    file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv";
    dest_method <- "curl";
    if(!file.exists(dest_file_path))
    {
        download.file(file_url, destfile=dest_file_path, method=dest_method);
    }

    retval <- fread(dest_file_path, sep=",");
    return(retval);
}


## Apply strsplit() to split all the names of the data frame on the characters
## "wgtp". What is the value of the 123 element of the resulting list?

q1 <- function()
{
    q1data <- q1_get_data();

    return(strsplit(x=colnames(q1data), split="wgtp")[[123]]);
    ## Solution = [1] ""   "15"
}

## -----------------------------------------------------------------------------

## Same data as in quizz3/q3_get_data()
q2_get_data <-  function()
{
    if(!file.exists("./data")){dir.create("./data")}

    ## source data
    dest_file_name <- "quizz4.q2.sourcedata.csv"
    dest_file_path <- paste("./data/", dest_file_name, sep="");
    file_url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv";
    dest_method <- "curl";
    if(!file.exists(dest_file_path))
    {
        download.file(file_url, destfile=dest_file_path, method=dest_method);
    }

    FIRST_RANK_LINE <- 6;
    FIRST_RANK_COUNTRY <- 1;
    LAST_RANK_COUNTRY <- 190;
    gdp_data <- fread(input=dest_file_path,
                      sep=",",
                      skip=FIRST_RANK_LINE-1,
                      select=c(1,2,4,5), # selected columns
                      nrows=LAST_RANK_COUNTRY-FIRST_RANK_COUNTRY+1);

    ## colClasses=c(character,));
    setnames(gdp_data, c("CountryCode", "Ranking",
                         "Economy", "millionsUSdolar"));
    return (gdp_data)
}


## Remove the commas from the GDP numbers in millions of dollars and average
## them. What is the average?
q2<- function()
{
    dtsource <- q2_get_data();
    return (mean(as.numeric(gsub(x=q2data$millionsUSdolar, pattern=",", replacement=""))));
    ## Solution = [1] 377652.4
}

## -----------------------------------------------------------------------------

## In the data set from Question 2 what is a regular expression that would
## allow you to count the number of countries whose name begins with "United"?

## Assume that the variable with the country names in it is named
## countryNames. How many countries begin with United?
q3 <- function()
{
    q3data <- q2_get_data();

    ## In my data: countryNames = Economy
    howmany <- length(grep("^United", x = q2data$Economy, value=FALSE, ignore.case=FALSE));

    return(paste("grep(\"^United\",countryNames), ", howmany));
    ## Result: grep("^United",countryNames), 3
}

## -----------------------------------------------------------------------------

get_q4_gdp_data <- function()
{
    return(q2_get_data());
}

get_q4_country_data <- function()
{
    if(!file.exists("./data")){dir.create("./data")}

    file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv";
    dest_file_name <- "quizz4.countrydata.csv"
    dest_file_path <- paste("./data/", dest_file_name, sep="");
    dest_method <- "curl";
    if(!file.exists(dest_file_path))
    {
        download.file(file_url, destfile=dest_file_path, method=dest_method);
    }
    country_data <- fread(dest_file_path, sep=",");
    ## Remove blank spaces in column names "Income Group"-> "Income.Group"
    tidy.names <- make.names(names(country_data), unique=TRUE);
    setnames(country_data, tidy.names);

}

q4 <- function()
{
    gdpdata <- get_q4_gdp_data();
    countrydata <- get_q4_country_data();

    ## Match the data based on the country shortcode.
    setkey(gdpdata, CountryCode);
    setkey(countrydata, CountryCode);


    ## Of the countries for which the end of the fiscal year is available, how
    ## many end in June?

    ##  ??fiscal year DB & table ¿meaning?
    ##
    ##  which(grepl("June", x = gdpdata, ignore.case=TRUE))
    ##  integer(0)
    ## which(grepl("June", x=countrydata, ignore.case=TRUE))
    ##  [1] 10
    ##  > names(countrydata)[10]
    ## [1] "Special.Notes"
    ##
    ## Therefore: countrydata$Special.Notes is the table use
    ## names(countrydata)[which(grepl("June", x=countrydata, ignore.case=TRUE))]
    ## [1] "Special.Notes"
    ##
    ## ## Searching the string to look for
    ## grep(pattern="June", x=countrydata$Special.Notes,
    ##      ignore.case=FALSE, value=FALSE)
    ## ## [1]  11  18  31  60  76 105 111 112 145 166 173 188 192 199 216 234
    ## grep(pattern="June", x=countrydata$Special.Notes[11],
    ##      ignore.case=FALSE, value=TRUE)
    ## ## [1] "Fiscal year end: June 30; reporting period for national accounts
    ## ## data: FY."
    ## grep(pattern="June", x=countrydata$Special.Notes[234],
    ##      ignore.case=FALSE, value=TRUE)
    ## ## [1] "Fiscal year end: June 30; reporting period for national accounts
    ## ## data: CY."
    ## ##

    ## Therefore: Look for "Fiscal year end: June"
    ## Of the countries for which the end of the fiscal year is available, how
    ## many end in June?

    return(length(grep(pattern="Fiscal year end: June",
                       x=countrydata$Special.Notes, ignore.case=FALSE)));
    ## [1] 13
}

## -----------------------------------------------------------------------------

q5 <- function()
{
    require(quantmod);
    ## install.packages("quantmod");
    amzn = getSymbols("AMZN",auto.assign=FALSE);
    sampleTimes = index(amzn);


    ## How many values were collected in 2012?
    ## USING year in package:lubridate
    year_2012 <- (year(ymd(sampleTimes))==2012);
    sol1 <- sum(year_2012);
    ## sol1<- sum(year(ymd(sampleTimes))==2012);
    ## [1] 250

    ## How many values were collecte on Mondays in 2012?
    Sys.setlocale("LC_TIME", "en_US");     #set locale to english
    sol2 <- sum(weekdays(sampleTimes[year_2012], abbreviate=TRUE) == "Mon");
    ## [1] 47
    return(c(sol1,sol2));
}

## -----------------------------------------------------------------------------

solve <- function()
{
    print(q1());
    print(q2());
    print(q3());
    print(q4());
    print(q5());

    ## Solutions

    ## [1] ""   "15"
    ## [1] 377652.4
    ## [1] "grep(\"^United\",countryNames),  3"
    ## [1] 13
    ## [1] 250  47
}
