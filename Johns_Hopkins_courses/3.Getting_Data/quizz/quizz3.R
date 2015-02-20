
## QUIZZ 3

## -----------------------------------------------------------------------------

q1_get_data<- function()
{
    if(!file.exists("./data")){dir.create("./data")}

    ## code book
    dest_file_name <- "getdata_data_PUMSDataDict06.pdf";
    dest_file_path <- paste("./data/", dest_file_name, sep="");
    file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf";
    dest_method <- "wget";
    if(!file.exists(dest_file_path))
    {
        download.file(file_url, destfile=dest_file_path, method=dest_method);
    }

    ## source data
    dest_file_name <- "getdata_data_ss06hid.csv"
    dest_file_path <- paste("./data/", dest_file_name, sep="");
    file_url <-
        "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv";
    dest_method <- "curl";
    if(!file.exists(dest_file_path))
    {
        download.file(file_url, destfile=dest_file_path, method=dest_method);
    }

    require(data.table);
    retval <- fread(dest_file_path, sep=",");
    return(retval);
}

## Create a logical vector that identifies the households on greater than 10
## acres who sold more than $10,000 worth of agriculture products. Assign that
## logical vector to the variable agricultureLogical.
q1 <- function()
{
    q1data <- q1_get_data();

    ## ACR: lot size
    ## AGS: Sales of Agriculture Products

    ## households on greater than 10 acres
    ## index10acres <- which(q1data$ACR==3);

    ## sold more than $10,000 worth of agriculture products: AGS = 6
    agricultureLogical<-intersect( which(q1data$AGS==6), which(q1data$ACR==3));

    q1_solution <- (agricultureLogical[1:3]);
    cat("\nq1.solution: ", q1_solution, "\n");
    ## return(q1_solution);

    ## Solution = 125 238 262
}

## -----------------------------------------------------------------------------

q2<- function()
{
    if(!require(jpeg))
    {
        ## suppressPackageStartupMessages();
        install.packages(jpeg, repos="http://cran.es.r-project.org/")
    }

    if(!file.exists("./data")){dir.create("./data")}
    dest_file_name<- "getdata_jeff.jpg"
    dest_file_path<- paste("./data/", dest_file_name,sep="");
    if(!file.exists(dest_file_path))
    {
        file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
        download.file(file_url, destfile=dest_file_path,
                      method="curl", mode="wb");
    }

    jpegdata <- readJPEG(source=dest_file_path, native=TRUE);

    q2_solution <- quantile(jpegdata, probs=c(0.3, 0.8));
    q2_solution;
    cat("\nq2.solution: ", q2_solution, "\n");
    ## return(q2_solution);

    ## Solution = -15259150 -10575416
}

## -----------------------------------------------------------------------------

q3_get_data<- function()
{
    require(data.table);

    if(!file.exists("./data")){dir.create("./data")}

    ##source data
    file_url <-
        "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv";
    dest_file_name <- "getdata_data_GDP.csv";
    dest_file_path <- paste("./data/", dest_file_name, sep="");
    dest_method <- "wget";
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
    setnames(gdp_data, c("CountryCode", "Ranking", "Economy", "millionsUS$"));


    file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv";
    dest_file_name <- "getdata_data_EDSTATS_Country.csv";
    dest_file_path <- paste("./data/", dest_file_name, sep="");
    dest_method <- "wget";
    if(!file.exists(dest_file_path))
    {
        download.file(file_url, destfile=dest_file_path, method=dest_method);
    }
    country_data <- fread(dest_file_path, sep=",");
    ## Remove blank spaces in column names "Income Group"-> "Income.Group"
    tidy.names <- make.names(names(country_data), unique=TRUE);
    setnames(country_data, tidy.names);

    ## retdata <- c(gdp_data, country_data);

    retdata <- list(gdp_data, country_data);
    return(retdata);
    ## Solution =
}

## Create a logical vector that identifies the households on greater than 10
## acres who sold more than $10,000 worth of agriculture products. Assign that
## logical vector to the variable agricultureLogical.
q3 <- function()
{
    q3data <- q3_get_data();
    summary(q3data)
##      Length Class      Mode
## [1,]  4     data.table list
## [2,] 31     data.table list

    class(q3data[[1]])
    ## [1] "data.table" "data.frame"
    class(q3data[[2]])
    ## [1] "data.table" "data.frame"

    ##     colnames(q3data[[1]])
## [1] "CountryCode" "Ranking"     "Economy"     "millionsUS$"

    ## > colnames(q3data[[2]])
##  [1] "CountryCode"
##  [2] "Long Name"
##  [3] "Income Group"
##  [4] "Region"
##  [5] "Lending category"
##  [6] "Other groups"
##  [7] "Currency Unit"
##  [8] "Latest population census"
##  [9] "Latest household survey"
## [10] "Special Notes"
## [11] "National accounts base year"
## [12] "National accounts reference year"
## [13] "System of National Accounts"
## [14] "SNA price valuation"
## [15] "Alternative conversion factor"
## [16] "PPP survey year"
## [17] "Balance of Payments Manual in use"
## [18] "External debt Reporting status"
## [19] "System of trade"
## [20] "Government Accounting concept"
## [21] "IMF data dissemination standard"
## [22] "Source of most recent Income and expenditure data"
## [23] "Vital registration complete"
## [24] "Latest agricultural census"
## [25] "Latest industrial data"
## [26] "Latest trade data"
## [27] "Latest water withdrawal data"
## [28] "2-alpha code"
## [29] "WB-2 code"
## [30] "Table Name"
## [31] "Short Name"


    ## Match the data based on the country shortcode. How many of the IDs
    ## match?
    setkey(q3data[[1]], CountryCode);
    setkey(q3data[[2]], CountryCode);
    result1 <-
        length(intersect(q3data[[1]]$CountryCode, q3data[[2]]$CountryCode));


    ## Sort the data frame in descending order by GDP rank (so United
    ## States is last). What is the 13th country in the resulting data frame?
    setkey(q3data[[1]], Ranking);
    require(plyr)
    temp <- arrange(q3data[[1]], desc(Ranking))
    result2 <- temp[13];
    ## CountryCode Ranking             Economy millionsUS$
    ## 1:         KNA     178 St. Kitts and Nevis        767
    ## cat("\nResult 2: 13th country is = ",  result2$Economy, "\n");

    cat("\nq3.solution1: ", result1, " ; ", result2$Economy, "\n");

    ## Result 1 =  189 matches.
    ## Result 2: 13th country is =  St. Kitts and Nevis

}

## -----------------------------------------------------------------------------

q4 <- function()
{
    q4data <- q3_get_data();
    gdp <- q4data[[1]]
    edstat <- q4data[[2]]
    setkey(edstat, "Income.Group");

    ## What is the average GDP ranking for the "High income: OECD" and "High
    ## income: nonOECD" group?  23, 45

    ## unique(edstat$Income.Group)
    ## [1] ""                     "High income: OECD"    "High income: nonOECD"
    ## [4] "Low income"           "Lower middle income"  "Upper middle income"

    #join into one data frame
    joinq4 <- join(x=gdp, y=edstat, type="inner");
    ## > which(joinq4$Income.Group=="High income: OECD")
    ##  [1] 1 3 4 5 6 9 11 12 13 15 18 20 21 23 24 25 27 33 40
    ## [20] 42 43 45 46 51 55 58 63 74 80 122
    ## sum(joinq4$Income.Group=="High income: OECD") # 30

    split_by_income <- split(joinq4, joinq4$Income.Group)
    ## names(split_by_income)
    ## [1] "High income: nonOECD" "High income: OECD"    "Low income"
    ## [4] "Lower middle income"  "Upper middle income"

    tidy.names <- make.names(names(split_by_income), unique=TRUE);
    names(split_by_income) <- tidy.names;
    ## names(split_by_income)
    ## [1] "High.income..nonOECD" "High.income..OECD"    "Low.income"
    ## [4] "Lower.middle.income"  "Upper.middle.income"
    mean_ranking <- function(x) { mean(x$Ranking, na.rm=TRUE); }
    result <- lapply(split_by_income, mean_ranking);

    ## $High.income..nonOECD
    ## [1] 91.91304

    ## $High.income..OECD
    ## [1] 32.96667

    ## $Low.income
    ## [1] 133.7297

    ## $Lower.middle.income
    ## [1] 107.7037

    ## $Upper.middle.income
    ## [1] 92.13333

    cat("\nq4.solution: (OECD, nonOECD): ",
        result$High.income..OECD, ", ",
        result$High.income..nonOECD, "\n");

    return(lapply(split_by_income, mean_ranking));

}

## -----------------------------------------------------------------------------

q5 <- function()
{
    require(Hmisc);

    q5data <- q3_get_data();
    gdp <- q5data[[1]];
    edstat <- q5data[[2]];
    ## setkey(edstat, "Income.Group");
    joinq5 <- join(x=gdp, y=edstat, type="inner");

    ## 1) Cut the GDP ranking into 5 separate quantile groups.
    joinq5$Ranking.quantiles <- cut2(joinq5$Ranking, g=5);


    ## 2) Make a table versus Income.Group.
    xt <- xtabs(~ mydata$Ranking.quantiles + Income.Group, data = mydata);
##      Income.Group
## mydata$Ranking.quantiles High income: nonOECD High income: OECD Low income
##                [  1, 39)                    4                18          0
##                [ 39, 77)                    5                10          1
##                [ 77,115)                    8                 1          9
##                [115,154)                    5                 1         16
##                [154,190]                    1                 0         11
##                         Income.Group
## mydata$Ranking.quantiles Lower middle income Upper middle income
##                [  1, 39)                   5                  11
##                [ 39, 77)                  13                   9
##                [ 77,115)                  12                   8
##                [115,154)                   8                   8
##                [154,190]                  16                   9

    ## xt_table <- ftable(xt);
    ## return(xt_table);


    q5_solution <- 5;
    cat("\nq5.solution: ", q5_solution, "\n");

}

## -----------------------------------------------------------------------------

solve <- function()
{
    q1();
    q2();
    q3();
    q4();
    q5();


## q1.solution:  125 238 262

## q2.solution:  -15259150 -10575416

## q3.solution1:  189  ;  St. Kitts and Nevis

## q4.solution = (OECD, nonOECD): 32.96667, 91.91304

## q5.solution:  5

}
