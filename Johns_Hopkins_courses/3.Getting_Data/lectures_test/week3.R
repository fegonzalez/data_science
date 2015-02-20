
Subsetting_and_sorting <- function()
{
    set.seed(13435)
    X <- data.frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
    X <- X[sample(1:5),];
    X$var2[c(1,3)] = NA;
    X
    X[(X$var1 <= 3 & X$var3 > 11),]
}

## -----------------------------------------------------------------------------

summarizing_data_getfile <- function()
{
    if(!file.exists("./data")){dir.create("./data")}

    if(!file.exists("./data/restaurants.csv"))
    {
        fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
        download.file(fileUrl,destfile="./data/restaurants.csv",method="curl")
    }
    return  (read.csv("./data/restaurants.csv"));
}

## -----------------------------------------------------------------------------

peer_review_data <- function()
{
    if(!file.exists("./data")){dir.create("./data")}
    fileUrl1 =
        "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
    fileUrl2 =
        "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
    if(!file.exists("./data/reviews.csv"))
        download.file(fileUrl1,destfile="./data/reviews.csv",method="curl")
    if(!file.exists("./data/solutions.csv"))
        download.file(fileUrl2,destfile="./data/solutions.csv",method="curl")
    ## reviews <- read.csv("./data/reviews.csv");
    ## solutions <- read.csv("./data/solutions.csv")
    ## head(reviews,2)
}

## -----------------------------------------------------------------------------
