
## -----------------------------------------------------------------------------

lecture21 <- function()
{
    if(!file.exists("./data")){dir.create("./data")}
    if(!file.exists("./data/cameras.csv"))
    {
        fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"

        download.file(fileUrl,destfile="./data/cameras.csv",method="curl")
    }
    ## cameraData <- read.csv("./data/cameras.csv")
    ## names(cameraData)

    fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
    fileUrl2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
    if(!file.exists("./data/reviews.csv"))
        download.file(fileUrl1,destfile="./data/reviews.csv",method="curl")
    if(!file.exists("./data/solutions.csv"))
        download.file(fileUrl2,destfile="./data/solutions.csv",method="curl")

    ## reviews <- read.csv("./data/reviews.csv"); solutions <- read.csv("./data/solutions.csv")
    ## head(reviews,2)

}

## -----------------------------------------------------------------------------

