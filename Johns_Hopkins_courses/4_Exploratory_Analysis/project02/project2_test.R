## 2) Test list

## - plot_all(): plot all the plots

## - test_all(): test the complete exercise tasks

##     i)   remove all the source data
##     ii)  remove all the output png files
##     iii) plot all the plots


source("plot1.R");
source("plot2.R");
source("plot3.R");
## source("plot4.R");
## source("plot5.R");
## source("plot6.R");

plot_all <- function()
{
    rm(list=ls());
    plot1();
    plot2();
    plot3();
    ## plot4();
    ## plot5();
    ## plot6();
}

test_all <- function()
{
    if (file.exists("./data/plot1.png")) file.remove("plot1.png");
    if (file.exists("./data/plot2.png")) file.remove("plot2.png");
    if (file.exists("./data/plot3.png")) file.remove("plot3.png");
    if (file.exists("./data/plot4.png")) file.remove("plot4.png");
    if (file.exists("./data/plot5.png")) file.remove("plot5.png");
    if (file.exists("./data/plot6.png")) file.remove("plot6.png");


    if (file.exists("./data"))
        unlink("./data", recursive = TRUE, force = TRUE);
    plot_all();
}

