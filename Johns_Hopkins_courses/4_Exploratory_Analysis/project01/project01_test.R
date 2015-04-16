## 2) Test list

## - plot_all(): plot all the plots

## - test_all(): test the complete exercise tasks

##     i)   remove all the source data
##     ii)  remove all the output png files
##     iii) plot all the plots


source("plot1.R");
source("plot2.R");
source("plot3.R");
source("plot4.R");

plot_all <- function()
{
    plot1();
    plot2();
    plot3();
    plot4();
}

test_all <- function()
{
    if (file.exists("plot1.png")) file.remove("plot1.png");
    if (file.exists("plot2.png")) file.remove("plot2.png");
    if (file.exists("plot3.png")) file.remove("plot3.png");
    if (file.exists("plot4.png")) file.remove("plot4.png");

    if (file.exists("./data"))
        unlink("./data", recursive = TRUE, force = TRUE);
    plot_all();
}

