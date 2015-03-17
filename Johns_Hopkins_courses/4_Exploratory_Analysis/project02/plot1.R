
## PROBLEM (plot1)
##
## Have total emissions from PM2.5 decreased in the United States from 1999 to
## 2008?
## Using the base plotting system, make a plot showing the total PM2.5 emission
## from all sources for each of the years 1999, 2002, 2005, and 2008.
##
## SOLUTION DECISION
##
## 1) Make a subset of the NEI source data by total emissions per year. Using
## variables as factors to be year-specify.
##
##  by_year <- group_by(factoredNEI, year);
##  total_emissions_by_year<-summarize(by_year, total_emissions=sum(Emissions))
##  The table result is:
##  1 1999         7332967
##  2 2002         5635780
##  3 2005         5454703
##  4 2008         3464206
##
## 2) Plot total_emissions ~ year
## \notice: Using stripchart() (base plotting system) instead of plot() to get
## points instead of bars when plotting over the factored variable "year".


require(graphics);
require(grDevices);
require(dplyr)

## -----------------------------------------------------------------------------

plot1 <- function(DEBUG_MODE=FALSE)
{
    ## DEBUG_MODE=TRUE;

    ## 1) download & unzip if necessary (common code, see at the end)
    get_source_data();

    ## 2) load processes data into memory (common code)
    SOURCE_DATA_NEI<- "./data/summarySCC_PM25.rds";
    SOURCE_DATA_SCC<- "./data/Source_Classification_Code.rds";
    SCC <- readRDS(SOURCE_DATA_SCC)
    ## Loading NEI will likely take a few seconds (around 45 seconds)
    if(!exists("NEI")){
        if(!DEBUG_MODE)
            NEI <- readRDS(SOURCE_DATA_NEI)
        else
            system.time(NEI <- readRDS(SOURCE_DATA_NEI));
    }

    ## 3) prepare the data to plot. Using factors to be year-specify
    factoredNEI <- mutate(NEI, year=as.factor(year));
    factoredNEI <- mutate(factoredNEI, type=as.factor(type));
    by_year <- group_by(factoredNEI, year);
    total_emissions_by_year<-summarize(by_year, total_emissions=sum(Emissions));
    ## year total_emissions
    ## 1 1999         7332967
    ## 2 2002         5635780
    ## 3 2005         5454703
    ## 4 2008         3464206


    ## 4) plot
    initial_dev <- dev.cur();
    outputname <- "./data/plot1.png";
    if(!DEBUG_MODE)
    {
        png(file = outputname, width = 480, height = 480);
    }

    ## Setup the plot area
    op <- par(mfrow = c(1, 1),
              bg="white",
              las=0);

    XVALUE <- (total_emissions_by_year$year);
    YVALUE <- total_emissions_by_year$total_emissions;
    LINE_LWDSIZE=1;
    LINECOLOR="black";
    PT_LWDSIZE=1;
    PTCOLOR="red";
    YLABTEXT="tons (millions)";
    XLABTEXT="year";
    MAINTEXT="United States total emissions from PM2.5";

    ## Plot a blank graph
    ## warning: plot() creates a boxplot instead of a point when using factor
    ## variables. Using stripchart instead.
    stripchart(total_emissions ~ year,
               xlab = "",
               ylab = "",
               yaxt = "n",
               vertical = TRUE,
               type="n",
               data = total_emissions_by_year);
    mtext(side=3, text=MAINTEXT, line=1.0, cex=1.6);

    #Complete the x axis
    mtext(side=1, text=XLABTEXT, line=2.5, cex=1.0);

    #Complete the y axis
    aty <- c(round(range(total_emissions_by_year$total_emissions)/1000000));
    aty <- seq(aty[1], aty[length(aty)]);
    aty <- aty * 1000000; ## [1] 3 4 5 6 7  (millions)
    axis(side=2, at=aty,
         labels=format(aty/1000000, scientific=FALSE),
         las=2);
    mtext(side=2, text=YLABTEXT, line=2.5, cex=1.0);

    ## grid();

    ## Plot content. Last to add to be on top of the grid (if grid exists)
    points(x=XVALUE, y=YVALUE, lwd = PT_LWDSIZE, col=PTCOLOR, pch = 20);
    lines(x=XVALUE, y=YVALUE, lwd = LINE_LWDSIZE, col=LINECOLOR);


    ## 5) create the png file
    if(DEBUG_MODE)
    {
        print("dev.copy png");
        dev.copy(png, file = outputname,    ## Copy plot to a PNG file
             width = 480, height = 480);
        dev.off();                          ## close the PNG device
        par(op); ## At end of plotting, reset to previous settings:
    }
    else
    {
        dev.off();                          ## close the PNG device
        par(op); ## At end of plotting, reset to previous settings:
        dev.off();                          ## close screen device
        stopifnot(initial_dev==dev.cur());
    }
}

## -----------------------------------------------------------------------------

##\function get_source_data()
## Given "RWW" as the current R working directory, this function download,
## unzip and stores the source data ready to be used.
get_source_data<- function()
{
    SOURCE_DATA1_NEI <- "./data/summarySCC_PM25.rds";
    SOURCE_DATA_SCC <- "./data/Source_Classification_Code.rds";
    if((file.exists(SOURCE_DATA1_NEI))&&(file.exists(SOURCE_DATA_SCC)))
        return(0);
    if(!file.exists("./data")){dir.create("./data")}
    dest_file_name <- "exdata_data_NEI_data.zip";
    dest_file_path <- paste("./data/", dest_file_name, sep="");
    file_url <-
        "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    dest_method <- "curl";

    ## download
    if(!file.exists(dest_file_path))
    {
        download.file(file_url, destfile=dest_file_path, method=dest_method);
    }

    ## unzip
    if((!file.exists(SOURCE_DATA1_NEI))||(!file.exists(SOURCE_DATA_SCC)))
    {
        require(utils);
        unzip(dest_file_path, exdir="./data/");
    }
    stopifnot(file.exists(SOURCE_DATA1_NEI));
    stopifnot(file.exists(SOURCE_DATA_SCC));
}

## -----------------------------------------------------------------------------
