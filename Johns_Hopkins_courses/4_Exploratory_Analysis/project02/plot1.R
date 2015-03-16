
#common source code for plot1() - plot4() functions: get_source_data(),
#load_power_data()
source("common.R");

require(graphics);
require(grDevices);
require(dplyr)

## Have total emissions from PM2.5 decreased in the United States from 1999 to
## 2008?
##
## Using the base plotting system, make a plot showing the total PM2.5 emission
## from all sources for each of the years 1999, 2002, 2005, and 2008.
plot1 <- function(DEBUG_MODE=FALSE)
{
    ## 1) download & unzip if necessary (common code)
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

    ## prepare the data to plot
    factoredNEI <- mutate(NEI, year=as.factor(year));
    factoredNEI <- mutate(factoredNEI, type=as.factor(type));
    by_year <- group_by(factoredNEI, year)
    total_emissions_by_year<-summarize(by_year, total_emissions=sum(Emissions));
    ## year total_emissions
    ## 1 1999         7332967
    ## 2 2002         5635780
    ## 3 2005         5454703
    ## 4 2008         3464206


    ## 3) plot
    initial_dev <- dev.cur();
    outputname <- "./data/plot1.png";
    if(!DEBUG_MODE)
    {
        png(file = outputname, width = 480, height = 480);
    }

    op <- par(mfrow = c(1, 1),
              ## mar = c(5, 5, 2, 1),
              ## oma = c(0, 0, 0, 0),
              bg="white",
              las=0);

    ## Create plot
    XVALUE <- total_emissions_by_year$year;
    YVALUE <- total_emissions_by_year$total_emissions;
    LWDSIZE=1;
    MYCOLOR="black";
    plot(x=XVALUE,
         y=YVALUE,
         main = "Total emissions of PM2.5",
         xlab="year",
         ylab="tons",
         type="h",
         lwd = LWDSIZE,
         col = MYCOLOR,
         ## asp=10
         );

    ## annotate
    lines(x=XVALUE, y=YVALUE, lwd = LWDSIZE, col=MYCOLOR);

    ## 4) create the png file
    if(DEBUG_MODE)
    {
        print("dev.copy png");
        dev.copy(png, file = outputname,    ## Copy plot to a PNG file
             width = 480, height = 480);
    }
    dev.off();                           ## close the PNG device
    par(op); ## At end of plotting, reset to previous settings:

    dev.off(); #close screen device
    stopifnot(initial_dev==dev.cur());
}

## -----------------------------------------------------------------------------
