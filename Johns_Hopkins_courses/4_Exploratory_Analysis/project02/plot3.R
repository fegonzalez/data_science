
## PROBLEM (plot3)
##
## Of the four types of sources indicated by the type (point, nonpoint, onroad,
## nonroad) variable, which of these four sources have seen decreases in
## emissions from 1999–2008 for Baltimore City? Which have seen increases in
## emissions from 1999–2008? Use the ggplot2 plotting system to make a plot
## answer this question.
##
##
## SOLUTION DECISION
##
## 1) filter the NEI source data to take only the observations from the city of
##    Baltimore (NEI$fips=="24510")
##
## baltimore <- filter(factoredNEI, fips=="24510");
##
## 2) Subset the filtered data by type of emissions (point, nonpoint, onroad,
## nonroad) per year.
##
## by_year <- group_by(baltimore, year);

##
## 3) Plot total_emissions ~ year
## \notice: Using stripchart() (base plotting system) instead of plot() to get
## points instead of bars when plotting over the factored variable "year".


require(graphics);
require(grDevices);
require(dplyr)

## -----------------------------------------------------------------------------

plot3 <- function(DEBUG_MODE=FALSE)
{
    DEBUG_MODE=TRUE;
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

    baltimore <- factoredNEI %>%
        filter(fips=="24510") %>%
            select(Emissions, type, year);
    ## str(baltimore)
    ## 'data.frame':	2096 obs. of  3 variables:


    baltimore_emissions_by_type_and_year <-
        xtabs(Emissions ~ year + type , data = group_by(baltimore, year));
    ##       type
    ## year     NON-ROAD   NONPOINT    ON-ROAD      POINT
    ##   1999  522.94000 2107.62500  346.82000  296.79500
    ##   2002  240.84692 1509.50000  134.30882  569.26000
    ##   2005  248.93369 1509.50000  130.43038 1202.49000
    ##   2008   55.82356 1373.20731   88.27546  344.97518


    ## stopifnot(sum(baltimore$Emissions) ==
    ##           sum(baltimore_emissions_by_type_and_year$point_emissions) +
    ##           sum(baltimore_emissions_by_type_and_year$nonroad_emissions) +
    ##           sum(baltimore_emissions_by_type_and_year$nonpoint_emissions) +
    ##           sum(baltimore_emissions_by_type_and_year$onroad_emissions));


    ## EQUIVALE A TODO ESTO:

    baltimore_emissions_by_type_and_year_dplyr<-calculate_with_dplyr(baltimore);

    class(baltimore_emissions_by_type_and_year)
    ## [1] "xtabs" "table"
    class(baltimore_emissions_by_type_and_year_dplyr)
    ## [1] "tbl_df"     "tbl"        "data.frame"



    ## 4) plot
    initial_dev <- dev.cur();
    outputname <- "./data/plot3.png";
    if(!DEBUG_MODE)
    {
        png(file = outputname, width = 480, height = 480);
    }

    ## Setup the plot area
    op <- par(mfrow = c(1, 1),
              bg="white",
              las=0);

    YLABTEXT="tons (thousands)";
    XLABTEXT="year";
    MAINTEXT="Baltimore City emissions from PM2.5";

    ## Plot a blank graph
    ## warning: plot() creates a boxplot instead of a point when using factor
    ## variables. Using stripchart instead.
    XVALUE <- baltimore_emissions_by_type_and_year$year;
    YVALUE <- baltimore_emissions_by_type_and_year$point_emissions;
    stripchart(YVALUE ~ XVALUE,
               xlab = "",
               ylab = "",
               yaxt = "n",
               vertical = TRUE,
               type="n",
               data = baltimore_emissions_by_type_and_year);
    mtext(side=3, text=MAINTEXT, line=1.0, cex=1.6);

    ## Complete the x axis
    mtext(side=1, text=XLABTEXT, line=2.5, cex=1.0);

    ## Complete the y axis
    YAXIS_ROUNDFACTOR <- 1000; #thousands
    yceiling <-  max(baltimore_emissions_by_type_and_year$point_emissions,
                     baltimore_emissions_by_type_and_year$nonpoint_emissions,
                     baltimore_emissions_by_type_and_year$onroad_emissions,
                     baltimore_emissions_by_type_and_year$nonroad_emissions,);
    yceiling <- ceiling(yceiling/YAXIS_ROUNDFACTOR) * YAXIS_ROUNDFACTOR;
    aty <- c(seq(0, yceiling, yceiling/10.0));
    axis(side=2,
         at=aty,
         ## labels=format(aty/YAXIS_ROUNDFACTOR, scientific=FALSE),
         las=2);
    mtext(side=2, text=YLABTEXT, line=2.8, cex=1.0);

    grid();

    ## Plot content. Last to add to be on top of the grid (if grid exists)
    XVALUE <- baltimore_emissions_by_type_and_year$year;
    YVALUE <- baltimore_emissions_by_type_and_year$point_emissions;
    LINE_LWDSIZE=1;
    LINECOLOR="black";
    PT_LWDSIZE=1;
    PTCOLOR="red";
    points(x=XVALUE, y=YVALUE, lwd = PT_LWDSIZE, col=PTCOLOR, pch = 20);
    lines(x=XVALUE, y=YVALUE, lwd = LINE_LWDSIZE, col=LINECOLOR);

    XVALUE <- baltimore_emissions_by_type_and_year$year;
    YVALUE <- baltimore_emissions_by_type_and_year$nonpoint_emissions;
    LINE_LWDSIZE=1;
    LINECOLOR="blue";
    PT_LWDSIZE=1;
    PTCOLOR="red";
    points(x=XVALUE, y=YVALUE, lwd = PT_LWDSIZE, col=PTCOLOR, pch = 20);
    lines(x=XVALUE, y=YVALUE, lwd = LINE_LWDSIZE, col=LINECOLOR);

    XVALUE <- baltimore_emissions_by_type_and_year$year;
    YVALUE <- baltimore_emissions_by_type_and_year$onroad_emissions;
    LINE_LWDSIZE=1;
    LINECOLOR="green";
    PT_LWDSIZE=1;
    PTCOLOR="red";
    points(x=XVALUE, y=YVALUE, lwd = PT_LWDSIZE, col=PTCOLOR, pch = 20);
    lines(x=XVALUE, y=YVALUE, lwd = LINE_LWDSIZE, col=LINECOLOR);

    XVALUE <- baltimore_emissions_by_type_and_year$year;
    YVALUE <- baltimore_emissions_by_type_and_year$nonroad_emissions;
    LINE_LWDSIZE=1;
    LINECOLOR="yellow";
    PT_LWDSIZE=1;
    PTCOLOR="red";
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


calculate_with_dplyr <- function(baltimore)
{
    ## type = POINT `per year
    point_byyear <- baltimore %>%
        filter(type=="POINT") %>%
            group_by(year) %>%
                summarize(point_emissions=sum(Emissions));

    nonroad_byyear <- baltimore %>%
        filter(type=="NON-ROAD") %>%
            group_by(year) %>%
                summarize(nonroad_emissions=sum(Emissions));

    nonpoint_byyear <- baltimore %>%
        filter(type=="NONPOINT") %>%
            group_by(year) %>%
                summarize(nonpoint_emissions=sum(Emissions));

    onroad_byyear <- baltimore %>%
        filter(type=="ON-ROAD") %>%
            group_by(year) %>%
                summarize(onroad_emissions=sum(Emissions));

    baltimore_emissions_by_type_and_year <-
        dplyr::inner_join(point_byyear, nonpoint_byyear, by="year");
    baltimore_emissions_by_type_and_year <-
        dplyr::inner_join(baltimore_emissions_by_type_and_year,
                          onroad_byyear, by="year");
    baltimore_emissions_by_type_and_year <-
        dplyr::inner_join(baltimore_emissions_by_type_and_year,
                          nonroad_byyear, by="year");

    stopifnot(sum(baltimore$Emissions) ==
              sum(baltimore_emissions_by_type_and_year$point_emissions) +
              sum(baltimore_emissions_by_type_and_year$nonroad_emissions) +
              sum(baltimore_emissions_by_type_and_year$nonpoint_emissions) +
              sum(baltimore_emissions_by_type_and_year$onroad_emissions));

##   year point_emissions nonpoint_emissions onroad_emissions nonroad_emissions
## 1 1999        296.7950           2107.625        346.82000         522.94000
## 2 2002        569.2600           1509.500        134.30882         240.84692
## 3 2005       1202.4900           1509.500        130.43038         248.93369
## 4 2008        344.9752           1373.207         88.27546          55.82356

    return (baltimore_emissions_by_type_and_year);
}
