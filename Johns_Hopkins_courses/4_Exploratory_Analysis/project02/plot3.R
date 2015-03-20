
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
##
## baltimore <- factoredNEI %>%
##     filter(fips=="24510") %>%
##         select(Emissions, type, year);
##
## 2) Subset the filtered data by type of emissions (point, nonpoint, onroad,
## nonroad) per year.
##
## baltimore_totals_df <- as.data.frame
## (xtabs(Emissions ~ year + type , data = group_by(baltimore, year)));
## year     type       Freq
## 1  1999 NON-ROAD  522.94000
## ...
## 16 2008    POINT  344.97518
##
## 3) Plot Freq ~ year + type
##
## Note.- the next 2 functions plots the same graphics
## do_qplot(baltimore_totals_df, DEBUG_MODE);     # (see code bellow)
## do_ggplot(baltimore_totals_df, DEBUG_MODE); # (see code bellow)


require(dplyr)
require(ggplot2);

## -----------------------------------------------------------------------------

plot3 <- function(DEBUG_MODE=FALSE)
{
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

    baltimore_emissions_by_type_and_year <-
        xtabs(Emissions ~ year + type , data = group_by(baltimore, year));

    baltimore_totals_df <- as.data.frame(baltimore_emissions_by_type_and_year)
    ## 1  1999 NON-ROAD  522.94000
    ## 16 2008    POINT  344.97518
    stopifnot(sum(baltimore$Emissions) == sum(baltimore_totals_df$Freq));

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


    ## Plot
    ## Note.- the next 2 functions plots the same graphics
    ## do_qplot(baltimore_totals_df, DEBUG_MODE);     # (see code bellow)
    do_ggplot(baltimore_totals_df, DEBUG_MODE); # (see code bellow)


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

do_ggplot <- function(baltimore_totals_df, DEBUG_MODE)
{
    YLABTEXT="tons (thousands)";
    XLABTEXT="year";
    MAINTEXT=expression("Baltimore City emissions from "* PM[2.5]);

    ## Plot
    PTCOLOR="red";
    SHAPEVALUE=20;
    g <- ggplot(data = baltimore_totals_df,
                aes(year, Freq, group = type)) +
                    geom_point(colour=PTCOLOR, shape=SHAPEVALUE) +
                        geom_line();
    print(g);
    ## facets
    g <- g + facet_grid(.~type, labeller = facets_source_labeller);
    if(DEBUG_MODE) print(g);

    ## layout theme
    g <- g + theme_bw();
    if(DEBUG_MODE) print(g);


    ## Complete the title
    g <- g + ggtitle(MAINTEXT); if(DEBUG_MODE) print(g);

    ## ## Complete the x axis
    g <- g + xlab(XLABTEXT); if(DEBUG_MODE) print(g);

    ## ## Complete the y axis
    g <- g + ylab(YLABTEXT); if(DEBUG_MODE) print(g);
    YAXIS_ROUNDFACTOR <- 1000; #thousands
    yceiling <- ceiling(max(baltimore_totals_df$Freq)/YAXIS_ROUNDFACTOR) *
        YAXIS_ROUNDFACTOR;
    aty <- c(seq(0, yceiling, yceiling/10.0));
    g <- g + scale_y_continuous(limits=c(0.0, yceiling));
    if(DEBUG_MODE) print(g);
    g <- g + scale_y_continuous(breaks=aty, labels=aty/YAXIS_ROUNDFACTOR);

    print(g);
}

## -----------------------------------------------------------------------------

do_qplot <- function(baltimore_totals_df, DEBUG_MODE)
{
    YLABTEXT="tons (thousands)";
    XLABTEXT="year";
    MAINTEXT=expression("Baltimore City emissions from "* PM[2.5]);

    ## Plot
    PTCOLOR="red";
    SHAPEVALUE=20;

    g <- qplot(year, Freq, data = baltimore_totals_df,
               margins = FALSE,
               group = type,
               geom = c("point", "line"),
               main = "",
               xlab = "",
               ylab = "");
               ## facets = . ~ type);

    ## only necessary if color & shape are desired
    g <- g + geom_point(colour=PTCOLOR, shape=SHAPEVALUE);

    ## facets
    g <- g + facet_grid(.~type, labeller = facets_source_labeller);

    ## layout theme
    g <- g + theme_bw();

    print(g);

    ## Complete the title
    ## MAINTITLE = "Baltimore City emissions from  PM2.5";
    ## MAINSUB = "\n(per source of emission)";
    ## MAINTEXT = paste(MAINTITLE, MAINSUB, sep="");
    g <- g + ggtitle(MAINTEXT); if(DEBUG_MODE) print(g);

    ## Complete the x axis
    g <- g + xlab(XLABTEXT); if(DEBUG_MODE) print(g);

    ## Complete the y axis
    g <- g + ylab(YLABTEXT); if(DEBUG_MODE) print(g);
    YAXIS_ROUNDFACTOR <- 1000; #thousands
    yceiling <- ceiling(max(baltimore_totals_df$Freq)/YAXIS_ROUNDFACTOR) *
        YAXIS_ROUNDFACTOR;
    aty <- c(seq(0, yceiling, yceiling/10.0));
    g <- g + scale_y_continuous(limits=c(0.0, yceiling));
    if(DEBUG_MODE) print(g);
    g <- g + scale_y_continuous(breaks=aty, labels=aty/YAXIS_ROUNDFACTOR);

    print(g);
}


## -----------------------------------------------------------------------------

##\info: "mf_labeller" at http://www.cookbook-r.com/Graphs/Facets_%28ggplot2%29/
facets_source_labeller <- function(var, value){
    value <- as.character(value)
    if (var=="type") {
        value[value=="NON-ROAD"] <- "non-road source";
        value[value=="ON-ROAD"] <- "on-road source"
        value[value=="NONPOINT"] <- "non-point source";
        value[value=="POINT"] <- "point source"
    }
    return(value)
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

