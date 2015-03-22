
## PROBLEM (plot4)
##
## Across the United States, how have emissions from coal combustion-related
## sources changed from 1999–2008?
##
##
## SOLUTION DECISION
##
## 1) Which are "coal combustion-related sources", let's call it' 'coalsources'
##    (non-automatic research)
##
##    a) Looking for what variables in SCC can provide information related to
##       "coal". I created the function "search_coal" for this.
##
##       search_coal(SCC)
##       [1] "Short.Name"
##       [1] "EI.Sector"
##       [1] "SCC.Level.Three"
##       [1] "SCC.Level.Four"
##
##    b) Using the documentation and the data, I decided that the most useful
##       variable to select the data related to coal was "EI.Sector".
##
##       Thus, the response to decision 1) is:
##
##       coalsources <-
##            grep("coal",levels(SCC$EI.Sector), ignore.case=TRUE, value=TRUE);
##
##       [1] "Fuel Comb - Comm/Institutional - Coal"
##       [2] "Fuel Comb - Electric Generation - Coal"
##       [3] "Fuel Comb - Industrial Boilers, ICEs - Coal"
##
##
## 2) Select & prepare the data to plot
##
## a) Select only "coal combustion-related sources" data
##
## coalsources <-
##     grep("coal",levels(SCC$EI.Sector), ignore.case=TRUE, value=TRUE);
##
## b) filter the data related to coal in SCC (only SCC codes needed)
##
## coalSCC <- SCC %>% filter(EI.Sector %in% coalsources) %>%
##     select(SCC, EI.Sector);
##
## c) From NEI, filter the data related to total Emissions due to coal for the
##    USA.
##
## usa_coal_emissions <- factoredNEI  %>% filter(SCC %in% coalSCC$SCC) %>%
##         select(SCC, Emissions, year);
## usa_coal_emissions <- merge(usa_coal_emissions, coalSCC);
## usa_coal_total_emissions <-
##     summarize(group_by(usa_coal_emissions, year, EI.Sector),
##               Emissions=sum(Emissions));
##
## 3) Plot the filtered data:(total missions ~ coal source & year)
##


require(dplyr)
require(ggplot2);

## -----------------------------------------------------------------------------

plot4 <- function(DEBUG_MODE=FALSE)
{
    ## 1) download & unzip if necessary (common code, see at the end)
    get_source_data();

    ## 2) load processes data into memory (common code)
    SOURCE_DATA_NEI<- "./data/summarySCC_PM25.rds";
    SOURCE_DATA_SCC<- "./data/Source_Classification_Code.rds";
    SCC <- readRDS(SOURCE_DATA_SCC)
    ## Loading NEI will likely take a few seconds (around 45 seconds)
    if(!exists("NEI")){
        NEI <- readRDS(SOURCE_DATA_NEI)
    }


    ## 3) prepare the data to plot.
    factoredNEI <- mutate(NEI, year=as.factor(year));
    factoredNEI <- mutate(factoredNEI, type=as.factor(type));
    factoredNEI <- mutate(factoredNEI, SCC=as.factor(SCC));

    ## a) Select only "coal combustion-related sources" data
    coalsources <-
        grep("coal",levels(SCC$EI.Sector), ignore.case=TRUE, value=TRUE);

    ## b) filter the data related to coal in SCC (only SCC codes needed)
    coalSCC <- SCC %>% filter(EI.Sector %in% coalsources) %>%
        select(SCC, EI.Sector);

    ## c) From NEI, filter the data related to Emissions due to coal for the USA
    usa_coal_emissions <- factoredNEI  %>%
        filter(SCC %in% coalSCC$SCC) %>%
            select(SCC, Emissions, year);
    usa_coal_emissions <- merge(usa_coal_emissions, coalSCC);
    ## group by EI.Sector (coal source) & year
    usa_coal_total_emissions <-
        summarize(group_by(usa_coal_emissions, year, EI.Sector),
                  Emissions=sum(Emissions));
    ## Source: local data frame [12 x 3]
    ## Groups: year
    ##    year                                   EI.Sector  Emissions
    ## 1  1999       Fuel Comb - Comm/Institutional - Coal  10617.830
    ## ...
    ## 12 2008 Fuel Comb - Industrial Boilers, ICEs - Coal  24053.602


    ## 4) plot (emissions ~ coal source & year)
    initial_dev <- dev.cur();
    outputname <- "./data/plot4.png";
    if(!DEBUG_MODE)
        png(file = outputname, width = 480, height = 480);
    ## Setup the plot area
    op <- par(mfrow = c(1, 1),
              bg="white",
              las=0);

    ## Plot
    PLOTDATA=usa_coal_total_emissions;
    YVALUE=PLOTDATA$Emissions;
    XVALUE=PLOTDATA$year;
    FACETVALUE=PLOTDATA$EI.Sector;
    YLABTEXT="tons (thousands)";
    XLABTEXT="year";
    MAINTEXT=
        expression("U.S.A. coal combustion-related emissions from "* PM[2.5]);
    PTCOLOR="red";
    SHAPEVALUE=20;

    g <- ggplot(data = usa_coal_total_emissions,
                aes(year, Emissions, group = EI.Sector)) +
        geom_point(colour=PTCOLOR, shape=SHAPEVALUE) +
        geom_line() +
        facet_grid(.~EI.Sector, labeller = facets_source_labeller) +
        ggtitle(MAINTEXT) +
        xlab(XLABTEXT) +
        ylab(YLABTEXT) +
            theme_bw();
    if(DEBUG_MODE) print(g);
    ## Complete the y axis
    YAXIS_ROUNDFACTOR <- 1000; #thousands
    yceiling <- ceiling(max(YVALUE)/YAXIS_ROUNDFACTOR) * YAXIS_ROUNDFACTOR;
    yfloor <- floor(min(YVALUE)/YAXIS_ROUNDFACTOR) * YAXIS_ROUNDFACTOR;
    aty <- c(seq(yfloor, yceiling+YAXIS_ROUNDFACTOR, yceiling/10.0));
    aty <- c(aty, yceiling);
    g <- g + scale_y_continuous(limits=range(YVALUE),
                                breaks=aty, labels=aty/YAXIS_ROUNDFACTOR);
    if(DEBUG_MODE) print(g);
    ## Complete plot sizes (http://docs.ggplot2.org/0.9.2.1/theme.html)
    g <- g + theme(plot.title = element_text(size = rel(1.05))) +
        theme(axis.text.x = element_text(size = rel(0.85))) +
        theme(axis.text.y = element_text(size = rel(0.85))) +
        theme(axis.title.x=element_text(size=rel(0.85), angle=0, vjust = 0.5)) +
        theme(axis.title.y=element_text(size=rel(0.85), angle=90, vjust=1.5)) +
        theme(strip.text.x = element_text(angle=0, size=rel(0.9),
              hjust = 0.5, vjust = 0.5))
    print(g);

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

##\info: "mf_labeller" at http://www.cookbook-r.com/Graphs/Facets_%28ggplot2%29/
facets_source_labeller <- function(var, value){
    value <- as.character(value);
    if (var=="EI.Sector") {
        value[value=="Fuel Comb - Comm/Institutional - Coal"] <-
            "Comm/Institucional coal";
        value[value=="Fuel Comb - Electric Generation - Coal"] <-
            "Electric Generation coal"
        value[value=="Fuel Comb - Industrial Boilers, ICEs - Coal"] <-
            "Industrial coal";
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

## Looking for what variables in SCC can provide information related to "coal".
search_coal <- function(data)
{
    ##browser();
    for (label in names(data)){
        ## print(label);
        ## print(levels(data[[label]]));
        result <- grep("coal",
                       levels(data[[label]]), ignore.case=TRUE, value=TRUE);
        if(length(result))
            print(label);
    }
}

## -----------------------------------------------------------------------------
