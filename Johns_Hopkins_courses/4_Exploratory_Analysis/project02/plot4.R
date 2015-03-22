
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
## WARNING: it has been detected a different number of measures per sector each
## year. We could consider to use either the total amount of emissions, or the
## relative frequency (sum(emissions)/count(emissions)). I have decide to plot
## both measures to give all the information to the viewer.
## The code to plot each data is inside a separate function "plot_total" and
## "plot_relativefreq".
##
## a) Select only "coal combustion-related sources" data
##
## coalsources <-
##     grep("coal",levels(SCC$EI.Sector), ignore.case=TRUE, value=TRUE);
##
## b) filter the data related to coal in SCC
##
## coalSCC <- SCC %>% filter(EI.Sector %in% coalsources) %>%
##     select(SCC, EI.Sector);
##
## c) From NEI, filter the data related to Emissions due to coal for the USA.
##
## usa_coal_emissions <- factoredNEI  %>% filter(SCC %in% coalSCC$SCC) %>%
##         select(SCC, Emissions, year);
## usa_coal_emissions <- merge(usa_coal_emissions, coalSCC);
## usa_coal_total_emissions <-
##     summarize(group_by(usa_coal_emissions, year, EI.Sector),
##               Emissions=sum(Emissions));
##
## usa_coal_relativefreq_emissions <-
##     summarize(group_by(usa_coal_emissions, year, EI.Sector),
##               total_emissions=sum(Emissions), count_emissions=n(),
##               relativefreq=total_emissions/count_emissions);
##
##
## 3) Plot the filtered data:(data ~ coal source & year)
##
## p1 <- plot_total(usa_coal_total_emissions, DEBUG_MODE);
## p2 <- plot_relativefreq(usa_coal_relativefreq_emissions, DEBUG_MODE);


## -----------------------------------------------------------------------------

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

    ## b) filter the data related to coal in SCC
    coalSCC <- SCC %>% filter(EI.Sector %in% coalsources) %>%
        select(SCC, EI.Sector);

    ## c) From NEI, filter the data related to Emissions due to coal for the USA
    usa_coal_emissions <- factoredNEI  %>%
        filter(SCC %in% coalSCC$SCC) %>%
            select(SCC, Emissions, year);
    usa_coal_emissions <- merge(usa_coal_emissions, coalSCC);

    ## group by EI.Sector (coal source) & year
    ## absolute frequency
    usa_coal_total_emissions <-
        summarize(group_by(usa_coal_emissions, year, EI.Sector),
                  Emissions=sum(Emissions));
    ## relative frequency
    usa_coal_relativefreq_emissions <-
        summarize(group_by(usa_coal_emissions, year, EI.Sector),
                  total_emissions=sum(Emissions), count_emissions=n(),
                  relativefreq=total_emissions/count_emissions);
    usa_coal_relativefreq_emissions <-
        data.frame(usa_coal_relativefreq_emissions);

    ## 4) plot (emissions ~ coal source & year)
    PNGWIDTH = 480;
    PNGHEIGHT = 480;
    initial_dev <- dev.cur();
    outputname <- "./data/plot4.png";
    if(!DEBUG_MODE)
        png(file = outputname, width = PNGWIDTH, height = PNGHEIGHT);

    ## Plot
    p1 <- plot_total(usa_coal_total_emissions, DEBUG_MODE);
    p2 <- plot_relativefreq(usa_coal_relativefreq_emissions, DEBUG_MODE);
    multiplot(p1, p2, cols=1);


    ## 5) create the png file
    if(DEBUG_MODE)
        {
            print("dev.copy png");
            dev.copy(png, file = outputname,    ## Copy plot to a PNG file
                     width = PNGWIDTH, height = PNGHEIGHT);
            dev.off();                          ## close the PNG device
        }
    else
        {
            while(dev.cur()>initial_dev)
                dev.off();                          ## close the PNG device
            stopifnot(initial_dev==dev.cur());
        }
}

## -----------------------------------------------------------------------------

plot_total <- function(usa_coal_total_emissions, DEBUG_MODE)
{
    PLOTDATA=usa_coal_total_emissions;
    YVALUE=PLOTDATA$Emissions;
    XVALUE=PLOTDATA$year;
    FACETVALUE=PLOTDATA$EI.Sector;
    YLABTEXT="tons (thousands)";
    XLABTEXT="year";
    MAINTEXT=expression("U.S.A. coal-related emissions (totals) from "*PM[2.5]);
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
    if(DEBUG_MODE) print(g);
    ## print(g);
    return(g);
}

## -----------------------------------------------------------------------------

plot_relativefreq <- function(usa_coal_relativefreq_emissions, DEBUG_MODE)
{
    PLOTDATA=usa_coal_relativefreq_emissions;
    YVALUE=PLOTDATA$relativefreq;
    XVALUE=PLOTDATA$year;
    FACETVALUE=PLOTDATA$EI.Sector;
    YLABTEXT="tons";
    XLABTEXT="year";
    MAINTEXT=expression("U.S.A. coal-related emissions (relative frequency) from "* PM[2.5]);
    PTCOLOR="red";
    SHAPEVALUE=20;

    g <- ggplot(data = usa_coal_relativefreq_emissions,
                aes(year, relativefreq, group = EI.Sector,
                    label=round(relativefreq, 2)))+
        geom_point(colour=PTCOLOR, shape=SHAPEVALUE) +
        geom_line() +
        facet_grid(.~EI.Sector, labeller = facets_source_labeller) +
        ggtitle(MAINTEXT) +
        xlab(XLABTEXT) +
        ylab(YLABTEXT) +
            theme_bw();
    if(DEBUG_MODE) print(g);
    g <- g + geom_text(size = rel(3.0), angle=45, hjust=-0.25, vjust=-0.25);
    if(DEBUG_MODE) print(g);

    ## Complete the y axis
    YAXIS_ROUNDFACTOR <- 1; #default range
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
    if(DEBUG_MODE) print(g);
    ## print(g);
    return(g);
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

## Multiple plot function
##
## ggplot objects can be passed in ..., or to plotlist (as a list of ggplot
## objects) - cols: Number of columns in layout - layout: A matrix specifying
## the layout. If present, 'cols' is ignored.
##
## If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then
## plot 1 will go in the upper left, 2 will go in the upper right, and 3 will
## go all the way across the bottom.
##
## www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29.html
##

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL){

  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## -----------------------------------------------------------------------------
