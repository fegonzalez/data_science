
## PROBLEM (plot5)
##
## How have emissions from motor vehicle sources changed from 1999–2008 in
## Baltimore City?
##
##
## SOLUTION DECISION
##
## Precondition) Which are the "emissions from motor vehicle sources", let's
## call it' 'motorsources'.
##
##    a) Looking for what variables in SCC can provide information related to
##       "motor" sources.
##       search_source(SCC, "motor")
##       [1] "Short.Name"
##       [1] "SCC.Level.Three"
##       [1] "SCC.Level.Four"
##
##     b) Using the documentation and the search result, I have decided that
##     the most useful variable to select the data related to 'motor source'
##     was "Short.Name". Also I have decided to use source data from all
##     the types of motor vehicles (motorcycle, motor vehicle, ...)
##
##       Thus, the response to decision to this Precondition is:
##
##       motorsources <-
##           grep("motor",levels(SCC$Short.Name), ignore.case=TRUE, value=TRUE);
##
##     [1] "Highway Veh - Gasoline - Motorcycles (MC) - Collector: Urban Time 1"
##          ...
##     [138] "Surface Coating /Motor Vehicles /Total: All Solvent Types"
##
##
## 1) Select & prepare the data to plot (get_plot_data() function)
##
##    a) Select only "motor source-related sources" data
##       motorsources <-
##           grep("motor",levels(SCC$Short.Name), ignore.case=TRUE, value=TRUE);
##
##    b) filter the data related to motor vehicles in SCC
##       motorSCC <- SCC %>% filter(Short.Name %in% motorsources) %>%
##           select(SCC, Short.Name);
##       motorSCC$SCC <- droplevels(motorSCC$SCC)
##
##    c) From NEI, filter the data related to Emissions due to motor vehicles
##       in the city of Baltimore.
##
##       balt_motor_emissions <- factoredNEI  %>%
##       filter(fips==BALTIMORE_FIPS) %>%
##         filter(SCC %in% motorSCC$SCC) %>%
##             select(SCC, Emissions, year);
##       balt_motor_emissions <- merge(balt_motor_emissions, motorSCC);
##
##    d) Select the data to plot.
##
##    Decisions to take: how to select the data to plot?. Group data over what
##    variables?
##
##    How many observations per variable ?
##    dt <- data.table(balt_motor_emissions)
##    range(dt[, .N, SCC]$N)
##    [1] 2 3
##    (dt[, .N, year]$N)
##    [1] 28 28 18 14
##    range(dt[, .N, Short.Name]$N)
##    [1] 2 3
##
##    Made decision: the only variable with enough observations to be mean-full
##    is 'year', so we will group the data ONLY by year: total emissions by
##    year & RelativeFreq of emissions by year (different amount of
##    observations each year)
##
##    Finally, data to plot: Baltimore City total emissions (absolute
##    frequency), and relative frequency of emissions, from PM2.5, related to
##    motor vehicle sources, and group by year.
##
##    balt_motor_emissions <-
##         summarize(group_by(balt_motor_emissions, year),
##                   total_emissions=sum(Emissions), count_emissions=n(),
##                   relativefreq=total_emissions/count_emissions);
##
##    year   total_emissions count_emissions relativefreq
##    1 1999       0.5600000              14   0.04000000
##    2 2002      10.5183944              28   0.37565694
##    3 2005      10.2240684              28   0.36514530
##    4 2008       0.4772056              18   0.02651142
##
##
## 2) Plot the filtered data (make_plot() function): motor source ~ year
##
##    p1 <- plotdata(data_to_plot, "total_emissions", DEBUG_MODE);
##    p2 <- plotdata(data_to_plot, "relativefreq", DEBUG_MODE);


## -----------------------------------------------------------------------------

require(dplyr)
require(ggplot2);

## -----------------------------------------------------------------------------

plot5 <- function(DEBUG_MODE=FALSE)
{
    ## download & unzip if necessary (common code, see at the end)
    get_source_data();

    ## load processes data into memory (common code)
    SOURCE_DATA_NEI<- "./data/summarySCC_PM25.rds";
    SOURCE_DATA_SCC<- "./data/Source_Classification_Code.rds";
    SCC <- readRDS(SOURCE_DATA_SCC);
    ## Loading NEI will likely take a few seconds (around 45 seconds)
    if(!exists("NEI")){
        print("loading NEI (wait ...)");
        NEI <- readRDS(SOURCE_DATA_NEI);
    }

    ## 3) Select & prepare the data to plot
    print("Summarizing data to plot ...");
    data_to_plot <- get_plot_data(NEI, SCC, DEBUG_MODE);

    ## 4) Plot the filtered data: (motor source ~ year)
    print("Plotting ...");
    make_plot(data_to_plot, DEBUG_MODE);
    print("Done!");
}

## -----------------------------------------------------------------------------

get_plot_data <- function(NEI, SCC, DEBUG_MODE=FALSE)
{
    BALTIMORE_FIPS = "24510";

    ## a) Select only "motor source-related sources" data
    motorsources <-
        grep("motor",levels(SCC$Short.Name), ignore.case=TRUE, value=TRUE);

    factoredNEI <- mutate(NEI, year=as.factor(year));
    factoredNEI <- mutate(factoredNEI, type=as.factor(type));
    factoredNEI <- mutate(factoredNEI, SCC=as.factor(SCC));

    ## b) filter the data related to motor vehicles in SCC
    motorSCC <- SCC %>% filter(Short.Name %in% motorsources) %>%
        select(SCC, Short.Name);
    motorSCC$SCC <- droplevels(motorSCC$SCC)

    ## c) From NEI, filter the data related to Emissions due to motor vehicles
    ##    in the city of Baltimore.
    balt_motor_emissions <- factoredNEI  %>%
        filter(fips==BALTIMORE_FIPS) %>%
        filter(SCC %in% motorSCC$SCC) %>%
            select(SCC, Emissions, year);
    balt_motor_emissions <- merge(balt_motor_emissions, motorSCC);
    balt_motor_emissions$Short.Name<-droplevels(balt_motor_emissions$Short.Name)
    balt_motor_emissions$SCC<-droplevels(balt_motor_emissions$SCC);

    ## d) Select the data to plot. (see doc. at the top of the file)
    balt_motor_emissions <-
        summarize(group_by(balt_motor_emissions, year),
                  total_emissions=sum(Emissions), count_emissions=n(),
                  relativefreq=total_emissions/count_emissions);
    balt_motor_emissions <- data.frame(balt_motor_emissions);

    return(balt_motor_emissions);
    ## year total_emissions count_emissions relativefreq
    ## 1 1999       0.5600000              14   0.04000000
    ## 2 2002      10.5183944              28   0.37565694
    ## 3 2005      10.2240684              28   0.36514530
    ## 4 2008       0.4772056              18   0.02651142
}

## -----------------------------------------------------------------------------

make_plot <- function(data_to_plot, DEBUG_MODE)
{
    PNGWIDTH = 480;
    PNGHEIGHT = 480;
    initial_dev <- dev.cur();
    outputname <- "./data/plot5.png";
    if(!DEBUG_MODE)
        png(file = outputname, width = PNGWIDTH, height = PNGHEIGHT);

    ## Plot
    p1 <- plotdata(data_to_plot, "total_emissions", DEBUG_MODE);
    p2 <- plotdata(data_to_plot, "relativefreq", DEBUG_MODE);

    ##browser();
    if(DEBUG_MODE) print(p1);
    if(DEBUG_MODE) print(p2);
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
            dev.off();                      ## close PNG & screen devices
        stopifnot(initial_dev==dev.cur());
    }
}


## -----------------------------------------------------------------------------

plotdata <- function(data_to_plot, oy_name, DEBUG_MODE)
{
    ## YVALUE <- data_to_plot[["total_emissions"]];
    YVALUE <- data_to_plot[[oy_name]];
    XVALUE <- data_to_plot$year;
    YLABTEXT="tons";
    XLABTEXT="year";
    PTCOLOR="red";
    SHAPEVALUE=20;

    ##browser();
    if(oy_name=="relativefreq"){
        MAINTEXT=expression("Baltimore city motor vehicle-related emissions (relative freq.) from "*PM[2.5]);

        g <- ggplot(data = data_to_plot, aes(year, relativefreq, group=1));}

    else{
        MAINTEXT=expression("Baltimore city motor vehicle-related emissions (totals) from "*PM[2.5]);

        g <- ggplot(data = data_to_plot, aes(year, total_emissions, group=1));
    }
    g <- g +
        geom_point(colour=PTCOLOR, shape=SHAPEVALUE) +
        geom_line() +
        ## facet_grid(.~EI.Sector, labeller = facets_source_labeller) +
        ggtitle(MAINTEXT) +
        xlab(XLABTEXT) +
        ylab(YLABTEXT) +
            theme_bw();
    if(DEBUG_MODE) print(g);

    ## Complete the y axis
    YAXIS_ROUNDFACTOR <- 1;
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

##\function get_source_data()
## Given "RWW" as the current R working directory, this function download,
## unzip and stores the source data ready to be used.
get_source_data <- function()
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

## Looking for what variables in SCC can provide information related to "motor".
##
search_source <- function(data, text)
{
    ####browser();
    for (label in names(data)){
        ## print(label);
        ## print(levels(data[[label]]));
        result <- grep(text,
                       levels(data[[label]]), ignore.case=TRUE, value=TRUE);
        if(length(result))
            print(label);
    }
}

## -----------------------------------------------------------------------------
