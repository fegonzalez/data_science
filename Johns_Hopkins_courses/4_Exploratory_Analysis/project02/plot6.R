
## PROBLEM (plot6)
##
## Compare emissions from motor vehicle sources in Baltimore City with
## emissions from motor vehicle sources in Los Angeles County, California (fips
## == "06037"). Which city has seen greater changes over time in motor vehicle
## emissions?
##
##
## SOLUTION DECISION
##
## Precondition) Which are the "emissions from motor vehicle sources", let's
## call it' 'motorsources'.
##
##
##     Note.- same solution than in plot5
##
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
##          grep("motor",levels(SCC$Short.Name), ignore.case=TRUE, value=TRUE);
##
##
## 1) Select & prepare the data to plot (get_plot_data() function)
##
##    a) Select only "motor source-related sources" data
##
##       motorsources <-
##           grep("motor",levels(SCC$Short.Name), ignore.case=TRUE, value=TRUE);
##
##    b) filter the data related to motor vehicles in SCC
##
##    motorSCC <- SCC %>% filter(Short.Name %in% motorsources) %>%
##                select(SCC, Short.Name);
##
##    c) From NEI, filter the data related to Emissions due to motor vehicles
##       in the city of Baltimore OR in LA. county.
##
##       motor_emissions <- factoredNEI  %>%
##         filter(fips==get_baltimore_fips() | fips==get_lacounty_fips()) %>%
##         filter(SCC %in% motorSCC$SCC) %>%
##             select(SCC, Emissions, fips, year);
##       motor_emissions <- merge(motor_emissions, motorSCC);
##
##  d) Select the data to plot: comparative of the total and relative emissions
##     between Baltimore & LA County from 1999 to 2008.
##
##     motor_emissions <-
##         summarize(group_by(motor_emissions, fips, year),
##                   total_emissions=sum(Emissions), count_emissions=n(),
##                   relativefreq=total_emissions/count_emissions);
##
## fips year total_emissions count_emissions relativefreq
## 1 06037 1999      68.4060000              13   5.26200000
## 2 06037 2002      78.0598486              38   2.05420654
## 3 06037 2005      85.7657985              38   2.25699470
## 4 06037 2008      85.1871200               5  17.03742400
## 5 24510 1999       0.5600000              14   0.04000000
## 6 24510 2002      10.5183944              28   0.37565694
## 7 24510 2005      10.2240684              28   0.36514530
## 8 24510 2008       0.4772056              18   0.02651142
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

plot6 <- function(DEBUG_MODE=FALSE)
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
    ## a) Select only "motor source-related sources" data
    motorsources <-
        grep("motor",levels(SCC$Short.Name), ignore.case=TRUE, value=TRUE);

    factoredNEI <- mutate(NEI, year=as.factor(year));
    factoredNEI <- mutate(factoredNEI, type=as.factor(type));
    factoredNEI <- mutate(factoredNEI, SCC=as.factor(SCC));
    factoredNEI <- mutate(factoredNEI, fips=as.factor(fips));

    ## b) filter the data related to motor vehicles in SCC
    motorSCC <- SCC %>% filter(Short.Name %in% motorsources) %>%
        select(SCC, Short.Name);
    motorSCC$SCC <- droplevels(motorSCC$SCC)

    ## c) From NEI, filter the data related to Emissions due to motor vehicles
    ##    in the city of Baltimore OR in LA. county.
    motor_emissions <- factoredNEI  %>%
        filter(fips==get_baltimore_fips() | fips==get_lacounty_fips()) %>%
        filter(SCC %in% motorSCC$SCC) %>%
            select(SCC, Emissions, fips, year);
    motor_emissions <- merge(motor_emissions, motorSCC);
    motor_emissions$Short.Name<-droplevels(motor_emissions$Short.Name)
    motor_emissions$SCC<-droplevels(motor_emissions$SCC);
    motor_emissions$fips<-droplevels(motor_emissions$fips);

    ## d) Select the data to plot: total & relative emissions ~ fips & year
    motor_emissions <-
        summarize(group_by(motor_emissions, fips, year),
                  total_emissions=sum(Emissions), count_emissions=n(),
                  relativefreq=total_emissions/count_emissions);
    motor_emissions <- data.frame(motor_emissions);

    return(motor_emissions);
}

## -----------------------------------------------------------------------------

make_plot <- function(data_to_plot, DEBUG_MODE)
{
    PNGWIDTH = 480;
    PNGHEIGHT = 480;
    initial_dev <- dev.cur();
    outputname <- "./data/plot6.png";
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
    ## example.- YVALUE <- data_to_plot[["total_emissions"]];
    YVALUE <- data_to_plot[[oy_name]];
    XVALUE <- data_to_plot$year;
    YLABTEXT="tons";
    XLABTEXT="year";
    PTCOLOR="red";
    SHAPEVALUE=20;

    ##browser();
    if(oy_name=="relativefreq"){
        MAINTEXT=expression("Baltimore vs LA. Motor vehicle-related emissions (relative freq.) from "*PM[2.5]);
        g <- ggplot(data=data_to_plot,
                    aes(year, relativefreq, group=fips, colour=fips));}
    else{
        MAINTEXT=expression("Baltimore vs LA. Motor vehicle-related emissions (totals) from "*PM[2.5]);
        g <- ggplot(data=data_to_plot,
                    aes(year, total_emissions, group=fips, , colour=fips));
    }
    g <- g +
        geom_point(colour=PTCOLOR, shape=SHAPEVALUE) +
            ## geom_line(colour=data_to_plot$fips) +
            geom_line() +
        ## facet_grid(.~EI.Sector, labeller = facets_source_labeller) +
        ggtitle(MAINTEXT) +
        xlab(XLABTEXT) +
        ylab(YLABTEXT) +
        scale_colour_discrete(name = "U.S. county ",
                              breaks=c(get_baltimore_fips(),
                                  get_lacounty_fips()),
                              labels=c("Baltimore", "LA")) +
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
    g <- g + theme(plot.title = element_text(size = rel(0.85))) +
        theme(axis.text.x = element_text(size = rel(0.85))) +
        theme(axis.text.y = element_text(size = rel(0.85))) +
        theme(axis.title.x=element_text(size=rel(0.85), angle=0, vjust = 0.5)) +
        theme(axis.title.y=element_text(size=rel(0.85), angle=90, vjust=1.5)) +
        theme(strip.text.x = element_text(angle=0, size=rel(0.9),
              hjust = 0.5, vjust = 0.5))
    if(DEBUG_MODE) print(g);
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

get_baltimore_fips <- function() {return("24510");}
get_lacounty_fips <- function() {return("06037")};

## -----------------------------------------------------------------------------

