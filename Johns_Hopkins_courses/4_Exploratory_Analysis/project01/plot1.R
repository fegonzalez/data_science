
#common source code for plot1() - plot4() functions: get_source_data(),
#load_power_data()
source("common.R");

require(graphics);
require(grDevices);


plot1 <- function(DEBUG_MODE=FALSE)
{
    ## 1) download & unzip if necessary (common code)
    get_source_data();

    ## 2) load processes data into memory (common code)
    plot_data <- get_power_data(DEBUG_MODE);

    ## 3) plot
    initial_dev <- dev.cur();
    outputname <- "./plot1.png";

    if(!DEBUG_MODE)
    {
            ## png(filename = "Rplot%03d.png",
            ##     width = 480, height = 480, units = "px", pointsize = 12,
            ##     bg = "white",  res = NA, ...,
            ##     type = c("cairo", "cairo-png", "Xlib", "quartz"), antialias)
        png(file = outputname, width = 480, height = 480);
    }

    op <- par(mfrow = c(1, 1),
        mar = c(4, 4, 4, 4),
        oma = c(0, 0, 0, 0),
        #bg="white",
        bg="white",
        las=0);

    ## Create plot on screen device
    hist(plot_data$Global_active_power, col="red", freq=TRUE,
         xlim=c(0,6), ylim=c(0, 1200),
         main = "Global Active Power",
         xlab="Global Active Power (kilowatts)",
         ylab="Frequency",
         xaxt="n",
         yaxt="n");

    ## annotate
    ## draw the axis with user-defined tick-marks (2 ways just to practice)
    axis(side=1, at=c(0, 2, 4, 6));
    axis(side=2, yaxp=c(0, 1200, 6));

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
