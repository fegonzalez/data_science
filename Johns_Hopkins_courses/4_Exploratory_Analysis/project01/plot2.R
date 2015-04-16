
#common source code for plot1() - plot4() functions: get_source_data(),
#load_power_data()
source("common.R");

require(graphics);
require(grDevices);


plot2 <- function(DEBUG_MODE=FALSE)
{
    ## 1) download & unzip if necessary (common code)
    get_source_data();

    ## 2) load processes data into memory (common code)
    plot_data <- get_power_data(FALSE);
    Sys.setlocale("LC_TIME", "en_US.UTF-8"); #to dates & weekdays in english

    ## 3) plot
    initial_dev <- dev.cur();
    outputname <- "./plot2.png";
    if(!DEBUG_MODE)
    {
        png(file = outputname, width = 480, height = 480);
    }
    op <- par(mfrow = c(1, 1),
        mar = c(4, 4, 4, 4),
        oma = c(0, 0, 0, 0),
        bg="white",
        las=0);


    ## Create plot on screen device
    ## xvalue <- 1:length(plot_data$Global_active_power);
    xvalue <- plot_data$date_time;
    yvalue <- plot_data$Global_active_power;
    plot(x=xvalue,
         y=yvalue,
         col="black",
         xlab="",
         ylab="Global Active Power (kilowatts)",
         yaxt="n",
         ## axes=FALSE,
         type="n"); #do not plot the points yet

    ## annotate
    lines(x=xvalue, y=yvalue);
    ## axis(side=1,
    ##      at = c(1, length(plot_data$Global_active_power)/2,
    ##          length(plot_data$Global_active_power)),
    ##      labels = c("Thu", "Fri","Sat"));
    axis(side=2, yaxp=c(0, 6, 3));
    box();

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
