
#common source code for plot1() - plot4() functions: get_source_data(),
#load_power_data()
source("common.R");

require(graphics);
require(grDevices);


##\nicetohave plot3: legend details
plot3 <- function(DEBUG_MODE=FALSE)
{
    ## 1) download & unzip if necessary (common code)
    get_source_data();

    ## 2) load processes data into memory (common code)
    plot_data <- get_power_data(FALSE);
    Sys.setlocale("LC_TIME", "en_US.UTF-8"); #to dates & weekdays in englishssss

    ## 3) plot
    initial_dev <- dev.cur();
    outputname <- "./plot3.png";
    if(!DEBUG_MODE)
    {
        png(file = outputname, width = 480, height = 480);
    }
    op <- par(mfrow = c(1, 1),
        mar = c(4, 4, 4, 4),
        oma = c(0, 0, 0, 0),
        bg="white",
        las=0);

    COLORS <- c("black", "red", "blue");
    LEGEND_NAMES <- c((names(plot_data)[7:9]))
    YVALUE1 <- plot_data$Sub_metering_1;
    ## XVALUE1 <- 1:length(YVALUE1);
    XVALUE1 <- plot_data$date_time;
    YVALUE2 <- plot_data$Sub_metering_2;
    XVALUE2 <- XVALUE1;
    YVALUE3 <- plot_data$Sub_metering_3;
    XVALUE3 <- XVALUE1;

    plot(x=XVALUE1,
         y=YVALUE1,
         col=COLORS[1],
         xlab="",
         ylab="Energy sub metering",
         yaxt="n",
         ## axes=FALSE,
         type="n"); #do not plot the points yet

    ## annotate
    points(x=XVALUE2, y=YVALUE2, type="n");
    points(x=XVALUE3, y=YVALUE3, type="n");
    lines(x=XVALUE1, y=YVALUE1, col = COLORS[1]);
    lines(x=XVALUE2, y=YVALUE2, col = COLORS[2]);
    lines(x=XVALUE3, y=YVALUE3, col = COLORS[3]);
    ## axis(side=1,
    ##      at = c(1, length(YVALUE1)/2, length(YVALUE1)),
    ##      labels = c("Thu", "Fri","Sat"));
    axis(side=2, yaxp=c(0, 30, 3));
    box();


    ## LEGEND_PCH <- "_";
    ## LEGEND_PCH_LEN <- 26;
    legend("topright",
           col = COLORS,
           legend = LEGEND_NAMES,
           lty = 1, # ---
           ## pch = LEGEND_PCH,
           ## xjust = 1,
           ## yjust = 1,
           ## seg.len = LEGEND_PCH_LEN
           ## adj = c(0, 0.1, 1)
           );


    ## 4) create the png file

    #WARNING:dev.copy produces little errors in legend result.
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
