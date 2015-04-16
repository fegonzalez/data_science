
#common source code for plot1() - plot4() functions: get_source_data(),
#load_power_data()
source("common.R");

require(graphics);
require(grDevices);


plot4 <- function(DEBUG_MODE=FALSE)
{
    ## 1) download & unzip if necessary (common code)
    get_source_data();

    ## 2) load processes data into memory (common code)
    plot_data <- get_power_data(FALSE);
    Sys.setlocale("LC_TIME", "en_US.UTF-8"); #to dates & weekdays in englishssss

    ## 3) plot
    initial_dev <- dev.cur();
    outputname <- "./plot4.png";
    if(!DEBUG_MODE)
    {
        png(file = outputname, width = 480, height = 480);
    }

    op <- par(mfrow = c(2, 2),     # 2x2 layout
              oma = c(2, 2, 0, 0), # two rows text at the outer left and bottom
              bg="white",
              cex.axis=0.9,    # small tick-labels
              las=0);

    subplot11(plot_data, DEBUG_MODE);
    subplot12(plot_data, DEBUG_MODE);
    subplot21(plot_data, DEBUG_MODE);
    subplot22(plot_data, DEBUG_MODE);

    ## 4) create the png file
    if(DEBUG_MODE)
    {
        print("dev.copy png");
        dev.copy(png, file = outputname,    ## Copy plot to a PNG file
             width = 480, height = 480);
    }
    dev.off();                           ## close the PNG device
    par(op); ## At end of plotting, reset to previous settings:


## browser()

    dev.off(); #close screen device

    stopifnot(initial_dev==dev.cur());
}

## -----------------------------------------------------------------------------

subplot11 <- function(plot_data, DEBUG_MODE=FALSE)
{
    stopifnot((class(plot_data)[1]=="tbl_df"));

    ## Create plot on screen device
    xvalue <- 1:length(plot_data$Global_active_power);
    yvalue <- plot_data$Global_active_power;
    plot(x=xvalue,
         y=yvalue,
         col="black",
         xlab="",
         ylab="Global Active Power",
         ## xaxt="n",
         axes=FALSE,
         type="n"); #do not plot the points yet

    ## annotate
    lines(x=xvalue, y=yvalue);
    axis(side=1,
         at = c(1, length(plot_data$Global_active_power)/2,
             length(plot_data$Global_active_power)),
         labels = c("Thu", "Fri","Sat"));
    axis(side=2, yaxp=c(0, 6, 3));
    box();
}

## -----------------------------------------------------------------------------

subplot12 <- function(plot_data, DEBUG_MODE=FALSE)
{
    stopifnot((class(plot_data)[1]=="tbl_df"));

    YVALUE1 <- plot_data$Voltage;
    ## XVALUE1 <- 1:length(YVALUE1);
    XVALUE1 <- plot_data$date_time;
    plot(x=XVALUE1,
         y=YVALUE1,
         col="black",
         xlab="datetime",
         ylab="Voltage",
         ## axes=FALSE,
         yaxt="n",
         type="n"); #do not plot the points yet

    lines(x=XVALUE1, y=YVALUE1);
    ## axis(side=1,
    ##      at = c(1, length(YVALUE1)/2, length(YVALUE1)),
    ##      labels = c("Thu", "Fri","Sat"));
    axis(side=2, at=seq(234, 246, by = 2));
    box();
}

## -----------------------------------------------------------------------------

subplot21 <- function(plot_data, DEBUG_MODE=FALSE)
{
    stopifnot((class(plot_data)[1]=="tbl_df"));

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

    legend("topright",
           col = COLORS,
           legend = LEGEND_NAMES,
           lty = 1, # ---
           xjust = 0,
           yjust = 1,
           bty ="n"
           );
}

## -----------------------------------------------------------------------------

subplot22 <- function(plot_data, DEBUG_MODE=FALSE)
{
    stopifnot((class(plot_data)[1]=="tbl_df"));

    YVALUE1 <- plot_data$Global_reactive_power;
    ## XVALUE1 <- 1:length(YVALUE1);
    XVALUE1 <- plot_data$date_time;
    plot(x=XVALUE1,
         y=YVALUE1,
         col="black",
         xlab="datetime",
         ylab="Global_reactive_power",
         ## axes=FALSE,
         yaxt="n",
         type="n"); #do not plot the points yet

    lines(x=XVALUE1, y=YVALUE1);
    ## axis(side=1,
    ##      at = c(1, length(YVALUE1)/2, length(YVALUE1)),
    ##      labels = c("Thu", "Fri","Sat"));
    axis(side=2, at=seq(0.0, 0.5, by = 0.1));
    box();
}

## -----------------------------------------------------------------------------
