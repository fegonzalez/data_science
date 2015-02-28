air_pollution_usa<- function()
{
    pollution <- read.csv("data/avgpm25.csv",
                          colClasses = c("numeric", "character", "factor",
                              "numeric", "numeric"));
    return(pollution);
    ## head(pollution);
    ## summary(pollution$pm25)
 }

##----------------------------------------------------------

baseplot_test <- function()
{
    library(datasets);

    par(mfcol = c(1,1), mar = c(5, 4, 2, 1));

    ## BASE PLOT
    with(airquality,
         plot(Wind,Ozone,
              main = "Ozone and Wind in New York City",
              type = "n"))
    ## ANNOTATION
    with(subset(airquality, Month == 5),
         points(x=Wind, y=Ozone, pch=1, col = "blue"))
    with(subset(airquality, Month != 5),
         points(Wind, Ozone, pch=20, col = "red"))
    legend("topright",
           pch = c(1,20),
           col = c("blue", "red"),
           legend = c("May", "Other Months"));
    ## REGRESSION LINE
    model <- lm(Ozone ~ Wind, airquality)
    abline(model, lwd = 2, col="purple")
}

##----------------------------------------------------------

multiplebase_plot_test <- function()
{
  Library(datasets);
  par(mfrow = c(1, 3),
      mar = c(4, 4, 2, 1),
      oma = c(0, 0, 2, 0));

#   with(airquality,
#   {
#     plot(Wind, Ozone, main = "Ozone and Wind")
#     plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
#     plot(Temp, Ozone, main = "Ozone and Temperature")
#     mtext("Ozone and Weather in New York City", outer = TRUE)
#   })

##to see the result step by step
  plot(airquality$Wind, airquality$Ozone, main = "Ozone and Wind",
       las=1, bg="white");
  plot(airquality$Solar.R, airquality$Ozone,
       main = "Ozone and Solar Radiation");
  plot(airquality$Temp, airquality$Ozone, main = "Ozone and Temperature")
  mtext("Ozone and Weather in New York City", outer = TRUE)
}##EOFUNC

##----------------------------------------------------------

lattice_test <- function()
{
  library(lattice)
  state <- data.frame(state.x77, region = state.region)
  xyplot(Life.Exp ~ Income | region,
         data = state,
         layout = c(4, 1))
}

##----------------------------------------------------------

ggplot2_test <- function()
{
  library(ggplot2);
  data(mpg);
  qplot(displ, hwy, data = mpg);
}

##----------------------------------------------------------

## 5.2) How Does a Plot Get Created?  (file devices)
plotfile_test<- function()
{
    ## a) EXPLICITLY launch a graphics device
    pdf(file = "myplot.pdf") # file created in my working directory

    ## b)Call a plotting function to make a plot (no plot will appear on the
    ## screen)
    with(faithful, plot(eruptions, waiting));

    ## c) Annotate plot if necessary
    title(main = "Old Faithful Geyser data");  #still nothing on screen

    ## d) EXPLICITLY CLOSE graphics device with dev.off() (THIS IS VERY
    ## IMPORTANT!)

    dev.off()  ## Close the PDF file device
    ## Now you can view the file 'myplot.pdf' on your computer
}

##----------------------------------------------------------

copy_plot_test <- function()
{
    dev.cur();
    with(faithful, plot(eruptions, waiting))  ## Create plot on screen device
    dev.cur()
    title(main = "Old Faithful Geyser data")  ## Add a main title
    dev.cur()
    dev.copy(png, file = "geyserplot.png")  ## Copy my plot to a PNG file
    dev.cur()
    dev.off()  ## Don't forget to close the PNG device!
    dev.cur()
}
