############################################################################
#
# Index
#
############################################################################
1) Principles of Analytic Graphics
2) Exploratory graphs
3) Plotting Systems in R
4) The Process of Making a Plot
5) Graphics Device ( ?Devices )
6) The Base Plotting System in R
7) The Lattice Plotting System
8) ggplot2 Plotting System
9) Clustering
10) Dimension Reduction

Annex 1) Making up the data
Annex 2) Glossary 2 Spanish
Annex 3) References
############################################################################


##--------------------------------------------------------------------------
## 1) Principles of Analytic Graphics
##--------------------------------------------------------------------------

   Principle 1: Show comparisons (Always ask "Compared to What?")

   Principle 2: Show causality, mechanism, explanation, systematic structure
                (What is your causal framework for thinking about a question?)

   Principle 3: Show multivariate data (more than 2 variables)

   Principle 4: Integrate multiple modes of evidence (words, numbers, images ..)

   Principle 5: Describe and document the evidence with appropriate labels,
                scales, sources, etc.
                A data graphic should tell a complete story that is credible

   Principle 6: Content is king (presentation is the servant)


##--------------------------------------------------------------------------
## 2) Exploratory graphs
##--------------------------------------------------------------------------

   Goal: Development graphs (quick & dirty)


2.1) Simple Summaries of Data

a) One dimension functions:
summary()
boxplot()
hist()
barplot()
Density plot

## e.g.
pollution <- read.csv("data/avgpm25.csv", 
                      colClasses = c("numeric", "character", "factor", "numeric", "numeric"))
head(pollution)
hist(pollution$pm25, col = "green")
rug(pollution$pm25)
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)
abline(h = 12, lwd = 2, lty = 2)
barplot(table(pollution$region), col = "wheat", main = "Counties per Region")


b) Two dimensions

    b.1) Multiple/overlayed 1-D plots (Lattice/ggplot2)
    b.2) Scatterplots
    b.3) Smooth scatterplots

INFO Scatterplots:

     The scatterplot is useful for displaying the relationship between two
continuous variables, although it can also be used with one continuous and one
categorical variable, or two categorical variables.


## b.1)
boxplot(pm25 ~ region, data = pollution, col = "red")
## b.2)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1)) # (see 6.1.1)
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")

## b.3)
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main ="West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main ="East"))
with(pollution, plot(latitude, pm25, col = region))

c) More than 2 dimensions

    Overlayed/multiple 2-D plots; coplots
    Use color, size, shape to add dimensions
    Spinning plots
    Actual 3-D plots (not that useful)


*) Overlaying Features:
abline() # Add Straight Lines to a Plot (v=vertical, h=horizontal)
rug()    # Add a rug to a plot



##--------------------------------------------------------------------------
## 3) Plotting Systems in R
##--------------------------------------------------------------------------

3.1) Base: "artist's palette" (graphic step by step) model.
     BAD) Can’t go back once plot has started

3.2) library(lattice): entire plot specified by one function
     BAD) Sometimes awkward to specify an entire plot in a single function call
     BAD) Cannot "add" to the plot once it is created

## state <- data.frame(state.x77, region = state.region)
## xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

3.3) library(ggplot2): Mixes elements of Base and Lattice

## qplot(displ, hwy, data = mpg)


WARNING   The systems can not be mixed !


##--------------------------------------------------------------------------
## 4) The Process of Making a Plot
##--------------------------------------------------------------------------

a) Where will the plot be made? On the screen? In a file?

b) How will the plot be used?

    ? Is the plot for viewing temporarily on the screen?
    ? Will it be presented in a web browser?
    ? Will it eventually end up in a paper that might be printed?
    ? Are you using it in a presentation?

c) Size of data into the plot?

d) Do you need to be able to dynamically resize the graphic?

e) What graphics system will you use: base, lattice, or ggplot2?


##--------------------------------------------------------------------------
## 5) Graphics Device ( ?Devices )
##--------------------------------------------------------------------------

A graphics device is something where you can make a plot appear

When you make a plot in R, it has to be "sent" to a specific graphics device


5.1) Types of Graphics devices

  a) Screen devices: quartz() (MAC), x11() (Linux), windows

  b) File Devices

    b1) Vector formats (pdf, svg, ps, win.metafile)

        GOOD: resize well.
        GOOD: line drawings and solid colors using a modest number of points
        BAD: plot with many objects/points

    b2) Bitmap formats (png, jpeg, tiff, bmp)

        GOOD: large number of points, natural scenes or web-based plots
        BAD: line-type graphics, resize.


5.2) How Does a Plot Get Created?  (screen devices)

  a) Call a plotting function like plot, xyplot, or qplot
     The plot appears on the screen device

  b) Annotate plot if necessary


5.3) How Does a Plot Get Created?  (file devices)

  a) EXPLICITLY launch a graphics device

  pdf(file = "myplot.pdf") # file created in my working directory

  b) Call a plotting function to make a plot (no plot will appear on the screen)

with(faithful, plot(eruptions, waiting));

  c) Annotate plot if necessary

title(main = "Old Faithful Geyser data");  #still nothing on screen

  d) EXPLICITLY CLOSE graphics device with dev.off() (THIS IS VERY IMPORTANT!)

  dev.off()  ## Close the PDF file device
                ## Now you can view the file 'myplot.pdf' on your computer


5.4) Multiple Open Graphics Devices

* It is possible to open multiple graphics devices (screen, file, or both)

* Plotting can only occur on one graphics device at a time

* ?dev.cur
* dev.cur() : currently active graphics device =  2, 3, ...
* dev.set(<integer>) : change the active graphics device
* dev.off() : close current device


5.5) Copying plots

* dev.copy: copy a plot from one device to another
* dev.copy2pdf

## Example:
copy_plot_test <- function()
{
    dev.cur();
    with(faithful, plot(eruptions, waiting))  ## Create plot on screen device
    dev.cur()
    title(main = "Old Faithful Geyser data")  ## annotate
    dev.cur()
    dev.copy(png, file = "geyserplot.png")    ## Copy plot to a PNG file
    dev.cur()
    dev.off()  ## Don't forget to close the PNG device!
    dev.cur()
}


##--------------------------------------------------------------------------
## 6) The Base Plotting System in R
##--------------------------------------------------------------------------

library(graphics) # plotting functions (plot, hist, ...)

library(grDevices) # contains all the code implementing the various graphics
                   # devices, including X11, PDF, PostScript, PNG, etc.

There are two phases to creating a base plot

  i) Initializing a new plot
  ii) Annotating (adding to) an existing plot


6.1) Graphic Parameters

    pch: the plotting symbol (default is open circle)

    lty: the line type (default is solid line), can be dashed, dotted, etc.

    lwd: the line width, specified as an integer multiple

    col: the plotting color, specified as a number, string, or hex code;
         the colors() function gives you a vector of colors by name

    xlab: character string for the x-axis label

    ylab: character string for the y-axis label

    type: "y" plot & put the data in the plot;
          "n" plot but not put the data yet -> blank plot (add "points" later)


6.1.1) The par() function

It is used to specify global graphics parameters that affect all plots in an R
session. These parameters can be overridden when specified as arguments to
specific plotting functions.

    las: the orientation of the axis labels on the plot

    bg: the background color

    mar: the margin size (lines): c(bottom, left, top, right)

    oma: the outer margin size (default is 0 for all sides)

    mfrow: number of plots per row, column (plots are filled row-wise)
    mfcol: number of plots per row, column (plots are filled column-wise)


Use par("param_name") to see the parameter value (par("lty"))

## Example:
##
pollution <- read.csv("data/avgpm25.csv",
                      colClasses = c("numeric", "character", "factor",
                          "numeric", "numeric"));
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main ="West"))


6.2) Base Plotting Functions

    plot: make a scatterplot, or other type of plot depending on the class of
          the object being plotted

    lines: add lines to a plot, given a vector x values and a vector of y values
           (or a 2-column matrix); this function just connects the dots
           ## plot(cars, main = "lowess(cars)")
           ## lines(lowess(cars), col = 2)

    points: add points to a plot  { example(points) }

    text: add text labels to a plot using specified x, y coordinates

    title: add annotations to x, y axis labels, title, subtitle, outer margin

    mtext: add arbitrary text to the margins (inner or outer) of the plot

    axis: adding axis ticks/labels



## Example:
##
 plot_test <- function()
 {
     library(datasets);
     par(mfcol = c(1,1), mar = c(5, 4, 2, 1));
     with(airquality,
          plot(Wind,Ozone,
               main = "Ozone and Wind in New York City",
               type = "n"))
     with(subset(airquality, Month == 5),
          points(Wind, Ozone, pch=1, col = "blue"))
     with(subset(airquality, Month != 5),
          points(Wind, Ozone, pch=4, col = "red"))

     legend("topright",
            pch = c(1,4),
            col = c("blue", "red"),
            legend = c("May", "Other Months"));
}

## Example:
 multiplebase_plot_test <- function()
 {
   library(datasets);
   par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0));
   #to see the result step by step
   plot(airquality$Wind, airquality$Ozone, main = "Ozone and Wind");
   plot(airquality$Solar, airquality$Ozone,
        main = "Ozone and Solar Radiation");
   plot(airquality$Temp, airquality$Ozone, main = "Ozone and Temperature")
   mtext("Ozone and Weather in New York City", outer = TRUE)
 }


6.3) stripchart function

## warning: plot() "plots" a boxplot instead of a point for factor variable
op <- par(mfrow = c(1, 1),
          bg="white",
          las=0);

stripchart(Wind ~ as.factor(Ozone),
           main = "Ozone and Solar Radiation",
           vertical = TRUE,
           pch = 20, lwd = 1, col = "black",
           data = airquality);
par <- op;

6.4) Complex plotting

a) layout()

6.5) Colors

"/jh_all_courses/04_ExploratoryAnalysis/Colors/index.html#9"

- colorRamp (grDevices): Take a palette of colors and return a function that
takes valeus between 0 and 1, indicating the extremes of the color palette
(e.g. see the 'gray' function).

-colorRampPalette (grDevices): Take a palette of colors and return a function
that takes integer arguments and returns a vector of colors interpolating the
palette (like heat.colors or topo.colors).

- colors() lists the names of colors you can use in any plotting function.


##--------------------------------------------------------------------------
## 7) The Lattice Plotting System
##--------------------------------------------------------------------------

* library(lattice): plotting functions independent of the “base” graphics system

* library(grid): implements a different & independent graphing system.
                 The lattice package builds on top of grid.

* One phase: all plotting/annotation is done at once with a single function call

* Good for putting many many plots on a screen to compare them
  (see e.g. 'customize_panel' at the end)


7.1) Lattice Functions

       << lattice_function (oy_var ~ ox_var | f * g, data) >>

     - f and g are conditioning variables — they are optional.

     - The * indicates an interaction between two variables.

     - data: if no one is passed, then the parent frame is used.

    (e.g.) xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1))

    Produces a set of 5 panels showing the relationship between Ozone and Wind
    for each month (one panel per month).


    xyplot: this is the main function for creating scatterplots.

    bwplot: box-and-whiskers plots (“boxplots”)

    histogram: histograms

    stripplot: like a boxplot but with actual points

    dotplot: plot dots on "violin strings"

    splom: scatterplot matrix; like pairs in base plotting system.

    levelplot, contourplot: for plotting "image" data

## e.g.
library(lattice)
library(datasets)
xyplot(Ozone ~ Wind, data = airquality)
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1))


7.2) Lattice Behavior

  Lattice vs base graphics functions.

* Base graphics functions plot data directly to the graphics device (screen,
PDF file, etc.)

* Lattice graphics functions return an object of class trellis.

* The print methods for lattice functions actually do the work of plotting the
data on the graphics device.

* Lattice functions return "plot objects" that can, in principle, be stored
(but it’s usually better to just save the code + data).

* On the command line, trellis objects are auto-printed so that it appears the
function is plotting the data.

## p <- xyplot(Ozone ~ Wind, data = airquality)  ## Nothing happens!
## class(p) # [1] "trellis"
## print(p)                                      ## Plot appears


7.3) Lattice Panel Functions

* The panel function controls what happens inside each panel of the plot.

* Panel functions receive the x/y coordinates of the data points in their panel
(along with any optional arguments)


7.3.1) Customizing 'panel' functions.

       1) call default panel function (panel.abline, panel.lmline, ...)
       2) overlay functions

## e.g. customize_panel
##
## customize_panel <- function(x, y, ...)
## {
##     panel.xyplot(x, y, ...)                # 1) call default panel function
##     panel.abline(h = median(x), lty = 2);  # 2) overlay
##     panel.lmline(x, y, col = 2);
## }
## airquality <- transform(airquality, Month = factor(Month))
## xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1),
##        panel=customize_panel);



7.) Graphical Parameters for Trellis Displays

* trellis.par.set()

Description:

Functions used to query, display and modify graphical parameters for fine
control of Trellis displays.  Modifications are made to the settings for the
currently active device only.



##--------------------------------------------------------------------------
## 8) ggplot2 Plotting System (library(ggplot2)
##--------------------------------------------------------------------------

* Split the difference between base and lattice

* Automatically deals with spacings, text, titles but also allows you to
  annotate by “adding”

\NOTICE: www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2).html
  "To make graphs with ggplot2, the data must be in a data frame, and in "long"
   (as opposed to wide) format."

## (e.g. project02/plot3.R: paint emissions ~ year + type)
##
# baltimore_emissions_by_type_and_year <-
#    xtabs(Emissions ~ year + type , data = group_by(baltimore, year));
# baltimore_totals_df <- as.data.frame(baltimore_emissions_by_type_and_year)


8.1) The Basics: qplot()

* Plots are made up of AESTHETICS (size, shape, color) and GEOMS (points, lines)

* Factors are important for indicating subsets of the data (if they are to have
  different properties); they should be labeled.

* Use 'ggplot()' for doing things 'qplot()' cannot do.

* params:

    - qplot(x, y, ...): plot = point y ~ x

    - qplot(x, ...):    plot = histogram(x)


8.1.1) qplot() example

library(ggplot2)
str(mpg)
qplot(x=displ, y=hwy, data=mpg)

## Note.- levels(mpg$drv) =  "4" "f" "r"

a) Modifying aesthetics (size, shape, color)

qplot(displ, hwy, data = mpg, color = drv) # color = mpg$drv
qplot(displ, hwy, data = mpg, shape = drv) # shape = mpg$drv
qplot(displ, hwy, data = mpg, size=drv, color=drv)

b) Adding a geom (points, lines)

qplot(log(hwy), data = mpg, geom="density")
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"), method="lm")
##\warning using the advanced function geom_smooth (see later)
qplot(votes, rating, data = movies) + geom_smooth()

c) Histograms

qplot(x=log(hwy), data = mpg)
qplot(x=log(hwy), data = mpg, fill = drv) # Histograms by group

d) Facets (conditional plots: layout of rows & cols)

qplot(displ, hwy, data = mpg, facets = . ~ drv)      # 3 cols (. ~ drv)
qplot(hwy, data = mpg, facets = drv ~ ., binwidth=2) # 3 rows (drv ~ .)
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"), method="lm",
      facets = . ~ drv)


8.2) The Advanced: ggplot()

* New components:

  - stats: statistical transformations like binning, quantiles, smoothing.

  - scales: what scale an aesthetic map uses (example: male=red, female=blue).

  - coordinate system.

* Plots are built up in layers (similar to 'The Base Plotting System')

    1) Plot the data
    2) Overlay a summary
    3) Metadata and annotation

* Functions:

   - aes: generate aesthetic mappings that describe how variables in the data
          are mapped to visual properties (aesthetics) of geoms.

   - geom_point: used to create scatterplots.

   - geom_smooth: add a smoothed conditional mean.

   - facet_grid: lay out panels in a grid.

   - geom_bar: histograms

* Axes:

    http://www.cookbook-r.com/Graphs/Axes_%28ggplot2%29/

    - scale_x_continuous



8.2.1) ggplot() example

## MAACS data
##
   load("./data/maacs.Rda")
##
   str(maacs)
## 'data.frame':	750 obs. of  5 variables:
##  $ id       : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ eno      : num  141 124 126 164 99 68 41 50 12 30 ...
##  $ duBedMusM: num  2423 2793 3055 775 1634 ...
##  $ pm25     : num  15.6 34.4 39 33.2 27.1 ...
##  $ mopos    : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...

a) Basic plot

qplot(x=log(pm25), y=duBedMusM, data=maacs, facets=.~mopos, 
        geom=c("point", "smooth"), method="lm")

b) Building Up in Layers

head(maacs, 2); tail(maacs,2)
##
##      id  eno  duBedMusM   pm25    mopos
## 1     1  141       2423   15.560   yes
## 2     2  124       2793   34.370   yes
## ...
## 749 749  17.0       113   23.145    no
## 750 750  13.5        96   26.732    no

   g <- ggplot(maacs, aes(log(pm25), duBedMusM))
   summary(g)
   ## data: id, eno, duBedMusM, pm25, mopos [750x5]
   ## mapping:  x = log(pm25), y = duBedMusM
   ## faceting: facet_null()
   print(g)   # Error: No layers in plot yet

c) First Plot with Point Layer

   g <- g + geom_point(); print(g)
   g <- ggplot(maacs, aes(log(pm25), duBedMusM, color=mopos));
   print(g)
   g <- g + geom_point(); print(g)

d) Adding More Layers: Smooth

   g <- g + geom_point() + geom_smooth(); print(g)
   g <- g + geom_point() + geom_smooth(method = "lm"); print(g)

e) Adding More Layers: Facets

   g<- g + geom_point() + facet_grid(.~mopos) + geom_smooth(method="lm")
   print(g)


8.2.2) Annotation

* Labels: xlab(), ylab(), labs(), ggtitle()

* Each of the “geom” functions has options to modify

* theme(): for things that only make sense globally.
           ## o Example: theme(legend.position = "none")

* Two standard appearance themes are included
  o theme_gray(): The default theme (gray background)
  o theme_bw(): More stark/plain

f) Modifying Aesthetics

   g <- g + geom_point(color="steelblue", size=4, alpha=1/2); print(g);
   g <- g + geom_point(aes(color=mopos), size=4, alpha=1/2); print(g);

g) Modifying Labels

   g <- g + labs(title = "MAACS Cohort"); print(g);
   g <- g + labs(x= expression("log " * PM[2.5]), y="the duBedMusM"); print(g);

h) Customizing the Smooth

   g <- g + geom_point(aes(color=mopos), size=2, alpha=1/2); print(g);
   g <- g + geom_smooth(size=4, linetype=3, method="lm", se=FALSE, color="blue")
   print(g);

i) Changing the Theme

   g <- g + geom_point(aes(color = mopos)); print(g)
   g <- g + geom_point(aes(color = mopos)) + theme_bw(base_family = "Times");
   print(g)
   g <- g + geom_point(aes(color = mopos)) + theme_gray(base_family = "Times");
   print(g);

j) Axis Limits

j.1) A Note about Axis Limits (e.g. Outliers)

   testdat <- data.frame(x = 1:100, y = rnorm(100))
   testdat[50,2] <- 100  ## Outlier!
   plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3))
   g <- ggplot(testdat, aes(x = x, y = y)); print(g)
   g <- g + geom_line(); print(g)

j.2) Axis Limits

   g <- g + geom_line() + ylim(-3, 3); print(g);
   g <- ggplot(testdat, aes(x = x, y = y)) +geom_line();
   g <- g + geom_line() + coord_cartesian(ylim=c(-3,3)); print(g);


8.2.3) More Complex Example

PROBLEM: How to show the relation between two variables in function of a third
variable that is not categorical but continuous?

   i.e. " duBedMusM ~ log(pm25) | eno & mopos , data = maacs "
   when 'eno' is continuous

SOLUTION: cut() function

k) Making 'eno' Tertiles

   ## Calculate the tertiles of the data
   cutpoints <- quantile(maacs$eno, seq(0, 1, length=4), na.rm=TRUE);
   ## 0%   33.33333%    66.66667%   100%

   ## Cut the data at the tertiles and create a new factor variable
   maacs$eno2tert <- cut(maacs$eno, cutpoints)
   str(maacs);

   ## See the levels of the newly created factor variable
   levels(maacs$eno2tert)
   ## [1] "(5,20]"     "(20,51.3]"  "(51.3,276]"

l) Code for Final Plot

   ## Setup ggplot with data frame
   rm(g);
   g <- ggplot(maacs, aes(log(pm25), duBedMusM)); print(g);

   ## Add layers
   g <- g + geom_point(alpha = 1/3);                             print(g);
   g <- g + facet_wrap(mopos ~ eno2tert, nrow = 2, ncol = 4);    print(g);
   g <- g + geom_smooth(method="lm", se=FALSE, col="steelblue"); print(g);
   g <- g + theme_bw(base_family = "Avenir", base_size = 10);    print(g);
   g <- g + labs(x = expression("log " * PM[2.5]));              print(g);
   g <- g + labs(y = "duBedMusM");                               print(g);
   g <- g + labs(title = "MAACS Cohort");                        print(g);


## str(maacs)
## $ eno      : num  141 124 126 164 99 68 41 50 12 30 ...
## $ duBedMusM: num  2423 2793 3055 775 1634 ...
## $ pm25     : num  15.6 34.4 39 33.2 27.1 ...
## $ mopos    : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...


##--------------------------------------------------------------------------
## 9) Clustering
##--------------------------------------------------------------------------

How do we define close?

* Most important step
  - Garbage in $\longrightarrow$ garbage out
* Distance or similarity
  - Continuous - euclidean distance
  - Continous - correlation similarity
  - Binary - manhattan distance
* Pick a distance/similarity that makes sense for your problem


Important references:

[Rafa''s Distances and Clustering Video](http://www.youtube.com/watch?v=wQhVWUcXM0A)

[Elements of statistical learning](http://www-stat.stanford.edu/~tibs/ElemStatLearn/)


9.1) Hierarchical clustering

* Gives an idea of the relationships between variables/observations
* The picture may be unstable
  - Change a few points
  - Have different missing values
  - Pick a different distance
  - Change the merging strategy
  - Change the scale of points for one variable
* But it is deterministic
* Choosing where to cut is not always obvious
* Should be primarily used for exploration

a) R functions

- dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
## ?stats::dist

set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
df <- data.frame(x=x,y=y)
## euclidean distance
dist(df);

- hclust(d, method = "complete", members = NULL) ## Hierarchical clustering
## ?stats::hclust

distxy <- dist(df)
hClustering <- hclust(distxy)
plot(hClustering)

- heatmap
set.seed(143)
dataMatrix <- as.matrix(df)[sample(1:12), ]
heatmap(dataMatrix)

* Pretty dendograms
## jh_all_courses/04_ExploratoryAnalysis/hierarchicalClustering/index.html#15
## http://gallery.r-enthusiasts.com/RGraphGallery.php?graph=79

9.2) K-means Clustering

* A partioning approach
  - Fix a number of clusters
  - Get "centroids" of each cluster
  - Assign things to closest centroid
  - Reclaculate centroids
* Requires
  - A defined distance metric
  - A number of clusters
  - An initial guess as to cluster centroids
* Produces
  - Final estimate of cluster centroids
  - An assignment of each point to clusters


a) R functions

- kmeans()  ## ?stats::kmeans
    Important parameters: x, centers, iter.max, nstart

set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
df <- data.frame(x, y)
kmeansObj <- kmeans(df, centers = 3)
names(kmeansObj)
kmeansObj$cluster
par(mar = rep(0.2, 4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)
## heatmap
set.seed(1234)
dataMatrix <- as.matrix(df)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")


##--------------------------------------------------------------------------
## 10) Dimension Reduction.- Principal Components Analysis and Singular Value
## Decomposition
## --------------------------------------------------------------------------


10.1) Principal Components Analysis (wikipedia)

En estadística, el análisis de componentes principales (en español ACP, en
inglés, PCA) es una técnica utilizada para reducir la dimensionalidad de un
conjunto de datos. Intuitivamente la técnica sirve para hallar las causas de la
variabilidad de un conjunto de datos y ordenarlas por importancia.

10.2) Singular value decomposition

In linear algebra, the singular value decomposition (SVD) is a factorization of
a real or complex matrix. It has many useful applications in signal processing
and statistics.


jh_all_courses/04_ExploratoryAnalysis/dimensionReduction/index.html


- svd (?base::svd): compute the singular-value decomposition of a rectangular
matrix.



Related problems

You have multivariate variables X1,…,Xn so X1=(X11,…,X1m)

- Find a new set of multivariate variables that are uncorrelated and explain as
much variance as possible.

- If you put all the variables together in one matrix, find the best matrix
created with fewer variables (lower rank) that explains the original data.

The first goal is statistical and the second goal is data compression.


* Related solutions - PCA/SVD

SVD

If X is a matrix with each variable in a column and each observation in a row
then the SVD is a "matrix decomposition"

     X=UDVT

where the columns of U are orthogonal (left singular vectors), the columns of V
are orthogonal (right singular vectors) and D is a diagonal matrix (singular
values).

PCA

The principal components are equal to the right singular values if you first
scale (subtract the mean, divide by the standard deviation) the variables.


* Notes

Johns_Hopkins_courses/jh_all_courses/04_ExploratoryAnalysis/dimensionReduction/index.html#24



############################################################################
#
# Glossary 2 Spanish
#
############################################################################

tidy: ordenado

thorough: minucioso

Scatter: dispersión


############################################################################
#
# References
#
############################################################################


[1] Principles of Analytic Graphics

[1.1] Edward Tufte (2006). Beautiful Evidence, Graphics Press LLC. - www.edwardtufte.com


[2] Exploratory graphs

[2.1] R Graph Gallery - http://gallery.r-enthusiasts.com/

[2.2] R Bloggers - http://www.r-bloggers.com/


[3] Plotting Systems in R

[3.1] Paul Murrell (2011). R Graphics, CRC Press.

[3.2] Hadley Wickham (2009). ggplot2, Springer.


[8] ggplot2 Plotting System

[8.1] The ggplot2 book - Hadley  Wickham

[8.2] The R Graphics Cookbook - Winston Chang - http://www.cookbook-r.com/Graphs

[8.3] ggplot2 web site - http://ggplot2.org - http://docs.ggplot2.org


[9] Clustering

[9.1] Rafa''s Distances and Clustering Video - (http://www.youtube.com/watch?v=wQhVWUcXM0A

[9.2] Elements of statistical learning - http://www-stat.stanford.edu/~tibs/ElemStatLearn/)


[10] Dimension Reduction

[10.1] jh_all_courses/04_ExploratoryAnalysis/dimensionReduction/index.html
