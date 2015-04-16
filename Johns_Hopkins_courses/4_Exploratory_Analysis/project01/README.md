################################################################################

# Exploratory Data Analysis

Project 01 for the course "Exploratory Data Analysis"

Author:  fegonzalez

################################################################################


Info.- complete project instructions: "https://class.coursera.org/exdata-012/human_grading/view/courses/973506/assessments/3/submissions"


Note.- Lets call "RWW" the directory where the input scrip stands.


## I) Project execution

1) System Requirements

- Active Internet connection (at least for the first execution)
- R installed
- Required R-packages already installed (data.table, dplyr, lubridate,
  graphics, grDevices);


2) Input files

- RWW/plot1.R:  creates plot1.png
- RWW/plot2.R:  creates plot2.png
- RWW/plot3.R:  creates plot3.png
- RWW/plot4.R:  creates plot4.png
- RWW/common.R: common code (load source file & creates the dataset to plot),
                used by RWW/plot1.R, RWW/plot2.R, RWW/plot3.R and RWW/plot4.R.


3) Execution steps

    a) Open a R session.
    b) Set the working directory to the one where "plot1.R" stands.
    c) In the R console type:
        # create plot1
        source("plot1.R")
        plot1()
        # create plot2
        source("plot2.R")
        plot2()
        # create plot3
        source("plot3.R")
        plot3()
        # create plot4
        source("plot4.R")
        plot4()


4) Output files

- RWW/plot1.png
- RWW/plot2.png
- RWW/plot3.png
- RWW/plot4.png


5) Design decision notes

a) get_source_data (common.R)

For the first execution of the project scripts, or if the source data is not
found in the expected place (see iii bellow), the source data is automatically
downloaded and installed in the system:

    i)   RWW/data directory is created.
    ii)  Source file downloaded: household_power_consumption.zip
    iii) source data unzipped:  RWW/data/household_power_consumption.txt


b) get_power_data (common.R)

- Common use:

   The project instructions says "Your code file should include code for reading the data ...".

   As this code is common to the four plots, instead of repeat it in each plot
script, it has been implemented in a unique function, and then each plot script
calls to that function to get the data.

- Optimization: only the rows relative to the required data are read (dates 2007-02-01 and 2007-02-02)


## II) Project test script

1) Input files

- RWW/project01_test.R


2) Test list

- plot_all(): create all the png files

- test_all(): to test the complete exercise tasks:

    i)   remove all the source data

    ii)  remove all the output png files

   iii) create all the png files


3) plot_all(), Execution steps:

    a) Open a R session.
    b) Set the working directory to the one where "project01_test.R" stands.
    c) In the R console type:

        source("project01_test.R")
        plot_all()


4) test_all(), Execution steps:

    a) Open a R session.
    b) Set the working directory to the one where "project01_test.R" stands.
    c) In the R console type:

        source("project01_test.R")
        test_all()
