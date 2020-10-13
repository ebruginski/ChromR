ChromR
======

<p align="center">
<img src="https://github.com/ebruginski/ChromR/blob/master/docs/logo.png" weight="200" height = "231.8">
</p>

Description
-----------

The ChromR package brings some functions to help analysts,
Ph.D. students and technicians to plan and organize LC analyses. The
functions are separated into 2 main sections, Pre-analysis and
Post-analysis. In Pre-analysis, the functions will help to randomize the
injections order, calculation of the quantity of mobile phase and the
spent time of batches. The Post-analysis functions will help to organize
the obtained files according to their respective batches or classes,
avoiding human error.

Updates
-----------

13/10/2020 - New function to format peaklist for batch correction.

Getting Started
---------------

### a) Install package dependencies

``` r
install.packages(c("devtools","lubridate", "ggplot2", "progress"))
```

### b) Install the package

``` r
devtools::install_github("ebruginski/ChromR")
```

Overview
--------

### Pre-analysis functions

<b>Sample Randomization:</b>

x = The dataframe for this function need to have: first column as
samples id and the second column as sample class.

gnumber = number of classes in the sample, need to be between 2 and 6.

gnames = names of the class, need to be the same of the input data
frame.

<u>Example:</u>

``` r
library(ChromR)

## Input the sample list

samplelist <- read.csv("/example/samplelist.csv")

## Run the randomization function

rsamplelist <- SampleRand(x = samplelist, gnumber = 2, gnames = c("disease", "health"))
```

<b>Mobile phase and time calculation:</b>

x = path to the gradient profile .csv file, format the header as below.

<p align="left">
<img src="https://github.com/ebruginski/ChromR/blob/master/docs/ex_gradient_prof.png">
</p>

runs = number of the runs.

over = overage of mobile phase to keep the system secure (%).

plot = plot the gradient profile.

<u>Example:</u>
``` r
library(ChromR)

gradientprof <- "/example/gradientprofile.csv"

GradCalc(x = gradientprof, runs = 15, over = 15, plot = TRUE)
```

### Post-analysis functions

<b>Files organizer:</b>

x = path to the samplelist .csv file, format the header metadata as below.

<p align="left">
<img src="https://github.com/ebruginski/ChromR/blob/master/docs/ex_samplelist.png">
</p>

filetype = extension of the files that you want to move.

by = select the type of sorting, class or batch.

remove = TRUE to remove the files from origin and FALSE to not remove.

<u>Example:</u>
``` r
library(ChromR)

## Set the working directory where the files are
setwd("/example")

samplelist <- "/example/samplelist.csv"

FileOrganizer(x = samplelist, filetype = ".mzML", by= "class", remove = FALSE)
```
