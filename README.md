ChromR
======

<p align="center">
<img src="https://github.com/ebruginski/ChromR/blob/master/docs/logo.png" weight="200" height = "231.8">
</p>

Description
-----------

The ChromR package brings some functions to help analysts,
Ph.D. students and technicians to plan and organize LC analyses. The
functions are separated into 2 main sections, Pre-analyse and
Post-analyse. In Pre-analyse, the functions will help to randomize the
injections order, calculation of the quantity of mobile phase and the
spent time of batches. The Post-analyse functions will help to organize
the obtained files according to their respective batches or classes,
avoiding human error.

Getting Started
---------------

### a) Install package dependencies

``` r
install.packages(c("devtools","lubridate", "ggplot2", "progress"))
```

### b) Install the package

``` r
install_github("ebruginski/ChromR")
```

Overview
--------

### Pre-analyse functions

<b>Sample Randomization:</b>

x = The dataframe for this function need to have: first column as
samples id and the second column as sample groups.

gnumber = number of groups in the sample, need to be between 2 and 6.

gnames = names of the groups, need to be the same of the input data
frame.

<sub>Example:</sub>

``` r
library(ChromR)

## Input the sample list

samplelist <- read.csv("/example/samplelist.csv")

## Run the randomization function

rsamplelist <- SampleRand(x = samplelist, gnumber = 2, gnames = c("disease", "health"))
```

<b>Mobile phase and time calculation:</b>

x = path to the gradient profile .csv file

<p align="left">
<img src="https://github.com/ebruginski/ChromR/blob/master/docs/ex_gradient_prof.png">
</p>

runs = number of the runs.

over = overage of mobile phase to keep the system secure (%).

plot = plot the gradient profile.
```r
library(ChromR)

gradientprof <- "/example/gradientprofile.csv"

GradCalc(x = gradientprof, runs = 15, over = 15, plot = TRUE)
```


### Post-analyse functions









