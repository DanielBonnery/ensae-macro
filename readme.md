Advance Time Series
================
Théo Ferry, Daniel Bonnéry
Jan 12, 2024

## How to run

### Install the following packages:

downloader, dplyr, ggplot2, matlab, plyr, reshape2, rjags, utils, zoo

### Copy and execute the project:

-   Clone the project to a local repository via:

``` r
system("git clone https://github.com/DanielBonnery/ensae-macro.git")
```

-   open the .rproj file in Rstudio
-   run:

``` r
library(targets)
tar_make()
```

## What it does

### Save intermediary objects

The program creates a "\_targets" folder with intermediary objects

### Create plots

The program creates an “outputs” folder and all the plots displayed in
the report and save them in the “outputs” folder
