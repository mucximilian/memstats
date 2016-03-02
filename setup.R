library(ggplot2)
library(scales)
library(zoo)
library(xts)

source("functions.R")
source("functions_plot.R")

# Indicate the name of the input CSV file here 
latest_file <- "memrise_stats_20160301.csv"
filepath <- paste("input/csv", latest_file, sep="/")

memstats <- get_data(file)

# Create output directories
dir.create("output", showWarnings = FALSE)
dir.create("output/plots", showWarnings = FALSE)
dir.create("output/csv", showWarnings = FALSE)