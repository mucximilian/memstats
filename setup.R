library(ggplot2)
library(scales)
library(zoo)
library(xts)

source("functions.R")
source("functions_memstats.R")
source("functions_plot.R")

file <- "input/csv/memrise_stats_20160301.csv"

memstats <- get_data(file)

# Create output directories
dir.create("output", showWarnings = FALSE)
dir.create("output/plots", showWarnings = FALSE)
dir.create("output/csv", showWarnings = FALSE)