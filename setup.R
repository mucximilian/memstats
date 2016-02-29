library(ggplot2)
library(scales)
library(zoo)

source("functions.R")
source("functions_mem_stats.R")
source("functions_plot.R")

# TO DO:
# Create directory structure if not existing

file <- "input/csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)