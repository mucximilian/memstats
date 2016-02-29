library(ggplot2)
library(scales)
library(zoo)

source("functions.R")
source("functions_mem_stats.R")
source("functions_plot.R")

file <- "input/csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)