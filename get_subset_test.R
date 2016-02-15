library(ggplot2)
library(scales)

source("functions.R")

file <- "csv/memrise_stats_20160215.csv"

mem_stats_full <- get_data(file)
mem_stats <- get_data_points(mem_stats_full)

mem_stats_sub <- get_subset(mem_stats, "2015-03")
print(mem_stats_sub)