library(ggplot2)
library(scales)

source("functions.R")

file <- "csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)

points_total_abs <- get_points_abs(mem_stats)

mem_stats_sub <- get_subset(points_total_abs, "2015-03")
print(mem_stats_sub)