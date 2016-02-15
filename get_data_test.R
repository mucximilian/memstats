library(ggplot2)
library(scales)

source("get_data.R")

file <- "csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)

print(head(mem_stats))