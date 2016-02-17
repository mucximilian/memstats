library(xts)

source("functions.R")

################################################################################

file <- "input/csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)

mem_stats.xts <- xts(mem_stats[, c(10, 11)], order.by = mem_stats[, "DATE"])

print(head(mem_stats.xts))

mem_stats_monthly <- apply.monthly(mem_stats.xts, mean, na.rm=TRUE)

print(mem_stats_monthly)