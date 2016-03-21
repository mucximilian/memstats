library(xts)

source("functions.R")

################################################################################

file <- "memrise_stats_20160321.csv"

mem_stats <- get_data(file)

mem_stats.xts <- xts(mem_stats[, "POINTS"], order.by = mem_stats[, "DATE"])
str(mem_stats.xts)

print(head(mem_stats.xts))

mem_stats_monthly <- apply.monthly(mem_stats.xts, mean, na.rm=TRUE)

print(mem_stats_monthly)