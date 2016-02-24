library(ggplot2)
library(scales)
library(zoo)

source("functions.R")
source("functions_plot.R")

file <- "input/csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)

################################################################################
# Total cumulative

stats_total_abs <- get_points_abs(mem_stats)
stats_total_cum <- get_points_cum(mem_stats)
stats_items_abs <- get_items_abs(mem_stats)
stats_items_cum <- get_items_cum(mem_stats)

plot_total_abs(stats_total_abs, "Points")
plot_total_cum(stats_total_cum, "Points")
plot_total_abs(stats_items_abs, "Items")
plot_total_cum(stats_items_cum, "Items")

get_points_total_avg_day(mem_stats)
get_items_total_avg_day(mem_stats)

################################################################################
# Total averages per period

mem_stats_points.xts <- xts(mem_stats[, c(10)], order.by = mem_stats[, "DATE"])
mem_stats_items.xts <- xts(mem_stats[, c(11)], order.by = mem_stats[, "DATE"])

print(head(mem_stats_items.xts))

plot_weekly(mem_stats_items.xts, "Items")
plot_monthly(mem_stats_items.xts, "Items")
plot_quarterly(mem_stats_items.xts, "Items")
plot_yearly(mem_stats_items.xts, "Items")

plot_weekly(mem_stats_points.xts, "Points")
plot_monthly(mem_stats_points.xts, "Points")
plot_quarterly(mem_stats_points.xts, "Points")
plot_yearly(mem_stats_points.xts, "Points")

################################################################################
# Plot followersing

plot_followersing(mem_stats)