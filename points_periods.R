library(xts)
library(ggplot2)
library(scales)

source("functions.R")

################################################################################

get_points_total_weekly <- function(mem_stats) {
    mem_stats_weekly <- apply.weekly(mem_stats.xts, mean, na.rm=TRUE)
    
    bla <- data.frame(datetime = index(mem_stats_weekly), mem_stats_weekly[, c(1)], row.names = NULL)
    
    print(head(bla))
    
    # Plot the graph
    mem_stats_plot <- ggplot(bla, aes(x=datetime, y=bla[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Week") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = "Points") +
        scale_y_continuous(labels = comma) +
        labs(title = "Memrise points (average per week)")
    
    plot(mem_stats_plot, "points_total_weekly")
}

get_points_total_monthly <- function(mem_stats) {
    mem_stats_monthly <- apply.monthly(mem_stats.xts, mean, na.rm=TRUE)
    print(mem_stats_monthly)
}

get_points_total_quarterly <- function(mem_stats) {
    mem_stats_quarterly <- apply.quarterly(mem_stats.xts, mean, na.rm=TRUE)
    print(mem_stats_quarterly)
}

get_points_total_yearly <- function(mem_stats) {
    mem_stats_yearly <- apply.yearly(mem_stats.xts, mean, na.rm=TRUE)
    print(mem_stats_yearly)
}

################################################################################

file <- "input/csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)

mem_stats.xts <- xts(mem_stats[, c(10)], order.by = mem_stats[, "DATE"])

print(head(mem_stats.xts))

get_points_total_weekly(mem_stats.xts)
get_points_total_monthly(mem_stats.xts)
get_points_total_quarterly(mem_stats.xts)
get_points_total_yearly(mem_stats.xts)