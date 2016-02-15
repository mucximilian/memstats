library(ggplot2)
library(scales)

source("functions.R")

################################################################################

get_items_total_abs <- function(mem_stats) {
    items_total_abs <- get_items_abs(mem_stats)
    
    # Plot the graph
    mem_stats_plot <- ggplot(items_total_abs, aes(x=DATE, y=ITEMS_ABS)) +
        geom_point(shape=1) +
        geom_smooth(method=lm) +
        labs(x = "") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = "Items per day") +
        scale_y_continuous(labels = comma) +
        labs(title = "Memrise all-time items absolute")
    
    plot(mem_stats_plot, "items_total_abs")
}

get_items_total_cum <- function(mem_stats) {
    items_total_cum <- get_items_cum(mem_stats)
    
    # Plot the graph
    mem_stats_plot <- ggplot(items_total_cum, aes(x=DATE, y=ITEMS_TOTAL)) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = "Items total (all-time)") +
        scale_y_continuous(labels = comma) +
        labs(title = "Memrise all-time items cumulative")
    
    plot(mem_stats_plot, "items_total_cum")
}

################################################################################

file <- "csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)

get_items_total_abs(mem_stats)
get_items_total_cum(mem_stats)