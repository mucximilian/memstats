library(ggplot2)
library(scales)

source("functions.R")

################################################################################

get_points_total_abs <- function(mem_stats) {
    points_total_abs <- get_points_abs(mem_stats)
    
    # Plot the graph
    mem_stats_plot <- ggplot(points_total_abs, aes(x=DATE, y=POINTS_ABS)) +
        geom_point(shape=20, size=1, color="grey30") +
        geom_smooth(method=lm) +
        geom_hline(yintercept=mean(points_total_abs$POINTS_ABS, na.rm=TRUE),
                   colour="lightblue") +
        labs(x = "Day") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = "Points") +
        scale_y_continuous(labels = comma) +
        labs(title = "Memrise points per day (all-time)")
    
    plot(mem_stats_plot, "points_total_abs")
}

get_points_total_cum <- function(mem_stats) {
    points_total_cum <- get_points_cum(mem_stats)
    
    # Plot the graph
    mem_stats_plot <- ggplot(points_total_cum, aes(x=DATE, y=POINTS_TOTAL)) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Day") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = "Points") +
        scale_y_continuous(labels = comma) +
        labs(title = "Memrise points cumulative (all-time)")
    
    plot(mem_stats_plot, "points_total_cum")
}

get_points_total_avg_day <- function(mem_stats) {
    points_total_avg <- get_points_abs(mem_stats)
    print(mean(points_total_avg$POINTS_ABS, na.rm=TRUE))
}

################################################################################

file <- "input/csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)

get_points_total_cum(mem_stats)
get_points_total_abs(mem_stats)

get_points_total_avg_day(mem_stats)
get_items_total_avg_day(mem_stats)