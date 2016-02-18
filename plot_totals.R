library(ggplot2)
library(scales)

source("functions.R")

################################################################################

plot_total_abs <- function(stats, y_type) {
    # Plot the graph
    stats_plot <- ggplot(stats, aes(x=DATE, y=stats[, c(2)])) +
        geom_point(shape=20, size=1, color="grey30") +
        geom_smooth(method=lm) +
        geom_hline(yintercept=mean(stats[, c(2)], na.rm=TRUE),
                   colour="lightblue") +
        labs(x = "Day") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = "Items") +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"per day (all-time)", sep=" "))
    
    plot(stats_plot, paste(tolower(y_type), "total_abs", sep="_"))
}

plot_total_cum <- function(stats, y_type) {
    # Plot the graph
    stats_plot <- ggplot(stats, aes(x=DATE, y=stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Day") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = "Items") +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"cumulative (all-time)", sep="_"))
    
    plot(stats_plot, paste(tolower(y_type), "total_cum", sep="_"))
}

get_points_total_avg_day <- function(mem_stats) {
    points_total_avg <- get_points_abs(mem_stats)
    print(mean(points_total_avg$POINTS_ABS, na.rm=TRUE))
}

get_items_total_avg_day <- function(mem_stats) {
    items_total_avg <- get_items_abs(mem_stats)
    print(mean(items_total_avg$ITEMS_ABS, na.rm=TRUE))
}

################################################################################

file <- "input/csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)

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