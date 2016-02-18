library(xts)
library(ggplot2)
library(scales)

source("functions.R")

################################################################################

plot_weekly <- function(stats_xts, y_type) {
    stats_weekly <- apply.weekly(stats_xts, mean, na.rm=TRUE)
    
    df.stats_weekly <- data.frame(datetime = index(stats_weekly),
                                  stats_weekly[, c(1)],
                                  row.names = NULL)
    
    # Plot the graph
    stats_plot <- ggplot(df.stats_weekly, 
                             aes(x=datetime, y=df.stats_weekly[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Week") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"(average per week)", sep=" "))
    
    plot(stats_plot, paste(tolower(y_type), "total_week", sep="_"))
}

plot_monthly <- function(stats_xts, y_type) {
    stats <- apply.monthly(stats_xts, mean, na.rm=TRUE)
    
    df.stats <- data.frame(datetime = index(stats),
                           stats[, c(1)],
                           row.names = NULL)
    
    # Plot the graph
    stats_plot <- ggplot(df.stats, 
                         aes(x=datetime, y=df.stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Week") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"(average per month)", sep=" "))
    
    plot(stats_plot, paste(tolower(y_type), "total_month", sep="_"))
}

plot_quarterly <- function(stats_xts, y_type) {
    stats <- apply.quarterly(stats_xts, mean, na.rm=TRUE)
    
    df.stats <- data.frame(datetime = index(stats),
                           stats[, c(1)],
                           row.names = NULL)
    
    # Plot the graph
    stats_plot <- ggplot(df.stats, 
                         aes(x=datetime, y=df.stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Month") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"(average per quarter)", sep=" "))
    
    plot(stats_plot, paste(tolower(y_type), "total_quarter", sep="_"))
}

plot_yearly <- function(stats_xts, y_type) {
    stats <- apply.yearly(stats_xts, mean, na.rm=TRUE)
    
    df.stats <- data.frame(datetime = index(stats),
                           stats[, c(1)],
                           row.names = NULL)
    
    # Plot the graph
    stats_plot <- ggplot(df.stats, 
                         aes(x=datetime, y=df.stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Year") +
        scale_x_date(date_breaks = "1 year",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"(average per year)", sep=" "))
    
    plot(stats_plot, paste(tolower(y_type), "total_year", sep="_"))
}

get_items_total_yearly <- function(mem_stats) {
    mem_stats_yearly <- apply.yearly(mem_stats.xts, mean, na.rm=TRUE)
    
    mem_stats_yearly <- data.frame(datetime = index(mem_stats_yearly),
                                   mem_stats_yearly[, c(1)],
                                   row.names = NULL)
    
    # Plot the graph
    mem_stats_plot <- ggplot(mem_stats_yearly, 
                             aes(x=datetime, y=mem_stats_yearly[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Year") +
        scale_x_date(date_breaks = "1 year",
                     labels=date_format("%b %y")) +
        labs(y = "Points") +
        scale_y_continuous(labels = comma) +
        labs(title = "Memrise items (average per year)")
    
    plot(mem_stats_plot, "items_total_year")
}

################################################################################

file <- "input/csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)

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
