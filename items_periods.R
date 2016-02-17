library(xts)
library(ggplot2)
library(scales)

source("functions.R")

################################################################################

get_items_total_weekly <- function(mem_stats) {
    mem_stats_weekly <- apply.weekly(mem_stats.xts, mean, na.rm=TRUE)
    
    df.mem_stats_weekly <- data.frame(datetime = index(mem_stats_weekly),
                                      mem_stats_weekly[, c(1)],
                                      row.names = NULL)
    
    # Plot the graph
    mem_stats_plot <- ggplot(df.mem_stats_weekly, 
                             aes(x=datetime, y=df.mem_stats_weekly[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Week") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = "Points") +
        scale_y_continuous(labels = comma) +
        labs(title = "Memrise items (average per week)")
    
    plot(mem_stats_plot, "items_total_weekly")
}

get_items_total_monthly <- function(mem_stats) {
    mem_stats_monthly <- apply.monthly(mem_stats.xts, mean, na.rm=TRUE)
    
    mem_stats_monthly <- data.frame(datetime = index(mem_stats_monthly),
                                    mem_stats_monthly[, c(1)],
                                    row.names = NULL)
    
    # Plot the graph
    mem_stats_plot <- ggplot(mem_stats_monthly, 
                             aes(x=datetime, y=mem_stats_monthly[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Month") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = "Points") +
        scale_y_continuous(labels = comma) +
        labs(title = "Memrise items (average per month)")
    
    plot(mem_stats_plot, "items_total_month")
}

get_items_total_quarterly <- function(mem_stats) {
    mem_stats_quarterly <- apply.quarterly(mem_stats.xts, mean, na.rm=TRUE)
    
    mem_stats_quarterly <- data.frame(datetime = index(mem_stats_quarterly),
                                      mem_stats_quarterly[, c(1)],
                                      row.names = NULL)
    
    # Plot the graph
    mem_stats_plot <- ggplot(mem_stats_quarterly, 
                             aes(x=datetime, y=mem_stats_quarterly[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Month") +
        scale_x_date(date_breaks = "1 month",
                     labels=date_format("%b %y")) +
        labs(y = "Points") +
        scale_y_continuous(labels = comma) +
        labs(title = "Memrise items (average per quarter)")
    
    plot(mem_stats_plot, "items_total_quarter")
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

mem_stats.xts <- xts(mem_stats[, c(11)], order.by = mem_stats[, "DATE"])

print(head(mem_stats.xts))

get_items_total_weekly(mem_stats.xts)
get_items_total_monthly(mem_stats.xts)
get_items_total_quarterly(mem_stats.xts)
get_items_total_yearly(mem_stats.xts)