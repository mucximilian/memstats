library(xts)
library(ggplot2)
library(scales)

source("functions.R")

################################################################################
# Functions for total plots

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

################################################################################
# Functions for period plots

plot_weekly_averages <- function(stats_xts, y_type) {
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
    
    save_plot(stats_plot, paste(tolower(y_type), "total_week", sep="_"))
}

plot_monthly_averages <- function(stats_xts, y_type) {
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
    
    save_plot(stats_plot, paste(tolower(y_type), "total_month", sep="_"))
}

plot_quarterly_averages <- function(stats_xts, y_type) {
    stats <- apply.quarterly(stats_xts, mean, na.rm=TRUE)
    
    df.stats <- data.frame(DATE = index(stats),
                           stats[, c(1)],
                           row.names = NULL)
    
    # Plot the graph
    stats_plot <- ggplot(df.stats, 
                         aes(x=DATE, y=df.stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Month") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"(average per quarter)", sep=" "))
    
    save_plot(stats_plot, paste(tolower(y_type), "total_quarter", sep="_"))
}

plot_yearly_averages <- function(stats_xts, y_type) {
    stats <- apply.yearly(stats_xts, mean, na.rm=TRUE)
    
    df.stats <- data.frame(DATE = index(stats),
                           stats[, c(1)],
                           row.names = NULL)
    
    # Plot the graph
    stats_plot <- ggplot(df.stats, 
                         aes(x=DATE, y=df.stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Year") +
        scale_x_date(date_breaks = "1 year",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"(average per year)", sep=" "))
    
    save_plot(stats_plot, paste(tolower(y_type), "total_year", sep="_"))
}

################################################################################

plot_followersing <- function(stats) {
    
    followersing <- get_followersing(stats)
    
    print(head(followersing))
    
    # Plot the graph
    mem_stats_plot <- ggplot() +
        geom_line(data=followersing, aes(x=DATE, y=FOLLOWERS, color="green")) +
        geom_line(data=followersing, aes(x=DATE, y=FOLLOWING, color="blue")) +
        scale_color_manual(name="", labels=c("Following", "Followers"), values = c("blue", "green")) +
        labs(x = "Day") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = "People") +
        scale_y_continuous(labels = comma) +
        labs(title = "Memrise followers and following")
    
    plot(mem_stats_plot, "followersing")
}
