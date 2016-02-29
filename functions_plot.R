library(xts)
library(ggplot2)
library(scales)

source("functions.R")

################################################################################

# Cumulative plot
plot_cum <- function(stats, y_type) {

    # Plot the graph
    stats_plot <- ggplot(stats, aes(x=DATE, y=stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Day") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"cumulative (all-time)", sep=" "))
    
    save_plot(stats_plot, paste(tolower(y_type), "total_cum", sep="_"))
}

################################################################################
# Functions for total plots

plot_daily_abs <- function(stats, y_type, dir="") {
    
    # Plot the graph
    stats_plot <- ggplot(stats, aes(x=DATE, y=stats[, c(2)])) +
        geom_point(shape=20, size=1, color="grey30") +
        geom_smooth(method=lm) +
        geom_hline(yintercept=mean(stats[, c(2)], na.rm=TRUE),
                   colour="lightblue") +
        labs(x = "Day") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"per day (all-time)", sep=" "))
    
    file <- get_filename(dir, y_type, "abs_day")
    save_plot(stats_plot, file)
}

plot_weekly_abs <- function(stats_xts ,y_type, dir="") {
    
    stats <- apply.weekly(stats_xts, sum, na.rm=TRUE)
    
    df.stats <- data.frame(DATE = index(stats), stats[, c(1)], row.names = NULL)
    
    # Plot the graph
    stats_plot <- ggplot(df.stats, aes(x=DATE, y=df.stats[, c(2)])) +
        geom_point(shape=20, size=1, color="grey30") +
        geom_smooth(method=lm, size=0.5) +
        geom_hline(yintercept=mean(df.stats[, c(2)], na.rm=TRUE),
                   colour="lightblue", size=0.5) +
        labs(x = "Day") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise ", y_type," (total per week)", sep=" "))
    
    file <- get_filename(dir, y_type, "abs_week")
    save_plot(stats_plot, file)
}

plot_monthly_abs <- function(stats_xts, y_type, dir="") {
    
    stats <- apply.monthly(stats_xts, sum, na.rm=TRUE)
    
    df.stats <- data.frame(DATE = index(stats), stats[, c(1)], row.names = NULL)

    # Plot the graph
    stats_plot <- ggplot(df.stats, aes(x=DATE, y=df.stats[, c(2)])) +
        geom_point(shape=20, size=1, color="grey30") +
        geom_smooth(method=lm, size=0.5) +
        geom_hline(yintercept=mean(df.stats[, c(2)], na.rm=TRUE),
                   colour="lightblue", size=0.5) +
        labs(x = "Day") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise ", y_type," (total per month)", sep=" "))
    
    file <- get_filename(dir, y_type, "abs_month")
    save_plot(stats_plot, file)
}

plot_quarterly_abs <- function(stats_xts, y_type, dir="") {
    
    stats <- apply.quarterly(stats_xts, sum, na.rm=TRUE)
    
    df.stats <- data.frame(DATE = index(stats), stats[, c(1)], row.names = NULL)

    # Plot the graph
    stats_plot <- ggplot(df.stats, aes(x=DATE, y=df.stats[, c(2)])) +
        geom_point(shape=20, size=1, color="grey30") +
        geom_smooth(method=lm, size=0.5) +
        geom_hline(yintercept=mean(df.stats[, c(2)], na.rm=TRUE),
                   colour="lightblue", size=0.5) +
        labs(x = "Day") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise ", y_type," (total per quarter)", sep=" "))
    
    file <- get_filename(dir, y_type, "abs_quarter")
    save_plot(stats_plot, file)
}

plot_yearly_abs <- function(stats_xts, y_type, dir="") {
    
    stats <- apply.yearly(stats_xts, sum, na.rm=TRUE)
    
    df.stats <- data.frame(DATE = index(stats), stats[, c(1)], row.names = NULL)

    # Plot the graph
    stats_plot <- ggplot(df.stats, aes(x=DATE, y=df.stats[, c(2)])) +
        geom_point(shape=20, size=1, color="grey30") +
        geom_smooth(method=lm, size=0.5) +
        geom_hline(yintercept=mean(df.stats[, c(2)], na.rm=TRUE),
                   colour="lightblue", size=0.5) +
        labs(x = "Day") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise ", y_type," (total per year)", sep=" "))
    
    file <- get_filename(dir, y_type, "abs_year")
    save_plot(stats_plot, file)
}

################################################################################
# Functions for period plots
#
# The following functions plot the daily average  of the second column of a 
# dataframe per period, which can be 
#
# - week
# - month
# - quarter
# - year

plot_weekly_avg <- function(stats_xts, y_type) {
    
    stats <- apply.weekly(stats_xts, mean, na.rm=TRUE)
    
    df.stats <- data.frame(DATE = index(stats), stats[, c(1)], row.names = NULL)
    
    # Plot the graph
    stats_plot <- ggplot(df.stats, aes(x=DATE, y=df.stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Week") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"(average per week)", sep=" "))
    
    save_plot(stats_plot, paste(tolower(y_type), "avg_week", sep="_"))
}

plot_monthly_avg <- function(stats_xts, y_type) {
    stats <- apply.monthly(stats_xts, mean, na.rm=TRUE)
    
    df.stats <- data.frame(DATE = index(stats), stats[, c(1)], row.names = NULL)
    
    # Plot the graph
    stats_plot <- ggplot(df.stats, aes(x=DATE, y=df.stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Week") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"(average per month)", sep=" "))
    
    save_plot(stats_plot, paste(tolower(y_type), "avg_month", sep="_"))
}


plot_quarterly_avg <- function(stats_xts, y_type) {
    stats <- apply.quarterly(stats_xts, mean, na.rm=TRUE)
    
    df.stats <- data.frame(DATE = index(stats), stats[, c(1)], row.names = NULL)
    
    # Plot the graph
    stats_plot <- ggplot(df.stats, aes(x=DATE, y=df.stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Month") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"(average per quarter)", sep=" "))
    
    save_plot(stats_plot, paste(tolower(y_type), "avg_quarter", sep="_"))
}

plot_yearly_avg <- function(stats_xts, y_type) {
    stats <- apply.yearly(stats_xts, mean, na.rm=TRUE)
    
    df.stats <- data.frame(DATE = index(stats), stats[, c(1)], row.names = NULL)
    
    # Plot the graph
    stats_plot <- ggplot(df.stats, aes(x=DATE, y=df.stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Year") +
        scale_x_date(date_breaks = "1 year",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"(average per year)", sep=" "))
    
    save_plot(stats_plot, paste(tolower(y_type), "avg_year", sep="_"))
}

################################################################################

plot_followersing <- function(stats) {
    
    followersing <- get_followersing()
    
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
    
    save_plot(mem_stats_plot, "followersing")
}

################################################################################

per_month <- function(stats_xts, y_type, fun) {
    
    stats <- apply.monthly(stats_xts, fun, na.rm=TRUE)
    
    df.stats <- data.frame(DATE = index(stats), stats[, c(1)], row.names = NULL)
    
    plot_month(df.stats, y_type)
}
    
plot_month <- function(df, y_type) {
    # Plot the graph
    stats_plot <- ggplot(df, aes(x=DATE, y=df[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Week") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"(average per month)", sep=" "))
    
    save_plot(stats_plot, paste(tolower(y_type), "avg_month", sep="_"))
}

plot_week <- function(df, y_type) {
    # Plot the graph
    stats_plot <- ggplot(df, aes(x=DATE, y=df[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        labs(x = "Week") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = y_type) +
        scale_y_continuous(labels = comma) +
        labs(title = paste("Memrise", y_type,"(average per week)", sep=" "))
    
    save_plot(stats_plot, paste(tolower(y_type), "avg_week", sep="_"))
}