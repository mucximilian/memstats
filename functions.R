################################################################################
#
# This script contains the logic for the data subsetting in preparation for the
# actual plotting
#
################################################################################
# Functions for data preparation

create_xts_dataframe <- function(df) {
    # Creates a XTS dataframe from any data frame with date and a data column.
    # Requires the date column to be the first column.
    
    df <- xts(df[, c(2)], order.by = df[, c(1)])
    return(df)
}

get_per_period <- function(stats, period, fun, dir){
    
    stats_per_period <- apply_per_period(stats, paste(period, "s", sep=""), fun)
    
    # Create label sum/mean_per_week/month/quarter/year
    label <- switch(
        as.character(match.call()[4]),
        sum = paste(dir, "sum_per_", period, sep=""),
        mean = paste(dir, "mean_per", period, sep="_")
    )
    
    # Plot the data
    plot_daily_graph(stats_per_period, label)

    return(stats_per_period)
}

apply_per_period <- function(stats, period, fun) {
    # Applies a function (sum, mean) on per provided period on a XTS data frame
    #
    # Source: http://www.noamross.net/blog/2013/2/6/xtsmarkdown.html
    
    # Store initial column names (required before return)
    colnames_in = colnames(stats)
    
    # Create a XTS data frame for the periodically applied functions
    stats.xts <- create_xts_dataframe(stats)
    
    # If there are two records for a day (i.e. in case of any malfunction of the
    # data retrieval) get only the day with the hightest value to avoid pitfalls
    stats.xts <- apply.daily(stats.xts, pmax, na.rm=TRUE)
    
    # OLD
    # stats_period <- switch(
    #     period,
    #     year = apply.yearly(stats.xts, fun, na.rm=TRUE),
    #     quarter = apply.quarterly(stats.xts, fun, na.rm=TRUE),
    #     month = apply.monthly(stats.xts, fun, na.rm=TRUE),
    #     week = apply.weekly(stats.xts, fun, na.rm=TRUE)
    # )

    stats_period <- period.apply(
        stats.xts, endpoints(stats.xts, on = period, 1), 
        function(x) apply(x, 2, fun)
    )
    
    # Create an index on the date column
    stats_period.idx <- data.frame(
        DATE = index(stats_period),
        stats_period[, c(1)], 
        row.names = NULL
    )
    
    # Set column names from input
    colnames(stats_period.idx) <- colnames_in
    
    return(stats_period.idx)
}

get_cum <- function(stats, label) {
    plot_daily_graph(stats[,c(1,2)], paste(label, "cum", sep="_"), FALSE) # Points
    plot_daily_graph(stats[,c(1,4)], paste(label, "cum", sep="_"), FALSE) # Items
}

get_abs <- function(stats, label, type) {
    
    stats_abs <- switch(
        type,
        points = stats[,c(1,3)],
        items = stats[,c(1,5)]
    )
    
    plot_daily_scatterplot(stats_abs, paste(label, "abs", sep="_"))
    return(stats_abs)
}

get_mean <- function(stats) {
    return(mean(stats[,c(2)]))
}

get_sum <- function(stats){
    return(sum(stats[,c(2)]))
}

################################################################################
# Input data processing functions for period
#
# TO DOs:
# - Reducing redundance by function nesting

get_total <- function(stats) {
    
    dir <- "total/"
    
    out_dir <- paste("output/plots", dir, sep="/")
    dir.create(out_dir, showWarnings = FALSE)
    
    ############################################################################
    path_daily = paste(dir, "daily", sep="")
    
    # Cumulative per day
    get_cum(stats, path_daily)
    
    # Absolute per day
    abs_points <- get_abs(stats, path_daily, "points")
    abs_items <- get_abs(stats, path_daily, "items")
    
    # Day sums
    sum_points <- get_sum(abs_points)
    sum_items <- get_sum(abs_items)
    
    # Day means overall
    mean_daily_points <- get_mean(abs_points)
    mean_daily_items <- get_mean(abs_items)
    
    # Daily means per period
    get_per_period(abs_points, "week", mean, path_daily)
    get_per_period(abs_points, "month", mean, path_daily)
    get_per_period(abs_points, "quarter", mean, path_daily)
    get_per_period(abs_points, "year", mean, path_daily)
    
    get_per_period(abs_items, "week", mean, path_daily)
    get_per_period(abs_items, "month", mean, path_daily)
    get_per_period(abs_items, "quarter", mean, path_daily)
    get_per_period(abs_items, "year", mean, path_daily)
    
    ############################################################################
    path_weekly = paste(dir, "weekly", sep="")
    
    # Week sums
    sum_weekly_points <- get_per_period(abs_points, "week", sum, dir)
    sum_weekly_items <- get_per_period(abs_items, "week", sum, dir)
    
    # Week means overall
    mean_weekly_points <- get_mean(sum_weekly_points)
    mean_weekly_items <- get_mean(sum_weekly_items)
    
    # Weekly means per period
    get_per_period(sum_weekly_points, "month", mean, path_weekly)
    get_per_period(sum_weekly_points, "quarter", mean, path_weekly)
    get_per_period(sum_weekly_points, "year", mean, path_weekly)
    
    get_per_period(sum_weekly_items, "month", mean, path_weekly)
    get_per_period(sum_weekly_items, "quarter", mean, path_weekly)
    get_per_period(sum_weekly_items, "year", mean, path_weekly)
    
    ############################################################################
    path_monthly = paste(dir, "monthly", sep="")
    
    # Month sums
    sum_monthly_points <- get_per_period(abs_points, "month", sum, dir)
    sum_monthly_items <- get_per_period(abs_items, "month", sum, dir)
    
    # Month means overall
    mean_monthly_points <- get_mean(sum_monthly_points)
    mean_monthly_items <- get_mean(sum_monthly_items)
    
    # Monthly means per period sum
    get_per_period(sum_monthly_points, "quarter", mean, path_monthly)
    get_per_period(sum_monthly_points, "year", mean, path_monthly)
    
    get_per_period(sum_monthly_items, "quarter", mean, path_monthly)
    get_per_period(sum_monthly_items, "year", mean, path_monthly)
    
    ############################################################################
    path_quarterly = paste(dir, "quarterly", sep="")
    
    # Quarter sums
    sum_quarterly_points <- get_per_period(abs_points, "quarter", sum, dir)
    sum_quarterly_items <- get_per_period(abs_items, "quarter", sum, dir)
    
    # Quarter means overall
    mean_quarterly_points <- get_mean(sum_quarterly_points)
    mean_quarterly_items <- get_mean(sum_quarterly_items)
    
    # Quarterly means per period 
    get_per_period(sum_quarterly_points, "year", mean, path_quarterly)
    
    get_per_period(sum_quarterly_items, "year", mean, path_quarterly)
    
    ############################################################################
    path_annual = paste(dir, "annual", sep="")
    
    # Year sums
    sum_annual_points <- get_per_period(abs_points, "year", sum, dir)
    sum_annual_items <- get_per_period(abs_items, "year", sum, dir)
    
    # Year means overall
    mean_annual_points <- get_mean(sum_annual_points)
    mean_annual_items <- get_mean(sum_annual_items)
    
    ############################################################################
    # Single values output
    #
    # TO DOs:
    # - Save to CSV
    
    stats_total <- data.frame(
        sum_points,
        sum_items,
        mean_daily_points,
        mean_daily_items,
        mean_weekly_points,
        mean_weekly_items,
        mean_monthly_points,
        mean_monthly_items,
        mean_quarterly_points,
        mean_quarterly_items,
        mean_annual_points,
        mean_annual_items
    )
    
    print(stats_total)
    
    ############################################################################
    # Folwoers/-ing
    plot_followersing(stats[,c(1,6,7)], dir)
}

get_year <- function(stats) {
    
    dir <- "year"

    # Get year value and append to dir
    year <- strftime(stats[1,c(1)],format="%Y/")
    dir <- paste(dir, year, sep="/")
    
    out_dir <- paste("output/plots", dir, sep="/")
    dir.create(out_dir, showWarnings = FALSE)

    ############################################################################
    path_daily = paste(dir, "daily", sep="")
    
    # Cumulative per day
    get_cum(stats, path_daily)
    
    # Absolute per day
    abs_points <- get_abs(stats, path_daily, "points")
    abs_items <- get_abs(stats, path_daily, "items")
    
    # Day sums
    sum_points <- get_sum(abs_points)
    sum_items <- get_sum(abs_items)
    
    # Day means overall
    mean_daily_points <- get_mean(abs_points)
    mean_daily_items <- get_mean(abs_items)
    
    # Daily means per period
    get_per_period(abs_points, "week", mean, path_daily)
    get_per_period(abs_points, "month", mean, path_daily)
    get_per_period(abs_points, "quarter", mean, path_daily)
    
    get_per_period(abs_items, "week", mean, path_daily)
    get_per_period(abs_items, "month", mean, path_daily)
    get_per_period(abs_items, "quarter", mean, path_daily)
    
    ############################################################################
    path_weekly = paste(dir, "weekly", sep="")
    
    # Week sums
    sum_weekly_points <- get_per_period(abs_points, "week", sum, dir)
    sum_weekly_items <- get_per_period(abs_items, "week", sum, dir)
    
    # Week means overall
    mean_weekly_points <- get_mean(sum_weekly_points)
    mean_weekly_items <- get_mean(sum_weekly_items)
    
    # Weekly means per period
    get_per_period(sum_weekly_points, "month", mean, path_weekly)
    get_per_period(sum_weekly_points, "quarter", mean, path_weekly)
    
    get_per_period(sum_weekly_items, "month", mean, path_weekly)
    get_per_period(sum_weekly_items, "quarter", mean, path_weekly)
    
    ############################################################################
    # Month sums
    path_monthly = paste(dir, "monthly", sep="")
    
    # Month sums
    sum_monthly_points <- get_per_period(abs_points, "month", sum, dir)
    sum_monthly_items <- get_per_period(abs_items, "month", sum, dir)
    
    # Month means overall
    mean_monthly_points <- get_mean(sum_monthly_points)
    mean_monthly_items <- get_mean(sum_monthly_items)
    
    # Monthly means per period sum
    get_per_period(sum_monthly_points, "quarter", mean, path_monthly)
    
    get_per_period(sum_monthly_items, "quarter", mean, path_monthly)
    
    ############################################################################
    path_quarterly = paste(dir, "quarterly", sep="")
    
    # Quarter sums
    sum_quarterly_points <- get_per_period(abs_points, "quarter", sum, dir)
    sum_quarterly_items <- get_per_period(abs_items, "quarter", sum, dir)
    
    # Quarter means overall
    mean_quarterly_points <- get_mean(sum_quarterly_points)
    mean_quarterly_items <- get_mean(sum_quarterly_items)
    
    ############################################################################
    # Single values output
    #
    # TO DOs:
    # - Save to CSV
    
    stats_total <- data.frame(
        sum_points,
        sum_items,
        mean_daily_points,
        mean_daily_items,
        mean_weekly_points,
        mean_weekly_items,
        mean_monthly_points,
        mean_monthly_items,
        mean_quarterly_points,
        mean_quarterly_items
    )
    
    print(stats_total)
    
    ############################################################################
    # Folwoers/-ing
    plot_followersing(stats[,c(1,6,7)], dir)
}

get_month <- function(stats) {
    
    dir <- "month"
    
    # Get month value and append to dir
    month <- strftime(stats[1,c(1)],format="%Y-%m/")
    dir <- paste(dir, month, sep="/")
    
    out_dir <- paste("output/plots", dir, sep="/")
    dir.create(out_dir, showWarnings = FALSE)

    ############################################################################
    path_daily = paste(dir, "daily", sep="")
    
    # Cumulative per day
    get_cum(stats, path_daily)
    
    # Absolute per day
    abs_points <- get_abs(stats, path_daily, "points")
    abs_items <- get_abs(stats, path_daily, "items")
    
    # Day sums
    sum_points <- get_sum(abs_points)
    sum_items <- get_sum(abs_items)
    
    # Day means overall
    mean_daily_points <- get_mean(abs_points)
    mean_daily_items <- get_mean(abs_items)
    
    # Daily means per period
    get_per_period(abs_points, "week", mean, path_daily)

    get_per_period(abs_items, "week", mean, path_daily)

    ############################################################################
    path_weekly = paste(dir, "weekly", sep="")
    
    # Week sums
    sum_weekly_points <- get_per_period(abs_points, "week", sum, dir)
    sum_weekly_items <- get_per_period(abs_items, "week", sum, dir)
    
    # Week means overall
    mean_weekly_points <- get_mean(sum_weekly_points)
    mean_weekly_items <- get_mean(sum_weekly_items)

    ############################################################################
    # Single values output
    #
    # TO DOs:
    # - Save to CSV
    
    stats_total <- data.frame(
        sum_points,
        sum_items,
        mean_daily_points,
        mean_daily_items,
        mean_weekly_points,
        mean_weekly_items
    )
    
    print(stats_total)
    
    ############################################################################
    # Folwoers/-ing
    plot_followersing(stats[,c(1,6,7)], dir)
}

get_week <- function(stats) {
    
    dir <- "week"
    
    # Get week value and append to dir
    week <- strftime(stats[1,c(1)],format="%Y-%m-%W/")
    dir <- paste(dir, week, sep="/")
    
    out_dir <- paste("output/plots", dir, sep="/")
    dir.create(out_dir, showWarnings = FALSE)
    
    ############################################################################
    path_daily = paste(dir, "daily", sep="")
    
    # Cumulative per day
    get_cum(stats, path_daily)
    
    # Absolute per day
    abs_points <- get_abs(stats, path_daily, "points")
    abs_items <- get_abs(stats, path_daily, "items")
    
    # Day sums
    sum_points <- get_sum(abs_points)
    sum_items <- get_sum(abs_items)
    
    # Day means overall
    mean_daily_points <- get_mean(abs_points)
    mean_daily_items <- get_mean(abs_items)
    
    ############################################################################
    # Single values output
    #
    # TO DOs:
    # - Save to CSV
    
    stats_total <- data.frame(
        sum_points,
        sum_items,
        mean_daily_points,
        mean_daily_items
    )
    
    print(stats_total)
    
    ############################################################################
    # Folwoers/-ing
    plot_followersing(stats[,c(1,6,7)], dir)
}

################################################################################
# Input data splitting functions for periods

get_period_splits <- function(df, period, col=1) {
    # Splits a data frame into a list of data frames by a provided time period 
    # which cann be 'week', 'month' or 'year'.
    # Date column is the first column by default.
    
    split_period <- switch(
        period,
        week = "%W",
        month = "%m",
        year = "%Y"
    )
    
    tab <- split(df, format(df[, c(col)], split_period))
    return(tab)
}

split_by_period <- function(stats, period) {
    # Splits a data frame by a given period and calls the corresponding function
    # for processing
    stats.split <- get_period_splits(stats, period)
    
    out_dir <- paste("output/plots", period, sep="/")
    dir.create(out_dir, showWarnings = FALSE)

    switch(
        period,
        year = lapply(stats.split, get_year),
        month = lapply(stats.split, get_month),
        week = lapply(stats.split, get_week)
    )
}