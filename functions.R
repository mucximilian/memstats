################################################################################
#
# This script contains the logic for the data subsetting in preparation for the
# actual plotting
#
################################################################################

# Read and format CSV File
get_data <- function(file) {
    memstats <- read.csv(file, header = FALSE, 
                         sep = ",", na.strings = "NULL")
    
    colnames(memstats) <- c(
        "ID",
        "DATE",
        "POINTS_TOTAL",
        "POINTS_DAY",
        "POINTS_MONTH",
        "POINTS_WEEK",
        "ITEMS_TOTAL",
        "FOLLOWERS",
        "FOLLOWING"
    )
    
    # Order by ID and format date
    memstats <- memstats[order(memstats$ID), ]
    memstats$DATE <- as.Date(memstats$DATE , "%Y-%m-%d %H:%M:%S")
    
    # Computing absolute point and item diffs
    memstats$POINTS <- c(NA, memstats[2:nrow(memstats), 3] - memstats[1:(nrow(memstats)-1), 3])
    memstats$ITEMS <- c(NA, memstats[2:nrow(memstats), 7] - memstats[1:(nrow(memstats)-1), 7])
    
    # Replacing NAs with zero
    memstats$POINTS[is.na(memstats$POINTS)] <- 0
    memstats$ITEMS[is.na(memstats$ITEMS)] <- 0
    
    memstats_sub <- memstats[,c(
        "DATE",
        "POINTS_TOTAL",
        "POINTS",
        "ITEMS_TOTAL",
        "ITEMS",
        "FOLLOWERS",
        "FOLLOWING"
    )]
    
    return(memstats_sub)
}

################################################################################
# Functions for data preparation

create_xts_dataframe <- function(df) {
    # Creates a XTS dataframe from any data frame with date and a data column.
    # Requires the date column to be the first column.
    
    df <- xts(df[, c(2)], order.by = df[, c(1)])
    return(df)
}

get_period_subset <- function(memstats, period) {
    # This is only a test
    #
    # TO DO: Necessary to implement the function:
    # period_bounds <- get_period_bounds(period)
    
    memstats_sub <- subset(memstats, DATE >= as.Date("2015-10-15") & DATE <= as.Date("2015-10-20"))
    return(memstats_sub)
}

get_per_period <- function(stats, period, fun, dir){
    # Plot and return the output of a function applied to the input data on 
    # specified period
    
    stats_per_period <- apply_per_period(stats, paste(period, "s", sep=""), fun)
    
    # Create label sum/mean_per_week/month/quarter/year
    label <- switch(
        as.character(match.call()[4]),
        sum = paste(dir, "/", "sum_per_", period, sep=""),
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
    # Plot the cumulative sum of point and item data 
    plot_daily_graph(stats[,c(1,2)], paste(label, "cum", sep="_"), FALSE)
    plot_daily_graph(stats[,c(1,4)], paste(label, "cum", sep="_"), FALSE)
}

get_abs <- function(stats, label, type) {
    # Plot and return the absolute values of point or item data
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
# - Check if possible to reduce redundancy by function nesting

get_total <- function(stats) {
    
    dir_name <- "total"
    
    dir_out <- paste("output/plots", dir_name, sep="/")
    dir.create(dir_out, showWarnings = FALSE)
    
    ############################################################################
    path_daily = paste(dir_name, "daily", sep="/")
    
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
    path_weekly = paste(dir_name, "weekly", sep="/")
    
    # Week sums
    sum_weekly_points <- get_per_period(abs_points, "week", sum, dir_name)
    sum_weekly_items <- get_per_period(abs_items, "week", sum, dir_name)
    
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
    path_monthly = paste(dir_name, "monthly", sep="/")
    
    # Month sums
    sum_monthly_points <- get_per_period(abs_points, "month", sum, dir_name)
    sum_monthly_items <- get_per_period(abs_items, "month", sum, dir_name)
    
    # Month means overall
    mean_monthly_points <- get_mean(sum_monthly_points)
    mean_monthly_items <- get_mean(sum_monthly_items)
    
    # Monthly means per period sum
    get_per_period(sum_monthly_points, "quarter", mean, path_monthly)
    get_per_period(sum_monthly_points, "year", mean, path_monthly)
    
    get_per_period(sum_monthly_items, "quarter", mean, path_monthly)
    get_per_period(sum_monthly_items, "year", mean, path_monthly)
    
    ############################################################################
    path_quarterly = paste(dir_name, "quarterly", sep="/")
    
    # Quarter sums
    sum_quarterly_points <- get_per_period(abs_points, "quarter", sum, dir_name)
    sum_quarterly_items <- get_per_period(abs_items, "quarter", sum, dir_name)
    
    # Quarter means overall
    mean_quarterly_points <- get_mean(sum_quarterly_points)
    mean_quarterly_items <- get_mean(sum_quarterly_items)
    
    # Quarterly means per period 
    get_per_period(sum_quarterly_points, "year", mean, path_quarterly)
    
    get_per_period(sum_quarterly_items, "year", mean, path_quarterly)
    
    ############################################################################
    path_annual = paste(dir_name, "annual", sep="/")
    
    # Year sums
    sum_annual_points <- get_per_period(abs_points, "year", sum, dir_name)
    sum_annual_items <- get_per_period(abs_items, "year", sum, dir_name)
    
    # Year means overall
    mean_annual_points <- get_mean(sum_annual_points)
    mean_annual_items <- get_mean(sum_annual_items)
    
    ############################################################################
    # Followers/-ing
    plot_followersing(stats[,c(1,6,7)], dir_name)
    
    ############################################################################
    # Single values output
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

    save_as_csv(stats_total, dir_name)
}

get_year <- function(stats) {
    
    dir_name <- "year"

    # Get year value and append to dir
    year <- strftime(stats[1,c(1)],format="%Y")
    dir_name <- paste(dir_name, year, sep="/")
    
    dir_out <- paste("output/plots", dir_name, sep="/")
    dir.create(dir_out, showWarnings = FALSE)

    ############################################################################
    path_daily = paste(dir_name, "daily", sep="/")
    
    print(path_daily)

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
    path_weekly = paste(dir_name, "weekly", sep="/")
    
    # Week sums
    sum_weekly_points <- get_per_period(abs_points, "week", sum, dir_name)
    sum_weekly_items <- get_per_period(abs_items, "week", sum, dir_name)
    
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
    path_monthly = paste(dir_name, "monthly", sep="/")
    
    # Month sums
    sum_monthly_points <- get_per_period(abs_points, "month", sum, dir_name)
    sum_monthly_items <- get_per_period(abs_items, "month", sum, dir_name)
    
    # Month means overall
    mean_monthly_points <- get_mean(sum_monthly_points)
    mean_monthly_items <- get_mean(sum_monthly_items)
    
    # Monthly means per period sum
    get_per_period(sum_monthly_points, "quarter", mean, path_monthly)
    
    get_per_period(sum_monthly_items, "quarter", mean, path_monthly)
    
    ############################################################################
    path_quarterly = paste(dir_name, "quarterly", sep="/")
    
    # Quarter sums
    sum_quarterly_points <- get_per_period(abs_points, "quarter", sum, dir_name)
    sum_quarterly_items <- get_per_period(abs_items, "quarter", sum, dir_name)
    
    # Quarter means overall
    mean_quarterly_points <- get_mean(sum_quarterly_points)
    mean_quarterly_items <- get_mean(sum_quarterly_items)
    
    ############################################################################
    # Followers/-ing
    plot_followersing(stats[,c(1,6,7)], dir_name)
    
    ############################################################################
    # Single values output
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
    
    save_as_csv(stats_total, dir_name)
}

get_month <- function(stats) {
    
    dir_name <- "month"
    
    # Get month value and append to dir
    month <- strftime(stats[1,c(1)],format="%Y-%m")
    dir_name <- paste(dir_name, month, sep="/")
    
    dir_out <- paste("output/plots", dir_name, sep="/")
    dir.create(dir_out, showWarnings = FALSE)

    ############################################################################
    path_daily = paste(dir_name, "daily", sep="/")
    
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
    path_weekly = paste(dir_name, "weekly", sep="/")
    
    # Week sums
    sum_weekly_points <- get_per_period(abs_points, "week", sum, dir_name)
    sum_weekly_items <- get_per_period(abs_items, "week", sum, dir_name)
    
    # Week means overall
    mean_weekly_points <- get_mean(sum_weekly_points)
    mean_weekly_items <- get_mean(sum_weekly_items)
    
    ############################################################################
    # Followers/-ing
    plot_followersing(stats[,c(1,6,7)], dir_name)

    ############################################################################
    # Single values output
    stats_total <- data.frame(
        sum_points,
        sum_items,
        mean_daily_points,
        mean_daily_items,
        mean_weekly_points,
        mean_weekly_items
    )
    
    save_as_csv(stats_total, dir_name)
}

get_week <- function(stats) {
    
    dir_name <- "week"
    
    # Get week value and append to dir
    week <- strftime(stats[1,c(1)],format="%Y-%m-%W")
    dir_name <- paste(dir_name, week, sep="/")
    
    dir_out <- paste("output/plots", dir_name, sep="/")
    dir.create(dir_out, showWarnings = FALSE)
    
    ############################################################################
    path_daily = paste(dir_name, "daily", sep="/")
    
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
    # Followers/-ing
    plot_followersing(stats[,c(1,6,7)], dir_name)
    
    ############################################################################
    # Single values output
    stats_total <- data.frame(
        sum_points,
        sum_items,
        mean_daily_points,
        mean_daily_items
    )
    
    save_as_csv(stats_total, dir_name)
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
    
    dir_out_plots <- paste("output/plots", period, sep="/")
    dir.create(dir_out_plots, showWarnings = FALSE)
    
    dir_out_csv <- paste("output/csv", period, sep="/")
    dir.create(dir_out_csv, showWarnings = FALSE)

    switch(
        period,
        year = lapply(stats.split, get_year),
        month = lapply(stats.split, get_month),
        week = lapply(stats.split, get_week)
    )
}

################################################################################
# Create CSV output
save_as_csv <- function(stats, dir) {
    
    # Saving a data frame in the specified directory as CSV file     
    dir_out <- paste("output/csv", dir, sep="/")
    
    # Store stats in period/<period> directory
    #
    # dir.create(dir_out, showWarnings = FALSE)
    # filename <- paste(dir_out, "stats.csv", sep="/")
    
    # Store stats in period directory
    filename <- paste(dir_out, "stats.csv", sep="_")
    write.csv(stats, file = filename, row.names = FALSE)
    
    print(paste("Saving stats", filename, sep=" "))
}