################################################################################
#
# This script contains the logic for the data subsetting in preparation for the
# actual plotting
#
################################################################################

# Read and format CSV File
get_data <- function(file) {
    
    filepath <- paste("input", file, sep="/")
    
    memstats <- read.csv(filepath, header = FALSE, 
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

get_per_period <- function(stats, period, fun, dir, plot=TRUE){
    # Plot and return the output of a function applied to the input data on 
    # specified period
    
    stats_per_period <- apply_per_period(stats, period, fun)

    # Plot the data
    if(plot) {
        # Create label sum/mean_per_week/month/quarter/year
        label <- switch(
            as.character(match.call()[4]),
            sum = paste(dir, "/", "sum_per_", period, sep=""),
            mean = paste(dir, "mean_per", period, sep="_")
        )
        
        plot_graph(stats_per_period, label)
    }

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
        stats.xts, endpoints(stats.xts, on = paste(period, "s", sep=""), 1), 
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
    plot_graph(stats[,c(1,2)], paste(label, "cum", sep="_"), FALSE)
    plot_graph(stats[,c(1,4)], paste(label, "cum", sep="_"), FALSE)
}

get_abs <- function(stats, label, type) {
    # Plot and return columns with the absolute values of points or items
    stats_abs <- switch(
        type,
        points = stats[,c(1,3)],
        items = stats[,c(1,5)]
    )
    
    plot_daily_scatterplot(stats_abs, paste(label, "abs", sep="_"))
    return(stats_abs)
}

################################################################################
# Input data splitting functions for periods

# TO Do:
# Add last week/month/year function

get_period_splits <- function(df, period, col=1) {
    # Splits a data frame into a list of data frames by a provided time period 
    # which cann be 'week', 'month' or 'year'.
    # Date column is the first column by default.
    
    split_period <- switch(
        period,
        week = "%Y%W",
        month = "%Y%m",
        year = "%Y"
    )
    
    df.split <- split(df, format(df[, c(col)], split_period))

    return(df.split)
}

split_by_period <- function(stats, period, out_dir) {
    # Splits a data frame by a given period and calls the corresponding function
    # for further processing
    
    stats.split <- get_period_splits(stats, period)

    # Collect the results in a list of data frames
    results <- switch(
        period,
        year = lapply(stats.split, evaluate_period, out_dir=out_dir, period=period),
        month = lapply(stats.split, evaluate_period, out_dir=out_dir, period=period),
        week = lapply(stats.split, evaluate_period, out_dir=out_dir, period=period)
    )

    # Combine multiple data frames from list in single data frame
    results.df <- ldply(results, data.frame, .id = NULL)
    results.df <- results.df[order(results.df[1], decreasing = TRUE),]
    
    results.df <- switch(
        period,
        week = data.frame(
            strftime(results.df[,1], format="%Y-%m-%W"),
            results.df[2:5]
        ),
        month = data.frame(
            strftime(results.df[,1], format="%Y-%m"),
            results.df[2:7]
        ),
        year = data.frame(
            strftime(results.df[,1], format="%Y"),
            results.df[2:11]
        )
    )
    colnames(results.df)[1] <- period
    
    return(results.df)
}

################################################################################
# Create CSV output
save_as_csv <- function(stats, out_dir, period) {

    # Store stats in period directory
    filename <- paste(period, "stats.csv", sep="_")
    filepath <- paste(out_dir, filename, sep="/")
    write.csv(stats, file = filepath, row.names = FALSE)
    
    print(paste("Saving stats", filepath, sep=" "))
}

get_out_dir <- function(out_dir=NULL) {
    # Create output directory with the options
    # - added if provided
    # - date and time if not provided
    # - if provided and end with _ add date and time
    dir_output = "output"
    dir.create(dir_output, showWarnings = FALSE)
    if(!is.null(out_dir)) {
        if(!grepl("^output", out_dir)) {
            if(grepl("_$", out_dir)) {
                out_dir <- paste(out_dir, get_time_string(), sep="")
                out_dir <- paste(dir_output, out_dir, sep="/")
            } else {
                out_dir <- paste(dir_output, out_dir, sep="/")
            }
        }
    } else {
        out_dir <- paste("output", get_time_string(), sep="/")
    }
    dir.create(out_dir, showWarnings = FALSE)
    
    return(out_dir)
}

get_time_string <- function() {
    time <- strftime(Sys.time(), format="%Y%m%d-%H%M")
    return(time)
} 