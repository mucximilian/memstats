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
    
    # Replacing NAs with value from previous row
    memstats$POINTS_TOTAL <- na.locf(memstats$POINTS_TOTAL)
    memstats$ITEMS_TOTAL <- na.locf(memstats$ITEMS_TOTAL)
    memstats$FOLLOWERS <- na.locf(memstats$FOLLOWERS)
    memstats$FOLLOWING <- na.locf(memstats$FOLLOWING)
    
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

get_per_period <- function(stats, period, fun, outdir, plot=TRUE){
    # Plot and return the output of a function applied to the input data on a
    # specified period
    
    stats_per_period <- apply_per_period(stats, period, fun)

    # Plot the data
    if(plot) {
        # Create label sum/mean_per_week/month/quarter/year
        label <- switch(
            as.character(match.call()[4]), # Input function argument as string
            sum = paste("sum_per", period, sep="_"),
            mean = paste("mean_per", period, sep="_")
        )
        
        outdir <- add_to_path(outdir, label, TRUE)
        outdir <- add_to_path(outdir, colnames(stats)[2], TRUE)
        
        create_plot(stats_per_period, outdir)
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

get_cum <- function(stats, outdir, type) {
    # Plot and return columns with the absolute values of points or items
    stats_abs <- switch(
        type,
        points = stats[,c(1,2)],
        items = stats[,c(1,4)]
    )
    
    outdir <- add_to_path(outdir, "cum", TRUE)
    outdir <- add_to_path(outdir, colnames(stats_abs)[2], TRUE)
    
    create_plot(stats_abs, outdir, "graph")
    return(stats_abs)
}

get_abs <- function(stats, outdir, type) {
    # Plot and return columns with the absolute values of points or items
    stats_abs <- switch(
        type,
        points = stats[,c(1,3)],
        items = stats[,c(1,5)]
    )

    outdir <- add_to_path(outdir, "abs", TRUE)
    outdir <- add_to_path(outdir, colnames(stats_abs)[2], TRUE)
    
    create_plot(stats_abs, outdir, "scatterplot")
    return(stats_abs)
}

################################################################################
# Input data splitting functions for periods

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

################################################################################
# Create CSV output
save_as_csv <- function(stats, outdir) {
    # Store stats in period directory
    
    outdir <- add_to_path(outdir, "stats.csv", TRUE)
    filepath <- get_filepath(outdir)
    write.csv(stats, file = filepath, row.names = FALSE)
    
    print(paste("Saving stats", filepath, sep=" "))
}

get_outdir <- function(outdir=NULL) {
    # Creates a list that consists of three lists: 
    # - 1st list: general output directory path 
    # - 2nd list: part of the output path with period specific info used tor the
    #             title of a plot
    # - 3rd list: part of the filepath which is used for the name of the file
    #             and the labeling of the plots.
    #
    # The general output directory path is created with
    # - input directory name if provided
    # - date and time as directory name if not provided
    # - if provided input ends with underscore, date and time is added
    
    dir_output = "output"
    dir.create(dir_output, showWarnings = FALSE)
    
    dir_gen = list(dir_output)
    dir_perdiod = list()
    dir_labels = list()
    
    # Create output directory name from provided name or time string 
    if(!is.null(outdir)) {
        if(grepl("_$", outdir)) {
            # If provided name ends with underscore add time string
            outdir <- paste(outdir, get_time_string(), sep="")
            dir_gen <- c(dir_gen, outdir)
        } else {
            dir_gen <- c(dir_gen, outdir)
        }
    } else {
        dir_gen <- c(dir_gen, get_time_string())
    }
    
    filepath = list(dir_gen, dir_perdiod, dir_labels)

    create_dir(dir_gen)
    
    return(filepath)
}

add_to_path <- function(path, to_add, filename=FALSE) {
    # Appends a string to the second list of which the filepath is constructed
    if(!filename) {
        path[[2]] <- c(path[[2]], to_add)
        create_dir(path)
    } else {
        path[[3]] <- c(path[[3]], to_add)
    }
    return(path)
}

create_dir <- function(outdir) {
    path <- get_path(outdir)
    dir.create(path, showWarnings = FALSE)
}

get_path <- function(outdir) {
    # Construct the path from the outdir lists
    path <- do.call(file.path, as.list(unlist(outdir)))
    return(path)
}

get_filepath <- function(outdir) {
    # Returning a filepath from the outdir list
    filename <- tolower(paste(outdir[[3]], collapse="_"))
    filename <- gsub(" ", "_", filename)
    filepath <- do.call(file.path, c(outdir[[1]], outdir[[2]], filename))
    
    return(filepath)
}

get_time_string <- function() {
    # Returns the current time as YYYYMMDD-HHMM
    time <- strftime(Sys.time(), format="%Y%m%d-%H%M")
    return(time)
} 