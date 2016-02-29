# Read and format CSV File
get_data <- function(file) {
    mem_stats <- read.csv(file, header = FALSE, 
                          sep = ",", na.strings = "NULL")
    
    colnames(mem_stats) <- c(
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
    mem_stats <- mem_stats[order(mem_stats$ID), ]
    mem_stats$DATE <- as.Date(mem_stats$DATE , "%Y-%m-%d %H:%M:%S")
    
    # Computing absolute point and item diffs
    mem_stats$POINTS_ABS <- c(NA, mem_stats[2:nrow(mem_stats), 3] - mem_stats[1:(nrow(mem_stats)-1), 3])
    mem_stats$ITEMS_ABS <- c(NA, mem_stats[2:nrow(mem_stats), 7] - mem_stats[1:(nrow(mem_stats)-1), 7])
    
    # Replacing NAs with zero
    mem_stats$POINTS_ABS[is.na(mem_stats$POINTS_ABS)] <- 0
    mem_stats$ITEMS_ABS[is.na(mem_stats$ITEMS_ABS)] <- 0
    
    return(mem_stats)
}



get_filename <- function(dir, a, b) {
    return(paste(dir, paste(tolower(a), b, sep="_"), sep =""))
}

create_xts_dataframe <- function(df) {
    # Creates a XTS dataframe from any data frame with date and a data column.
    # Requires the date column to be the first column.
    
    df <- xts(df[, c(2)], order.by = df[, c(1)])
    return(df)
}

################################################################################

get_period_splits <- function(df, period, col=1) {
    # Splits a data frame into a list of data frames by a provided time period 
    # which cann be 'week', 'month' or 'year'.
    # Date column is the first column by default.
    
    split_period <- switch(period,
                           week = "%W",
                           month = "%m",
                           year = "%Y")
    
    tab <- split(df, format(df[, c(col)], split_period))
    return(tab)
}

get_per_period <- function(stats, period, fun){
    stats.xts <- create_xts_dataframe(stats)
    stats <- switch(period,
                    year = apply.yearly(stats.xts, fun, na.rm=TRUE),
                    quarter = apply.quarterly(stats.xts, fun, na.rm=TRUE),
                    month = apply.monthly(stats.xts, fun, na.rm=TRUE),
                    week = apply.weekly(stats.xts, fun, na.rm=TRUE)
                    )
    
    print(head(stats))
}

get_stats_total <- function(stats, fun){
    get_per_period(stats, "year", fun)
    get_stats_year(stats, fun)
}

get_stats_year <- function(stats, fun){
    get_per_period(stats, "quarter", fun)
    get_per_period(stats, "month", fun)
    get_stats_month(stats, fun)
}

get_stats_month <- function(stats, fun){
    get_per_period(stats, "week", fun)
    get_stats_week(stats, fun)
}

get_stats_week <- function(stats, fun){
    get_per_day(stats, fun)
}

get_total <- function(stats){
    get_stats_total(stats, sum)
    get_stats_total(stats, mean)
}

get_cum <- function(stats){
    get_stats_total(stats, sum)
}