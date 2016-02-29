################################################################################
# All functions operate on the 'mem_stats' data frame which is imported in the 
# 'setup.R script'

create_xts_dataframe <- function(col) {
    # Creates a XTS dataframe with a data and a date column. Requires a 'DATE'
    # column in the input data frame
    
    mem_stats.xts <- xts(mem_stats[, c(col)], order.by = mem_stats[, "DATE"])
    return(mem_stats.xts)
}

split_by_period <- function(period) {
    # Splits the mem_stats data frame into a list of data frames by a provided
    # time period which cann be 'week', 'month' or 'year'
    
    split_period <- switch(period,
                           week = "%W",
                           month = "%m",
                           year = "%Y")
    
    tab <- split(mem_stats, format(mem_stats$DATE, split_period))
    return(tab)
}

# Data subsets

get_followersing <- function() {
    followersing <- mem_stats[, c(2, 8, 9)]
    return(followersing)
}

get_period_subset <- function(mem_stats, period) {
    period_bounds <- get_period_bounds(period)
    mem_stats_sub <- subset(mem_stats, DATE >= as.Date("2015-10-15") & DATE <= as.Date("2015-10-20"))
    return(mem_stats_sub)
}

################################################################################

get_cum <- function(col) {
    mem_stats_points_tot <- mem_stats[, c(2, col)]
    return(mem_stats_points_tot)
}

get_abs <- function(col) {
    mem_stats_abs <- mem_stats[, c(2, col)]
    return(mem_stats_abs)
}

get_total_day <- function(fun, col) {
    print(fun(mem_stats[, c(col)], na.rm=TRUE))
}