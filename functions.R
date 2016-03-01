get_filename <- function(dir, a, b) {
    return(paste(dir, paste(tolower(a), b, sep="_"), sep =""))
}

get_y_label <- function(colname) {
    colname <- gsub("_", " ", tolower(colname))
    y_label <- unlist(strsplit(colname, " "))[1]
    return(y_label)
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

get_per_period <- function(stats, period, fun, label){
    
    colnames_in = colnames(stats)
    
    stats.xts <- create_xts_dataframe(stats)

    stats_period <- switch(period,
                    year = get_per_year(stats.xts, fun),
                    quarter = get_per_quarter(stats.xts, fun),
                    month = get_per_month(stats.xts, fun),
                    week = get_per_week(stats.xts, fun)
    )
    
    stats_period.idx <- data.frame(
        DATE = index(stats_period),
        stats_period[, c(1)], 
        row.names = NULL
    )
    
    colnames(stats_period.idx) <- colnames_in
    
    plot_daily_graph(stats_period.idx, label)
    
    return(stats_period.idx)
}

get_per_year <- function(stats.xts, fun) {
    return(apply.yearly(stats.xts, fun, na.rm=TRUE))
}

get_per_quarter <- function(stats.xts, fun) {
    return(apply.quarterly(stats.xts, fun, na.rm=TRUE))
}

get_per_month <- function(stats.xts, fun) {
    return(apply.monthly(stats.xts, fun, na.rm=TRUE))
}

get_per_week <- function(stats.xts, fun) {
    return(apply.weekly(stats.xts, fun, na.rm=TRUE))
}

################################################################################
# Data handling functions

get_cum <- function(stats, label) {
    plot_daily_graph(stats[,c(1,2)], paste(label, "cum", sep="_")) # Points
    plot_daily_graph(stats[,c(1,4)], paste(label, "cum", sep="_")) # Items
}

get_abs <- function(stats, label) {
    plot_daily_scatterplot(stats, paste(label, "abs", sep="_"))
}

get_mean <- function(stats) {
    return(mean(stats[,c(2)]))
}

get_sum <- function(stats){
    return(sum(stats[,c(2)]))
}

# The following function can all be replaced by 'get_per_period'

get_mean_weekly <- function(stats, dir) {
    get_per_period(stats, "week", mean, paste(dir, "mean_per_week", sep="_"))
}
get_mean_monthly <- function(stats, dir) {
    get_per_period(stats, "month", mean, paste(dir, "mean_per_month", sep="_"))
}
get_mean_quarterly <- function(stats, dir) {
    get_per_period(stats, "quarter", mean, paste(dir, "mean_per_quarter", sep="_"))
}
get_mean_yearly <- function(stats, dir){
    get_per_period(stats, "year", mean, paste(dir, "mean_per_year", sep="_"))
}

get_sum_weekly <- function(stats, dir){
    sum_weekly <- get_per_period(stats, "week", sum, paste(dir, "sum", sep="_"))
    return(sum_weekly)
}
get_sum_monthly <- function(stats, dir){
    sum_monthly <- get_per_period(stats, "month", sum, paste(dir, "sum", sep="_"))
    return(sum_monthly)
}
get_sum_quarterly <- function(stats, dir){
    sum_quarterly <- get_per_period(stats, "quarter", sum, paste(dir, "sum", sep="_"))
    return(sum_quarterly)
}
get_sum_yearly <- function(stats, dir){
    sum_yearly <- get_per_period(stats, "year", sum, paste(dir, "sum", sep="_"))
    return(sum_yearly)
    
}

################################################################################
# Data processing functions

get_total <- function(stats) {
    
    dir <- "total"
    
    abs_points <- stats[,c(1,3)]
    abs_items <- stats[,c(1,5)]
    
    ############################################################################
    dir_daily = paste(dir, "daily", sep="/")
    
    # Cumulative per day
    get_cum(stats, dir_daily)
    
    # Absolute per day
    get_abs(abs_points, dir_daily)
    get_abs(abs_items, dir_daily)
    
    # Sums
    sum_points <- get_sum(abs_points)
    sum_items <- get_sum(abs_items)
    
    # Daily means
    get_mean_weekly(abs_points, dir_daily)
    get_mean_monthly(abs_points, dir_daily)
    get_mean_quarterly(abs_points, dir_daily)
    get_mean_yearly(abs_points, dir_daily)
    
    get_mean_weekly(abs_items, dir_daily)
    get_mean_monthly(abs_items, dir_daily)
    get_mean_quarterly(abs_items, dir_daily)
    get_mean_yearly(abs_items, dir_daily)
    
    mean_daily_points <- get_mean(abs_points)
    mean_daily_items <- get_mean(abs_items)
    
    ############################################################################
    dir_weekly = paste(dir, "weekly", sep="/")
    
    # Week sums
    sum_weekly_points <- get_sum_weekly(abs_points, dir_weekly)
    sum_weekly_items <- get_sum_weekly(abs_items, dir_weekly)
    
    # Weekly means
    get_mean_monthly(sum_weekly_points, dir_weekly)
    get_mean_quarterly(sum_weekly_points, dir_weekly)
    get_mean_yearly(sum_weekly_points, dir_weekly)
    
    get_mean_monthly(sum_weekly_items, dir_weekly)
    get_mean_quarterly(sum_weekly_items, dir_weekly)
    get_mean_yearly(sum_weekly_items, dir_weekly)
    
    mean_weekly_points <- get_mean(sum_weekly_points)
    mean_weekly_items <- get_mean(sum_weekly_items)
    
    ############################################################################
    dir_monthly = paste(dir, "monthly", sep="/")
    
    # Month sums
    sum_monthly_points <- get_sum_monthly(abs_points, dir_monthly)
    sum_monthly_items <- get_sum_monthly(abs_items, dir_monthly)
    
    # Monthly means
    get_mean_quarterly(sum_monthly_points, dir_monthly)
    get_mean_yearly(sum_monthly_points, dir_monthly)
    
    get_mean_quarterly(sum_monthly_items, dir_monthly)
    get_mean_yearly(sum_monthly_items, dir_monthly)
    
    mean_monthly_points <- get_mean(sum_monthly_points)
    mean_monthly_items <- get_mean(sum_monthly_items)
    
    ############################################################################
    dir_quarterly = paste(dir, "quarterly", sep="/")
    
    # Quarter sums
    sum_quarterly_points <- get_sum_quarterly(abs_points, dir_quarterly)
    sum_quarterly_items <- get_sum_quarterly(abs_items, dir_quarterly)
    
    # Quarterly means
    get_mean_quarterly(sum_quarterly_points, dir_quarterly)
    
    get_mean_quarterly(sum_quarterly_items, dir_quarterly)
    
    mean_quarterly_points <- get_mean(sum_quarterly_points)
    mean_quarterly_items <- get_mean(sum_quarterly_items)
    
    ############################################################################
    dir_yearly = paste(dir, "yearly", sep="/")
    
    # Year sums
    sum_yearly_points <- get_sum_yearly(abs_points, dir_yearly)
    sum_yearly_items <- get_sum_yearly(abs_items, dir_yearly)
    
    # Year means
    mean_yearly_points <- get_mean(sum_yearly_points)
    mean_yearly_items <- get_mean(sum_yearly_items)
    
    print(mean_yearly_points)
    print(mean_yearly_items)
}

get_year <- function(stats) {
    
    dir <- "year"
    
    abs_points <- stats[,c(1,3)]
    abs_items <- stats[,c(1,5)]
    
    ############################################################################
    # Cumulative per day
    get_cum(stats[,c(1,2,4)], dir)
    
    # Absolute per day
    get_abs(abs_points, dir)
    get_abs(abs_items, dir)
    
    # Daily means
    get_mean_daily_week(abs_points, dir)
    get_mean_daily_month(abs_points, dir)
    get_mean_daily_quarter(abs_points, dir)
    
    get_mean_daily_week(abs_items, dir)
    get_mean_daily_month(abs_items, dir)
    get_mean_daily_quarter(abs_items, dir)
    
    ############################################################################
    # Week sums
    sum_weekly_points <- get_sum_weekly(abs_points, dir)
    sum_weekly_items <- get_sum_weekly(abs_items, dir)
    
    # Weekly means
    get_mean_weekly_month(sum_weekly_points, dir)
    get_mean_weekly_quarter(sum_weekly_points, dir)
    
    get_mean_weekly_month(sum_weekly_items, dir)
    get_mean_weekly_quarter(sum_weekly_items, dir)
    
    ############################################################################
    # Month sums
    sum_monthly_points <- get_sum_monthly(abs_points, dir)
    sum_monthly_items <- get_sum_monthly(abs_items, dir)
    
    # Monthly means
    get_mean_monthly_quarter(sum_monthly_points, dir)
    
    get_mean_monthl_quarter(sum_monthly_items, dir)
    
    ############################################################################
    # Quarter sums
    sum_quarterly_points <- get_sum_quarterly(abs_points, dir)
    sum_quarterly_items <- get_sum_quarterly(abs_items, dir)
}

get_month <- function(stats) {
    
    dir <- "month"
    
    abs_points <- stats[,c(1,3)]
    abs_items <- stats[,c(1,5)]
    
    ############################################################################
    # Cumulative per day
    get_cum(stats[,c(1,2,4)], dir)
    
    # Absolute per day
    get_abs(abs_points, dir)
    get_abs(abs_items, dir)
    
    # Daily means
    get_mean_daily_week(abs_points, dir)
    
    get_mean_daily_week(abs_items, dir)
    
    ############################################################################
    # Week sums
    sum_weekly_points <- get_sum_weekly(abs_points, dir)
    sum_weekly_items <- get_sum_weekly(abs_items, dir)
}

get_week <- function(stats) {
    
    dir <- "week"
    
    abs_points <- stats[,c(1,3)]
    abs_items <- stats[,c(1,5)]
    
    ############################################################################
    # Cumulative per day
    get_cum(stats[,c(1,2,4)], dir)
    
    # Absolute per day
    get_abs(abs_points, dir)
    get_abs(abs_items, dir)
}

################################################################################
# Splitting functions

split_by_year <- function(stats) {
    stats.split <- get_period_splits(stats, "year")
    
    get_year(stats)
}

split_by_month <- function(stats) {
    stats.split <- get_period_splits(stats, "month")
    
    get_month(stats)
}

split_by_year <- function(stats) {
    stats.split <- get_period_splits(stats, "week")
    
    get_week(stats)
}