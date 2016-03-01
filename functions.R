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

get_stats_total <- function(stats, fun, label){
    
    print(head(stats))
    
    per_year <- get_per_period(stats, "year", fun, paste(label, "per_year", sep="_"))
    
    get_stats_year(stats, fun, label)
}

get_stats_year <- function(stats, fun, label){
    per_quarter <- get_per_period(stats, "quarter", fun, paste(label, "per_quarter", sep="_"))
    per_month <- get_per_period(stats, "month", fun, paste(label, "per_month", sep="_"))
    
    get_stats_month(stats, fun, label)
}

get_stats_month <- function(stats, fun, label){
    per_week <- get_per_period(stats, "week", fun, paste(label, "per_week", sep="_"))
    
    get_stats_week(stats, fun, label)
}

get_stats_week <- function(stats, fun, label){
    per_day <- get_per_day(stats, paste(label, "per_day", sep="_"))
}

get_per_day <- function(stats, label) {
    plot_daily_scatterplot(stats, label)
}

################################################################################

get_cum <- function(stats, label) {
    plot_daily_graph(stats[,c(1,2)], paste(label, "cum", sep="/")) # Points
    plot_daily_graph(stats[,c(1,4)], paste(label, "cum", sep="/")) # Items
}

get_abs <- function(stats, label) {
    plot_daily_scatterplot(stats, label)
}


get_total <- function(stats) {
    
    abs_points <- stats[,c(1,3)]
    abs_items <- stats[,c(1,5)]
    
    ############################################################################
    # Cumulative per day
    get_cum(stats[,c(1,2,4)], "total")
    
    # Absolute per day
    get_abs(abs_points, "total")
    get_abs(abs_items, "total")
    
    # Daily means
    get_mean_daily_week(abs_points, "total")
    get_mean_daily_month(abs_points, "total")
    get_mean_daily_quarter(abs_points, "total")
    get_mean_daily_year(abs_points, "total")
    
    get_mean_daily_week(abs_items, "total")
    get_mean_daily_month(abs_items, "total")
    get_mean_daily_quarter(abs_items, "total")
    get_mean_daily_year(abs_items, "total")
    
    mean_daily_points <- get_mean(abs_points)
    mean_daily_items <- get_mean(abs_items)
    
    ############################################################################
    # Week sums
    sum_weekly_points <- get_sum_weekly(abs_points, "total")
    sum_weekly_items <- get_sum_weekly(abs_items, "total")
    
    # Weekly means
    get_mean_weekly_month(sum_weekly_points, "total")
    get_mean_weekly_quarter(sum_weekly_points, "total")
    get_mean_weekly_year(sum_weekly_points, "total")
    
    get_mean_weekly_month(sum_weekly_items, "total")
    get_mean_weekly_quarter(sum_weekly_items, "total")
    get_mean_weekly_year(sum_weekly_items, "total")
    
    mean_weekly_points <- get_mean(sum_weekly_points)
    mean_weekly_items <- get_mean(sum_weekly_items)
    
    ############################################################################
    # Month sums
    sum_monthly_points <- get_sum_monthly(abs_points, "total")
    sum_monthly_items <- get_sum_monthly(abs_items, "total")
    
    # Monthly means
    get_mean_monthly_quarter(sum_monthly_points, "total")
    get_mean_monthly_year(sum_monthly_points, "total")
    
    get_mean_monthl_quarter(sum_monthly_items, "total")
    get_mean_monthly_year(sum_monthly_items, "total")
    
    mean_monthly_points <- get_mean(sum_monthly_points)
    mean_monthly_items <- get_mean(sum_monthly_items)
    
    ############################################################################
    # Quarter sums
    sum_quarterly_points <- get_sum_quarterly(abs_points, "total")
    sum_quarterly_items <- get_sum_quarterly(abs_items, "total")
    
    # Quarterly means
    get_mean_quarterly_year(sum_quarterly_points, "total")
    
    get_mean_quarterly_year(sum_quarterly_items, "total")
    
    mean_quarterly_points <- get_mean(sum_quarterly_points)
    mean_quarterly_items <- get_mean(sum_quarterly_items)
    
    ############################################################################
    # Year sums
    sum_yearly_points <- get_sum_yearly(abs_points, "total")
    sum_yearly_points <- get_sum_yearly(abs_items, "total")
    
    # Year means
    mean_yearly_points <- get_mean(sum_yearly_points, "total")
    mean_yearly_items <- get_mean(sum_yearly_points, "total")
}

get_year <- function(stats) {
    
    abs_points <- stats[,c(1,3)]
    abs_items <- stats[,c(1,5)]
    
    ############################################################################
    # Cumulative per day
    get_cum(stats[,c(1,2,4)], "year")
    
    # Absolute per day
    get_abs(abs_points, "year")
    get_abs(abs_items, "year")
    
    # Daily means
    get_mean_daily_week(abs_points, "year")
    get_mean_daily_month(abs_points, "year")
    get_mean_daily_quarter(abs_points, "year")
    
    get_mean_daily_week(abs_items, "year")
    get_mean_daily_month(abs_items, "year")
    get_mean_daily_quarter(abs_items, "year")
    
    ############################################################################
    # Week sums
    sum_weekly_points <- get_sum_weekly(abs_points, "year")
    sum_weekly_items <- get_sum_weekly(abs_items, "year")
    
    # Weekly means
    get_mean_weekly_month(sum_weekly_points, "year")
    get_mean_weekly_quarter(sum_weekly_points, "year")
    
    get_mean_weekly_month(sum_weekly_items, "year")
    get_mean_weekly_quarter(sum_weekly_items, "year")
    
    ############################################################################
    # Month sums
    sum_monthly_points <- get_sum_monthly(abs_points, "year")
    sum_monthly_items <- get_sum_monthly(abs_items, "year")
    
    # Monthly means
    get_mean_monthly_quarter(sum_monthly_points, "year")
    
    get_mean_monthl_quarter(sum_monthly_items, "year")
    
    ############################################################################
    # Quarter sums
    sum_quarterly_points <- get_sum_quarterly(abs_points, "year")
    sum_quarterly_items <- get_sum_quarterly(abs_items, "year")
}

get_month <- function(stats) {
    
    abs_points <- stats[,c(1,3)]
    abs_items <- stats[,c(1,5)]
    
    ############################################################################
    # Cumulative per day
    get_cum(stats[,c(1,2,4)], "year")
    
    # Absolute per day
    get_abs(abs_points, "year")
    get_abs(abs_items, "year")
    
    # Daily means
    get_mean_daily_week(abs_points, "year")
    
    get_mean_daily_week(abs_items, "year")
    
    ############################################################################
    # Week sums
    sum_weekly_points <- get_sum_weekly(abs_points, "year")
    sum_weekly_items <- get_sum_weekly(abs_items, "year")
}

get_week <- function(stats) {
    
    abs_points <- stats[,c(1,3)]
    abs_items <- stats[,c(1,5)]
    
    ############################################################################
    # Cumulative per day
    get_cum(stats[,c(1,2,4)], "year")
    
    # Absolute per day
    get_abs(abs_points, "year")
    get_abs(abs_items, "year")
}

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