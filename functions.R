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


get_total <- function(stats) {
    plot_daily_graph(stats[,c(1,2)], "total/cum") # Points cumulative
    plot_daily_graph(stats[,c(1,4)], "total/cum") # Items cumulative

    get_stats_total(stats[,c(1,3)], sum, "total/abs_sum")
    get_stats_total(stats[,c(1,5)], sum, "total/abs_sum")
    get_stats_total(stats[,c(1,3)], mean, "total/daily_mean")
    get_stats_total(stats[,c(1,5)], mean, "total/daily_mean")
}

split_by_year <- function(stats) {
    stats.split <- get_period_splits(stats, "year")
    
    lapply(stats.split, plot_cum)
    lapply(stats.split, get_stats_year, fun=sum)
    lapply(stats.split, get_stats_year, fun=mean)
}