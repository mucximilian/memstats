################################################################################
#
# This script contains the logic for the evaluation that precedes the plotting
#
################################################################################

evaluate_stats <- function(stats, period=NULL, out_dir=NULL) {
    if(is.null(period)) {
        get_total(stats)
    } else {
        split_by_period(stats, period)
    }
}

evaluate_period <- function(stats, out_dir, period) {
    
    # Get period name and append to dir
    first_day_of_period <- stats[1,c(1)]
    format <- switch(
        period,
        week = "%Y-%m-%W",
        month = "%Y-%m"
    )
    period_str <- strftime(first_day_of_period, format)
    out_path <- paste(out_dir, period_str, sep="/")
    dir.create(out_path, showWarnings = FALSE)
    
    # Followers/-ing
    path_followersing = paste(out_path, "followersing", sep="/")
    plot_followersing(stats[,c(1,6,7)], path_followersing)
    
    # Print plots and get stats
    stats_abs <- get_stats(stats, out_path)
    
    result_stats <- switch(
        period,
        week = get_week(stats_abs, out_path),
        month = get_month(stats_abs, out_path)
    )
    
    # Adding week as date to stats table
    result_stats <- cbind(first_day_of_period, result_stats)
    
    return(result_stats)
}

get_week <- function(stats, out_path) {
    stats <- get_stats_daily(stats)
    return(stats)
}

get_month <- function(stats, out_path) {
    
    stats <- get_stats_daily(stats, "month")
    
    
    
    
    
    
    
    result_stats <- get_stats_week(stats, out_path)
    
    
    
    #################################
    # Daily means per period
    

    
    #################################
    # Week sums
    sum_weekly_points <- get_per_period(stats[c(1,2)], "week", sum, out_path)
    sum_weekly_items <- get_per_period(stats[c(1,3)], "week", sum, out_path)
    
    # Week means overall
    mean_weekly_points <- mean(sum_weekly_points[,c(2)])
    mean_weekly_items <- mean(sum_weekly_items[,c(2)])
    
    #################################
    # Single values output
    stats_total <- data.frame(
        result_stats,
        mean_weekly_points,
        mean_weekly_items
    )
    
    return(stats_total)
}

get_year <- function(stats, out_path) {
    
    result_stats <- get_stats_month(stats, out_path)
    
    points <- stats[c(1,2)]
    items <- stats[c(1,3)]
    
    #################################
    # Daily means per period
    
    path_daily = paste(out_path, "daily", sep="/")
    
    get_per_period(points, "month", mean, path_daily)
    get_per_period(points, "quarter", mean, path_daily)
    
    get_per_period(items, "month", mean, path_daily)
    get_per_period(items, "quarter", mean, path_daily)
    
    #################################
    # Weekly means per period
    path_weekly = paste(out_path, "weekly", sep="/")
    
    # Weekly means per period
    get_per_period(sum_weekly_points, "month", mean, path_weekly)
    get_per_period(sum_weekly_points, "quarter", mean, path_weekly)
    
    get_per_period(sum_weekly_items, "month", mean, path_weekly)
    get_per_period(sum_weekly_items, "quarter", mean, path_weekly)
    
    #################################
    # Month sums
    path_monthly = paste(dir_name, "monthly", sep="/")
    
    # Month sums
    sum_monthly_points <- get_per_period(abs_points, "month", sum, dir_name)
    sum_monthly_items <- get_per_period(abs_items, "month", sum, dir_name)
    
    # Month means overall
    mean_monthly_points <- mean(sum_monthly_points[,c(2)])
    mean_monthly_items <- mean(sum_monthly_items[,c(2)])
    
    # Monthly means per period sum
    get_per_period(sum_monthly_points, "quarter", mean, path_monthly)
    
    get_per_period(sum_monthly_items, "quarter", mean, path_monthly)
    
    #################################
    path_quarterly = paste(dir_name, "quarterly", sep="/")
    
    # Quarter sums
    sum_quarterly_points <- get_per_period(abs_points, "quarter", sum, dir_name)
    sum_quarterly_items <- get_per_period(abs_items, "quarter", sum, dir_name)
    
    # Quarter means overall
    mean_quarterly_points <- mean(sum_quarterly_points[,c(2)])
    mean_quarterly_items <- mean(sum_quarterly_items[,c(2)])
    
    #################################
    # Single values output
    stats_total <- data.frame(
        result_stats,
        mean_weekly_points,
        mean_weekly_items
    )
    
    return(stats_total)
}

get_stats <- function(stats, out_path) {
    
    path_daily = paste(out_path, "daily", sep="/")
    
    # Cumulative per day
    get_cum(stats, path_daily)
    
    # Absolute per day
    abs_points <- get_abs(stats, path_daily, "points")
    abs_items <- get_abs(stats, path_daily, "items")
    
    stats_abs <- cbind(abs_points, ITEMS=abs_items[,c(2)])
    
    return(stats_abs)
}

get_stats_daily <- function(stats, period=NULL, out_path=NULL) {
    # Day sums
    sum_points <- sum(stats[,c(2)])
    sum_items <- sum(stats[,c(3)])
    
    #################################
    # Day means overall
    mean_daily_points <- mean(stats[,c(2)])
    mean_daily_items <- mean(stats[,c(3)])

    # Create plots
    if(!is.null(period)) {
        if (period == "month") {
            path_daily = paste(out_path, "daily", sep="/") # TO DO get out path
            
            get_per_period(stats[c(1,2)], "week", mean, path_daily)
            get_per_period(stats[c(1,3)], "week", mean, path_daily)
        }
    }
    
    #################################
    # Single values output
    stats_total <- data.frame(
        sum_points,
        sum_items,
        mean_daily_points,
        mean_daily_items
    )
    
    return(stats_total)
}

get_stats_weekly <- function(stats, period) {
    
}

get_stats_monthly <- function(stats, period) {
    
}

get_stats_quarterly <- function(stats, period) {
    
}

get_stats_yearly <- function(stats, period) {
    
}