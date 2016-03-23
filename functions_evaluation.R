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
        week = get_stats_week(stats_abs, out_path),
        month = get_stats_month(stats_abs, out_path)
    )
    
    # Adding week as date to stats table
    result_stats <- cbind(first_day_of_period, result_stats)
    
    return(result_stats)
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

get_stats_week <- function(stats, out_path) {
    
    # Day sums
    sum_points <- sum(stats[,c(2)])
    sum_items <- sum(stats[,c(3)])
    
    # Day means overall
    mean_daily_points <- mean(stats[,c(2)])
    mean_daily_items <- mean(stats[,c(3)])
    
    ############################################################################
    # Single values output
    stats_total <- data.frame(
        sum_points,
        sum_items,
        mean_daily_points,
        mean_daily_items
    )
    
    return(stats_total)
}

get_stats_month <- function(stats, out_path) {
    
    result_stats <- get_stats_week(stats, out_path)
    
    points <- stats[c(1,2)]
    items <- stats[c(1,3)]
    
    # Daily means per period
    path_daily = paste(out_path, "daily", sep="/")
    get_per_period(points, "week", mean, path_daily)
    get_per_period(items, "week", mean, path_daily)
    
    ############################################################################
    path_weekly = paste(out_path, "weekly", sep="/")
    
    # Week sums
    sum_weekly_points <- get_per_period(points, "week", sum, out_path)
    sum_weekly_items <- get_per_period(items, "week", sum, out_path)
    
    # Week means overall
    mean_weekly_points <- mean(sum_weekly_points[,c(2)])
    mean_weekly_items <- mean(sum_weekly_items[,c(2)])
    
    ############################################################################
    # Single values output
    stats_total <- data.frame(
        result_stats,
        mean_weekly_points,
        mean_weekly_items
    )
    
    return(stats_total)
}