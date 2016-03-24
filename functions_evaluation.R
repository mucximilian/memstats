################################################################################
#
# This script contains the logic for the evaluation that precedes the plotting
#
################################################################################

evaluate_stats <- function(stats, period, out_dir=NULL) {
    
    out_dir <- get_out_dir(out_dir)
    
    out_dir <- paste(out_dir, period, sep="/")
    dir.create(out_dir, showWarnings = FALSE)

    # Evaluate overall stats or split the input data by period
    results <- NULL
    if(period =="total") {
        # Evaluate total data
        stats <- stats[complete.cases(stats),] # TO DO Remove not replace
        results <- evaluate_period(stats, out_dir, "total")
    } else {
        # Split first, then evaluate
        results <- split_by_period(stats, period, out_dir)
    }
    save_as_csv(results, out_dir, period)
}

evaluate_stats_full <- function(stats, out_dir=NULL) {
    
    out_dir <- get_out_dir(out_dir)
    
    evaluate_stats(memstats, "total", out_dir)
    evaluate_stats(memstats, "year", out_dir)
    evaluate_stats(memstats, "month", out_dir)
    evaluate_stats(memstats, "week", out_dir)
}

evaluate_period <- function(stats, out_dir, period) {
    
    if(!period=="total") {
    
        # Get period name and append to dir
        first_day_of_period <- stats[1,c(1)]
        format <- switch(
            period,
            week = "%Y-%m-%W",
            month = "%Y-%m",
            year = "%Y"
        )
        period_str <- strftime(first_day_of_period, format)
        out_dir <- paste(out_dir, period_str, sep="/")
        dir.create(out_dir, showWarnings = FALSE)
    }
    
    # Followers/-ing
    get_followersing(stats[,c(1,6,7)], out_dir)
    
    # Print plots and get stats
    stats_abs <- get_stats(stats, out_dir)

    result_stats <- switch(
        period,
        week = get_week(stats_abs, out_dir),
        month = get_month(stats_abs, out_dir),
        year = get_year(stats_abs, out_dir),
        total = get_total(stats_abs, out_dir)
    )

    if(!period=="total") {
        # Adding period as date to stats table
        result_stats <- cbind(first_day_of_period, result_stats)
    }

    return(result_stats)
}

get_followersing <- function(stats, out_path) {
    path_followersing = paste(out_path, "followersing", sep="/")
    plot_followersing(stats, path_followersing)
}

get_week <- function(stats, out_path) {
    stats <- get_stats_daily(stats, out_path)
    return(stats)
}

get_month <- function(stats, out_path) {
    
    stats_daily <- get_stats_daily(stats, out_path, "month")
    stats_weekly <- get_stats_weekly(stats, out_path)

    # Single values output
    stats_total <- data.frame(
        stats_daily,
        stats_weekly
    )
    
    return(stats_total)
}

get_year <- function(stats, out_path) {
    
    stats_daily <- get_stats_daily(stats, out_path, "year")
    stats_weekly <- get_stats_weekly(stats, out_path, "year")
    stats_monthly <- get_stats_monthly(stats, out_path, "year")
    stats_quarterly <- get_stats_quarterly(stats, out_path)
    
    # Single values output
    stats_total <- data.frame(
        stats_daily,
        stats_weekly,
        stats_monthly,
        stats_quarterly
    )
    
    return(stats_total)
}

get_total <- function(stats, out_path) {
    
    stats_daily <- get_stats_daily(stats, out_path, "total")
    stats_weekly <- get_stats_weekly(stats, out_path, "total")
    stats_monthly <- get_stats_monthly(stats, out_path, "total")
    stats_quarterly <- get_stats_quarterly(stats, out_path, "total")
    stats_yearly <- get_stats_yearly(stats, out_path)
    
    # Single values output
    stats_total <- data.frame(
        stats_daily,
        stats_weekly,
        stats_monthly,
        stats_quarterly,
        stats_yearly
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

get_stats_daily <- function(stats, out_path, period=NULL) {

    # Day sums
    sum_points <- sum(stats[,c(2)])
    sum_items <- sum(stats[,c(3)])
    
    # Day means overall
    mean_daily_points <- mean(stats[,c(2)])
    mean_daily_items <- mean(stats[,c(3)])

    # Create plots
    get_stats_daily_month <- function(stats, path_daily) {

        get_per_period(stats[c(1,2)], "week", mean, path_daily)
        get_per_period(stats[c(1,3)], "week", mean, path_daily)
    }
    get_stats_daily_year <- function(stats, path_daily) {

        get_stats_daily_month(stats, path_daily)
        
        get_per_period(stats[c(1,2)], "month", mean, path_daily)
        get_per_period(stats[c(1,3)], "month", mean, path_daily)
        
        get_per_period(stats[c(1,2)], "quarter", mean, path_daily)
        get_per_period(stats[c(1,3)], "quarter", mean, path_daily)
    }
    get_stats_daily_total <- function(stats, path_daily) {

        get_stats_daily_year(stats, path_daily)
        
        get_per_period(stats[c(1,2)], "year", mean, path_daily)
        get_per_period(stats[c(1,3)], "year", mean, path_daily)
    }
    if(!is.null(period)) {
        path_daily = paste(out_path, "daily", sep="/")

        switch(
            period,
            month = get_stats_daily_month(stats, path_daily),
            year = get_stats_daily_year(stats, path_daily),
            total = get_stats_daily_total(stats, path_daily)
        )
    }

        # Single values output
    stats_total <- data.frame(
        sum_points,
        sum_items,
        mean_daily_points,
        mean_daily_items
    )

    return(stats_total)
}

get_stats_weekly <- function(stats, out_path, period=NULL) {

    # Week sums
    sum_weekly_points <- get_per_period(stats[c(1,2)], "week", sum, out_path)
    sum_weekly_items <- get_per_period(stats[c(1,3)], "week", sum, out_path)
    
    # Week means overall
    mean_weekly_points <- mean(sum_weekly_points[,c(2)])
    mean_weekly_items <- mean(sum_weekly_items[,c(2)])
    
    # Create plots
    get_stats_weekly_year <- function(stats, path_weekly) {
        get_per_period(stats[c(1,2)], "month", mean, path_weekly)
        get_per_period(stats[c(1,3)], "month", mean, path_weekly)
        
        get_per_period(stats[c(1,2)], "quarter", mean, path_weekly)
        get_per_period(stats[c(1,3)], "quarter", mean, path_weekly)
    }
    get_stats_weekly_total <- function(stats, path_weekly) {
        get_stats_weekly_year(stats, path_weekly)

        get_per_period(stats[c(1,2)], "year", mean, path_weekly)
        get_per_period(stats[c(1,3)], "year", mean, path_weekly)
    }
    if(!is.null(period)) {
        path_weekly = paste(out_path, "weekly", sep="/")
        switch(
            period,
            year = get_stats_weekly_year(stats, path_weekly),
            total = get_stats_weekly_total(stats, path_weekly)
        )
    }
    
    # Single values output
    stats_total <- data.frame(
        mean_weekly_points,
        mean_weekly_items
    )
    
    return(stats_total)
}

get_stats_monthly <- function(stats, out_path, period=NULL) {
    
    # Month sums
    sum_monthly_points <- get_per_period(stats[c(1,2)], "month", sum, out_path)
    sum_monthly_items <- get_per_period(stats[c(1,3)], "month", sum, out_path)
    
    # Month means overall
    mean_monthly_points <- mean(sum_monthly_points[,c(2)])
    mean_monthly_items <- mean(sum_monthly_items[,c(2)])
    
    # Create plots
    get_stats_monthly_year <- function(stats, path_monthly) {
        get_per_period(stats[c(1,2)], "quarter", mean, path_monthly)
        get_per_period(stats[c(1,3)], "quarter", mean, path_monthly)
    }
    get_stats_monthly_total <- function(stats, path_monthly) {
        get_stats_monthly_year(stats, path_monthly)
        
        get_per_period(stats[c(1,2)], "year", mean, path_monthly)
        get_per_period(stats[c(1,3)], "year", mean, path_monthly)
    }
    if(!is.null(period)) {
        path_monthly = paste(out_path, "monthly", sep="/")
        switch(
            period,
            year = get_stats_monthly_year(stats, path_monthly),
            total = get_stats_monthly_total(stats, path_monthly)
        )
    }
    
    # Single values output
    stats_total <- data.frame(
        mean_monthly_points,
        mean_monthly_items
    )
    
    return(stats_total)
}

get_stats_quarterly <- function(stats, out_path, period=NULL) {
    
    # Quarter sums
    sum_quarterly_points <- get_per_period(stats[c(1,2)], "quarter", sum, out_path)
    sum_quarterly_items <- get_per_period(stats[c(1,3)], "quarter", sum, out_path)
    
    # Quarter means overall
    mean_quarterly_points <- mean(sum_quarterly_points[,c(2)])
    mean_quarterly_items <- mean(sum_quarterly_items[,c(2)])
    
    if(!is.null(period)) {
        path_quarterly = paste(out_path, "quarterly", sep="/")
        
        get_per_period(sum_quarterly_points, "year", mean, path_quarterly)
        get_per_period(sum_quarterly_items, "year", mean, path_quarterly)
    }
    
    # Single values output
    stats_total <- data.frame(
        mean_quarterly_points,
        mean_quarterly_items
    )
    
    return(stats_total)
    
}

get_stats_yearly <- function(stats, out_path) {

    # Year sums
    sum_annual_points <- get_per_period(stats[c(1,2)], "year", sum, out_path)
    sum_annual_items <- get_per_period(stats[c(1,3)], "year", sum, out_path)
    
    # Year means overall
    mean_annual_points <- mean(sum_annual_points[,c(2)])
    mean_annual_items <- mean(sum_annual_items[,c(2)])
    
    # Single values output
    stats_total <- data.frame(
        mean_annual_points,
        mean_annual_items
    )
    
    return(stats_total)
}