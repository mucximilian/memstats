################################################################################
#
# This script contains the logic for the evaluation that precedes the plotting
#
################################################################################

evaluate_stats <- function(stats, period, outdir=NULL, create_outdir=TRUE) {
    # Performs an evaluation on the chosen period (week, month, year or total)
    
    if(create_outdir) {
        outdir <- get_outdir(outdir)
    }
    outdir <- add_to_path(outdir, period)

    # Evaluate overall stats or split the input data by period
    results <- NULL
    if(period =="total") {
        # TO DO: Check if this can be removed due to NA check in 'get_data'
        stats <- stats[complete.cases(stats),]
        
        results <- evaluate_period(stats, outdir, "total")
    } else {
        # Split first, then evaluate
        results <- evaluate_period_splits(stats, period, outdir)
    }
    print("saving CSV")
    save_as_csv(results, outdir)
}

evaluate_stats_latest <- function(stats, period, type, outdir=NULL) {
    # Performs an evaluation of the current or last (type) week or month (period)
    
    outdir <- paste(outdir, type, sep="")
    outdir <- get_outdir(outdir)
    outdir <- add_to_path(outdir, period)
    
    # Split first, then evaluate
    stats.split <- get_period_splits(stats, period)
    latest <- switch(
        type,
        current = last(stats.split)[[1]], # Current period
        last = stats.split[length(stats.split)-1][[1]] # Last complete period
    )
    result <- evaluate_period(latest, outdir, period)
    
    save_as_csv(result, outdir, period)
}

evaluate_stats_full <- function(stats, outdir=NULL) {
    # Performs an evaluation of all periods
    
    outdir <- get_outdir(outdir)
    
    evaluate_stats(memstats, "total", outdir, FALSE)
    evaluate_stats(memstats, "year", outdir, FALSE)
    evaluate_stats(memstats, "month", outdir, FALSE)
    evaluate_stats(memstats, "week", outdir, FALSE)
}

evaluate_period <- function(stats, outdir, period) {

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
        outdir <- add_to_path(outdir, period_str)
    }
    
    # Followers/-ing
    get_followersing(stats[,c(1,6,7)], outdir)
    
    # Print plots and get stats
    stats_abs <- get_stats(stats, outdir)

    result_stats <- switch(
        period,
        week = get_week(stats_abs, outdir),
        month = get_month(stats_abs, outdir),
        year = get_year(stats_abs, outdir),
        total = get_total(stats_abs, outdir)
    )

    if(!period=="total") {
        # Adding period as date to stats table
        result_stats <- cbind(first_day_of_period, result_stats)
    }

    return(result_stats)
}

evaluate_period_splits <- function(stats, period, outdir) {
    # Splits a data frame by a given period and calls the corresponding function
    # for further processing
    
    stats.split <- get_period_splits(stats, period)
    
    # Collect the results in a list of data frames
    results <- switch(
        period,
        year = lapply(stats.split, evaluate_period, outdir=outdir, period=period),
        month = lapply(stats.split, evaluate_period, outdir=outdir, period=period),
        week = lapply(stats.split, evaluate_period, outdir=outdir, period=period)
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

get_followersing <- function(stats, outdir) {
    outdir <- add_to_path(outdir, "followersing", TRUE)
    plot_followersing(stats, outdir)
}

get_week <- function(stats, outdir) {
    stats <- get_stats_daily(stats, outdir)
    return(stats)
}

get_month <- function(stats, outdir) {
    
    stats_daily <- get_stats_daily(stats, outdir, "month")
    stats_weekly <- get_stats_weekly(stats, outdir)

    # Single values output
    stats_total <- data.frame(
        stats_daily,
        stats_weekly
    )
    
    return(stats_total)
}

get_year <- function(stats, outdir) {
    
    stats_daily <- get_stats_daily(stats, outdir, "year")
    stats_weekly <- get_stats_weekly(stats, outdir, "year")
    stats_monthly <- get_stats_monthly(stats, outdir, "year")
    stats_quarterly <- get_stats_quarterly(stats, outdir)
    
    # Single values output
    stats_total <- data.frame(
        stats_daily,
        stats_weekly,
        stats_monthly,
        stats_quarterly
    )
    
    return(stats_total)
}

get_total <- function(stats, outdir) {
    
    stats_daily <- get_stats_daily(stats, outdir, "total")
    stats_weekly <- get_stats_weekly(stats, outdir, "total")
    stats_monthly <- get_stats_monthly(stats, outdir, "total")
    stats_quarterly <- get_stats_quarterly(stats, outdir, "total")
    stats_yearly <- get_stats_yearly(stats, outdir)
    
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

get_stats <- function(stats, outdir) {
    
    outdir <- add_to_path(outdir, "daily", TRUE)

    # Cumulative per day
    get_cum(stats, outdir, "points")
    get_cum(stats, outdir, "items")
    
    # Absolute per day
    abs_points <- get_abs(stats, outdir, "points")
    abs_items <- get_abs(stats, outdir, "items")
    
    stats_abs <- cbind(abs_points, ITEMS=abs_items[,c(2)])
    
    return(stats_abs)
}

################################################################################

get_stats_daily <- function(stats, outdir, period=NULL) {

    # Day sums
    sum_points <- sum(stats[,c(2)])
    sum_items <- sum(stats[,c(3)])
    
    # Day means overall
    mean_daily_points <- mean(stats[,c(2)])
    mean_daily_items <- mean(stats[,c(3)])

    # Create plots
    get_stats_daily_month <- function(stats, outdir) {
        get_per_period(stats[c(1,2)], "week", mean, outdir)
        get_per_period(stats[c(1,3)], "week", mean, outdir)
    }
    get_stats_daily_year <- function(stats, outdir) {

        get_stats_daily_month(stats, outdir)
        
        get_per_period(stats[c(1,2)], "month", mean, outdir)
        get_per_period(stats[c(1,3)], "month", mean, outdir)
        
        get_per_period(stats[c(1,2)], "quarter", mean, outdir)
        get_per_period(stats[c(1,3)], "quarter", mean, outdir)
    }
    get_stats_daily_total <- function(stats, outdir) {

        get_stats_daily_year(stats, outdir)
        
        get_per_period(stats[c(1,2)], "year", mean, outdir)
        get_per_period(stats[c(1,3)], "year", mean, outdir)
    }
    if(!is.null(period)) {
        outdir <- add_to_path(outdir, "daily", TRUE)

        switch(
            period,
            month = get_stats_daily_month(stats, outdir),
            year = get_stats_daily_year(stats, outdir),
            total = get_stats_daily_total(stats, outdir)
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

get_stats_weekly <- function(stats, outdir, period=NULL) {

    # Week sums
    sum_weekly_points <- get_per_period(stats[c(1,2)], "week", sum, outdir)
    sum_weekly_items <- get_per_period(stats[c(1,3)], "week", sum, outdir)
    
    # Week means overall
    mean_weekly_points <- mean(sum_weekly_points[,c(2)])
    mean_weekly_items <- mean(sum_weekly_items[,c(2)])
    
    # Create plots
    get_stats_weekly_year <- function(stats, outdir) {
        get_per_period(stats[c(1,2)], "month", mean, outdir)
        get_per_period(stats[c(1,3)], "month", mean, outdir)
        
        get_per_period(stats[c(1,2)], "quarter", mean, outdir)
        get_per_period(stats[c(1,3)], "quarter", mean, outdir)
    }
    get_stats_weekly_total <- function(stats, outdir) {
        get_stats_weekly_year(stats, outdir)

        get_per_period(stats[c(1,2)], "year", mean, outdir)
        get_per_period(stats[c(1,3)], "year", mean, outdir)
    }
    if(!is.null(period)) {
        outdir <- add_to_path(outdir, "weekly", TRUE)
        switch(
            period,
            year = get_stats_weekly_year(stats, outdir),
            total = get_stats_weekly_total(stats, outdir)
        )
    }
    
    # Single values output
    stats_total <- data.frame(
        mean_weekly_points,
        mean_weekly_items
    )
    
    return(stats_total)
}

get_stats_monthly <- function(stats, outdir, period=NULL) {
    
    # Month sums
    sum_monthly_points <- get_per_period(stats[c(1,2)], "month", sum, outdir)
    sum_monthly_items <- get_per_period(stats[c(1,3)], "month", sum, outdir)
    
    # Month means overall
    mean_monthly_points <- mean(sum_monthly_points[,c(2)])
    mean_monthly_items <- mean(sum_monthly_items[,c(2)])
    
    # Create plots
    get_stats_monthly_year <- function(stats, outdir) {
        get_per_period(stats[c(1,2)], "quarter", mean, outdir)
        get_per_period(stats[c(1,3)], "quarter", mean, outdir)
    }
    get_stats_monthly_total <- function(stats, outdir) {
        get_stats_monthly_year(stats, outdir)
        
        get_per_period(stats[c(1,2)], "year", mean, outdir)
        get_per_period(stats[c(1,3)], "year", mean, outdir)
    }
    if(!is.null(period)) {
        outdir <- add_to_path(outdir, "monthly", TRUE)
        switch(
            period,
            year = get_stats_monthly_year(stats, outdir),
            total = get_stats_monthly_total(stats, outdir)
        )
    }
    
    # Single values output
    stats_total <- data.frame(
        mean_monthly_points,
        mean_monthly_items
    )
    
    return(stats_total)
}

get_stats_quarterly <- function(stats, outdir, period=NULL) {
    
    # Quarter sums
    sum_quarterly_points <- get_per_period(stats[c(1,2)], "quarter", sum, outdir)
    sum_quarterly_items <- get_per_period(stats[c(1,3)], "quarter", sum, outdir)
    
    # Quarter means overall
    mean_quarterly_points <- mean(sum_quarterly_points[,c(2)])
    mean_quarterly_items <- mean(sum_quarterly_items[,c(2)])
    
    if(!is.null(period)) {
        outdir <- add_to_path(outdir, "quarterly", TRUE)
        
        get_per_period(sum_quarterly_points, "year", mean, outdir)
        get_per_period(sum_quarterly_items, "year", mean, outdir)
    }
    
    # Single values output
    stats_total <- data.frame(
        mean_quarterly_points,
        mean_quarterly_items
    )
    
    return(stats_total)
}

get_stats_yearly <- function(stats, outdir) {

    # Year sums
    sum_annual_points <- get_per_period(stats[c(1,2)], "year", sum, outdir)
    sum_annual_items <- get_per_period(stats[c(1,3)], "year", sum, outdir)
    
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