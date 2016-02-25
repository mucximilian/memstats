source("setup.R")

test_total_abs <- function() {
    
    stats_xts <- create_xts_dataframe(10)
    
    stats_total_abs <- get_abs(10)
    
    #plot_yearly_abs("Points", "total/")
    #plot_quarterly_abs("Points", "total/")
    #plot_monthly_abs("Points", "total/")
    #plot_weekly_abs(stats_xts, "Points", "total/")
    plot_daily_abs(stats_total_abs, "Points", "total/")
}

test_total_abs()