source("setup.R")

test_total_abs <- function() {
    abs_xts <- create_xts_dataframe(10)
    
    #plot_yearly_abs(abs_xts, "Points", "total/")
    #plot_quarterly_abs(abs_xts, "Points", "total/")
    #plot_monthly_abs(abs_xts, "Points", "total/")
    #plot_weekly_abs(abs_xts, "Points", "total/")
    plot_daily_abs(abs_xts, "Points", "total/")
}

test_total_abs()