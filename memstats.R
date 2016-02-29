source("setup.R")

col_points <- 10
col_items <- 11

################################################################################
# Get data subsets with dates

memstats_followersing <- get_columns(c(8, 9))
memstats_points <- get_columns(col_points)
memstats_points_cum <- get_columns(3)
memstats_items <- get_columns(col_items)
memstats_items_cum <- get_columns(7)

################################################################################
# Overall stats

get_total_day(sum, col_points)
get_total_day(mean, col_points)
get_total_day(sum, col_items)
get_total_day(mean, col_items)

################################################################################
# Totals cumulative

stats_total_cum <- get_column(col_points)
stats_items_cum <- get_column(col_items)

plot_cum(stats_total_cum, "Points")
plot_cum(stats_items_cum, "Items")

################################################################################
# Totals absolute

stats_total_abs <- get_column(col_points)
stats_items_abs <- get_column(col_items)

plot_daily_abs(stats_total_abs, "Points")
plot_daily_abs(stats_items_abs, "Items")

# Total averages per period

mem_stats_points.xts <- create_xts_dataframe(col_points)
mem_stats_items.xts <- create_xts_dataframe(col_items)

plot_weekly_avg(mem_stats_items.xts, "Items")
plot_monthly_avg(mem_stats_items.xts, "Items")
plot_quarterly_avg(mem_stats_items.xts, "Items")
plot_yearly_avg(mem_stats_items.xts, "Items")

plot_weekly_avg(mem_stats_points.xts, "Points")
plot_monthly_avg(mem_stats_points.xts, "Points")
plot_quarterly_avg(mem_stats_points.xts, "Points")
plot_yearly_avg(mem_stats_points.xts, "Points")

# Totals per year

# TO DO:
# Add year loop

# plot_weekly_abs(per_year, "Items")
# plot_monthly_abs(per_year, "Items")
# plot_quarterly_abs(per_year, "Items")
# 
# plot_weekly_abs(per_year, "Points")
# plot_monthly_abs(per_year, "Points")
# plot_quarterly_abs(per_year, "Points")

# Averages per year

# TO DO:
# Add year loop

# plot_weekly_avg(per_year, "Items")
# plot_monthly_avg(per_year, "Items")
# plot_quarterly_avg(per_year, "Items")
# 
# plot_weekly_avg(per_year, "Points")
# plot_monthly_avg(per_year, "Points")
# plot_quarterly_avg(per_year, "Points")

################################################################################
# Plot followersing

plot_followersing(memstats_followersing)