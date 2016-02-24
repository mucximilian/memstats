source("setup.R")

################################################################################
# Overall stats

get_points_total_avg_day(mem_stats)
get_items_total_avg_day(mem_stats)

################################################################################
# Totals cumulative

stats_total_cum <- get_points_cum(mem_stats)
stats_items_cum <- get_items_cum(mem_stats)

plot_cum(stats_total_cum, "Points")
plot_cum(stats_items_cum, "Items")

################################################################################
# Totals absolute

stats_total_abs <- get_points_abs(mem_stats)
stats_items_abs <- get_items_abs(mem_stats)

plot_daily_abs(stats_total_abs, "Points")
plot_daily_abs(stats_items_abs, "Items")

# Total averages per period

mem_stats_points.xts <- xts(mem_stats[, c(10)], order.by = mem_stats[, "DATE"])
mem_stats_items.xts <- xts(mem_stats[, c(11)], order.by = mem_stats[, "DATE"])

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

plot_followersing(mem_stats)