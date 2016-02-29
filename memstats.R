source("setup.R")

col_points <- 10
col_items <- 11

################################################################################
# Get data subsets with dates

memstats_points <- get_columns(col_points)
memstats_points_cum <- get_columns(3)
memstats_items <- get_columns(col_items)
memstats_items_cum <- get_columns(7)
memstats_followersing <- get_columns(c(8, 9))

################################################################################
# Overall stats

get_total_day(sum, col_points)
get_total_day(mean, col_points)
get_total_day(sum, col_items)
get_total_day(mean, col_items)

################################################################################
# Totals absolute

get_total(memstats_points)
get_total(memstats_items)

################################################################################
# Totals cumulative

get_cum(memstats_points_cum)
get_cum(memstats_items_cum)

################################################################################
# Followers/-ing

plot_followersing(memstats_followersing)