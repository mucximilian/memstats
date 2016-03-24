################################################################################
#
# This is the main script
#
################################################################################

source("setup.R")

# Indicate the name of the input CSV file here 
file <- "memrise_stats_20160322.csv"

# Import file
memstats <- get_data(file)

#################################
# Get stats

# All stats (overall, year, month, week)
# evaluate_stats_full(memstats, "stats_")

evaluate_stats_latest(memstats, "month", "current")

# Overall stats
# evaluate_stats(memstats, "total", "test_total")

# Stats per year
# evaluate_stats(memstats, "year", "test_year")

# # # Stats per month
# evaluate_stats(memstats, "month", "test_month")

# # # Stats per week
# evaluate_stats(memstats, "week", "test_week")

