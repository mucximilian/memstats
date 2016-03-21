################################################################################
#
# This is the main script
#
################################################################################

source("setup.R")

# Indicate the name of the input CSV file here 
latest_file <- "memrise_stats_20160322.csv"

# Import file
memstats <- get_data(latest_file)

# Get stats
#
# Overall stats
get_total(memstats)

# Stats per year
split_by_period(memstats, "year")

# # # Stats per month
split_by_period(memstats, "month")

# # # Stats per week
split_by_period(memstats, "week")
