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

#################################
# Get stats

# Overall stats
# evaluate_stats(memstats)

# Stats per year
# evaluate_stats(memstats, "year")

# # # Stats per month
evaluate_stats(memstats, "month")

# # # Stats per week
# evaluate_stats(memstats, "week")

