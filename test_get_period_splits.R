source("setup.R")

# Indicate the name of the input CSV file here 
latest_file <- "memrise_stats_20160322.csv"

# Import file
memstats <- get_data(latest_file)

period = "week"

stats.split <- get_period_splits(memstats, period)