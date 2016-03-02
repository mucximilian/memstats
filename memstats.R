################################################################################
#
# This is the starting point for the Memrise data processing and plotting. All
# actions are performed on the input file defined in the 'setup.R' script.
#
################################################################################

source("setup.R")

# Overall stats
get_total(memstats)

# Stats per year
# split_by_year(memstats)
# 
# # Stats per month
# split_by_month(memstats)
# 
# # Stats per week
# split_by_week(memstats)