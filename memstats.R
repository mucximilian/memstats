source("setup.R")

# Overall stats
get_total(memstats)

# Stats per year
split_by_period(memstats, "year")

# # Stats per month
split_by_period(memstats, "month")

# # Stats per week
split_by_period(memstats, "week")