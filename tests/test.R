source("setup.R")

max_day <- apply_per_period(memstats[c(1,2)], "day", pmax)

print(max_day)