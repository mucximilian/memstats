source("setup.R")
source("functions.R")

#tab <- split(mem_stats, format(mem_stats$DATE, "%W"))

tab <- split_by_period(mem_stats, "week")

print(tab)