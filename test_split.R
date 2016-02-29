source("setup.R")

#tab <- split(mem_stats, format(mem_stats$DATE, "%W"))

tab <- split_by_period(mem_stats, "month")

bla <- function(x) {
    x.xts <- create_xts_dataframe(x, 10)
    print("##########################################")
    print(head(x))
    print(head(x.xts))
}

lapply(tab, bla)