source("setup.R")

#tab <- split(mem_stats, format(mem_stats$DATE, "%W"))

df <- get_columns(10)
df.split <- get_period_splits(df, "month")

test <- function(x, col){
    print("##########################################")
    print(head(x))
}

lapply(df.split, test, col=10)