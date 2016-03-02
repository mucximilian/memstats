source("setup.R")

#tab <- split(mem_stats, format(mem_stats$DATE, "%W"))

df <- get_columns(2)
df.split <- get_period_splits(df, "month")

test <- function(x){
    print("##########################################")
    print(colnames(x)[2])
}

lapply(df.split, test)