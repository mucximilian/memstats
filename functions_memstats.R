################################################################################
# All functions operate on the 'memstats' data frame which is imported in the 
# 'setup.R script'

# Read and format CSV File
get_data <- function(file) {
    memstats <- read.csv(file, header = FALSE, 
                         sep = ",", na.strings = "NULL")
    
    colnames(memstats) <- c(
        "ID",
        "DATE",
        "POINTS_TOTAL",
        "POINTS_DAY",
        "POINTS_MONTH",
        "POINTS_WEEK",
        "ITEMS_TOTAL",
        "FOLLOWERS",
        "FOLLOWING"
    )
    
    # Order by ID and format date
    memstats <- memstats[order(memstats$ID), ]
    memstats$DATE <- as.Date(memstats$DATE , "%Y-%m-%d %H:%M:%S")
    
    # Computing absolute point and item diffs
    memstats$POINTS <- c(NA, memstats[2:nrow(memstats), 3] - memstats[1:(nrow(memstats)-1), 3])
    memstats$ITEMS <- c(NA, memstats[2:nrow(memstats), 7] - memstats[1:(nrow(memstats)-1), 7])
    
    # Replacing NAs with zero
    memstats$POINTS[is.na(memstats$POINTS)] <- 0
    memstats$ITEMS[is.na(memstats$ITEMS)] <- 0
    
    memstats_sub <- memstats[,c(
        "DATE",
        "POINTS_TOTAL",
        "POINTS",
        "ITEMS_TOTAL",
        "ITEMS",
        "FOLLOWERS",
        "FOLLOWING"
    )]

    return(memstats_sub)
}

################################################################################
get_xts_dataframe <- function(col) {
    # Creates a XTS dataframe with a data and a date column. Requires a 'DATE'
    # column in the input data frame
    
    memstats.xts <- xts(memstats[, c(col)], order.by = memstats[, "DATE"])
    return(memstats.xts)
}

################################################################################
# Data subsets

get_period_subset <- function(memstats, period) {
    
    # TO DO: Need to implement this function
    # period_bounds <- get_period_bounds(period)
    
    memstats_sub <- subset(memstats, DATE >= as.Date("2015-10-15") & DATE <= as.Date("2015-10-20"))
    return(memstats_sub)
}

get_columns_with_date <- function(cols) {
    # Returns the requested columns and the date column which is column 1
    return(memstats[, c(1, cols)])
}