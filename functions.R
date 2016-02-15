get_data <- function(file) {

# Read and format CSV File
mem_stats <- read.csv(file, header = FALSE, 
                      sep = ",", na.strings = "NULL")

colnames(mem_stats) <- c(
    "ID",
    "DATE",
    "POINTS_TOTAL",
    "POINTS_DAY",
    "POINTS_MONTH",
    "POINTS_WEEK",
    "ITEMS",
    "FOLLOWERS",
    "FOLLOWING"
)

# Order by ID and format date
mem_stats <- mem_stats[order(mem_stats$ID), ]
mem_stats$DATE <- as.Date(mem_stats$DATE , "%Y-%m-%d %H:%M:%S")

# Computing absolute point and item diffs
mem_stats$POINTS_TOTAL_ABS <- c(NA, mem_stats[2:nrow(mem_stats), 3] - mem_stats[1:(nrow(mem_stats)-1), 3])
mem_stats$ITEMS_ABS <- c(NA, mem_stats[2:nrow(mem_stats), 7] - mem_stats[1:(nrow(mem_stats)-1), 7])

return(mem_stats)

}

get_data_points_total <- function(mem_stats_full) {
    
    mem_stats_points_tot <- mem_stats[, c(1, 2, 3)]
    return(mem_stats_points_tot)
}

get_data_points_abs <- function(mem_stats_full) {
    
    mem_stats_points_abs <- mem_stats[, c(1, 2, 10)]
    return(mem_stats_points_abs)
}

get_data_items_total <- function(mem_stats_full) {
    
    mem_stats_items_tot <- mem_stats[, c(1, 2, 7)]
    return(mem_stats_items_tot)
}

get_data_items_abs <- function(mem_stats_full) {
    
    mem_stats_items_abs <- mem_stats[, c(1, 2, 11)]
    return(mem_stats_items_abs)
}

get_period_bounds <- function(period) {
    
}

get_subset <- function(mem_stats, period) {
    
    period_bounds <- get_period_bounds(period)

    mem_stats_sub <- subset(mem_stats, DATE >= as.Date("2015-10-15") & DATE <= as.Date("2015-10-20"))
    
    return(mem_stats_sub)
}