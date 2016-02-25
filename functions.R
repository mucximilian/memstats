# Read and format CSV File
get_data <- function(file) {
    mem_stats <- read.csv(file, header = FALSE, 
                          sep = ",", na.strings = "NULL")
    
    colnames(mem_stats) <- c(
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
    mem_stats <- mem_stats[order(mem_stats$ID), ]
    mem_stats$DATE <- as.Date(mem_stats$DATE , "%Y-%m-%d %H:%M:%S")
    
    # Computing absolute point and item diffs
    mem_stats$POINTS_ABS <- c(NA, mem_stats[2:nrow(mem_stats), 3] - mem_stats[1:(nrow(mem_stats)-1), 3])
    mem_stats$ITEMS_ABS <- c(NA, mem_stats[2:nrow(mem_stats), 7] - mem_stats[1:(nrow(mem_stats)-1), 7])
    
    # Replacing NAs with zero
    mem_stats$POINTS_ABS[is.na(mem_stats$POINTS_ABS)] <- 0
    mem_stats$ITEMS_ABS[is.na(mem_stats$ITEMS_ABS)] <- 0
    
    return(mem_stats)
}

# Data subsets
get_cum <- function(col) {
    mem_stats_points_tot <- mem_stats[, c(2, col)]
    return(mem_stats_points_tot)
}

get_abs <- function(col) {
    mem_stats_abs <- mem_stats[, c(2, col)]
    return(mem_stats_abs)
}

get_followersing <- function(mem_stats) {
    mem_stats_items_abs <- mem_stats[, c(2, 8, 9)]
    return(mem_stats_items_abs)
}

get_subset <- function(mem_stats, period) {
    
    period_bounds <- get_period_bounds(period)

    mem_stats_sub <- subset(mem_stats, DATE >= as.Date("2015-10-15") & DATE <= as.Date("2015-10-20"))
    
    return(mem_stats_sub)
}

get_points_total_avg_day <- function(mem_stats) {
    points_total_avg <- get_points_abs(mem_stats)
    print(mean(points_total_avg$POINTS_ABS, na.rm=TRUE))
}

get_items_total_avg_day <- function(mem_stats) {
    items_total_avg <- get_items_abs(mem_stats)
    print(mean(items_total_avg$ITEMS_ABS, na.rm=TRUE))
}

save_plot <- function(plot, name){
    print(plot)
    file <- paste("output/plots/", name, ".png", sep = "")
    print(paste("Saving plot", file, sep=" "))
    ggsave(file, plot=plot, dpi=96)
    dev.off()
}

create_xts_dataframe <- function(col) {
    df.xts <- xts(mem_stats[, c(col)], order.by = mem_stats[, "DATE"])
    return(df.xts)
}