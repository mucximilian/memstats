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
mem_stats <- mem_stats[order(mem_stats$ID) , ]
mem_stats$DATE <- as.Date(mem_stats$DATE , "%Y-%m-%d %H:%M:%S")

# TO DO: Add point and item diffs
mem_stats$POINTS_TOTAL_ABS <- c(NA, mem_stats[2:nrow(mem_stats), 3] - mem_stats[1:(nrow(mem_stats)-1), 3])
mem_stats$ITEMS_ABS <- c(NA, mem_stats[2:nrow(mem_stats), 7] - mem_stats[1:(nrow(mem_stats)-1), 7])

return(mem_stats)

}