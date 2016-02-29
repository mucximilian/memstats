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

save_plot <- function(plot, name){
    print(plot)
    file <- paste("output/plots/", name, ".png", sep = "")
    print(paste("Saving plot", file, sep=" "))
    ggsave(file, plot=plot, dpi=96)
    dev.off()
}

get_filename <- function(dir, a, b) {
    return(paste(dir, paste(tolower(a), b, sep="_"), sep =""))
}