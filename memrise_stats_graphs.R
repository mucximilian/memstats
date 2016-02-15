library(ggplot2)
library(scales)

source("get_data.R")

file <- "csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)

# Creating a subset with total points per day of the 2nd and 3rd column
mem_stats_points_total <- mem_stats[, c(2, 3)]

# Plot the graph
mem_stats_plot <- ggplot(mem_stats_points_total, aes(x=DATE, y=POINTS_TOTAL)) +
    geom_line(colour = "red", size = 0.5) +
    labs(x = "") +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                 labels=date_format("%b %y")) +
    labs(y = "Points total (all-time)") +
    scale_y_continuous(labels = comma) +
    labs(title = "Memrise all-time points")

print(mem_stats_plot)

ggsave("plots/points_total.png", plot=mem_stats_plot, dpi=96)
dev.off()

