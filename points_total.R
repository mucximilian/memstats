library(ggplot2)
library(scales)

source("functions.R")

file <- "csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)
points_total <- get_data_points_total(mem_stats)

# Plot the graph
mem_stats_plot <- ggplot(points_total, aes(x=DATE, y=POINTS_TOTAL)) +
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

