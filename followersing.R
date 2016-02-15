library(ggplot2)
library(scales)

source("functions.R")

file <- "input/csv/memrise_stats_20160215.csv"

mem_stats <- get_data(file)

followersing <- get_followersing(mem_stats)

print(head(followersing))

# Plot the graph
mem_stats_plot <- ggplot() +
    geom_line(data=followersing, aes(x=DATE, y=FOLLOWERS, color="green")) +
    geom_line(data=followersing, aes(x=DATE, y=FOLLOWING, color="blue")) +
    scale_color_manual(name="", labels=c("Following", "Followers"), values = c("blue", "green")) +
    labs(x = "") +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                 labels=date_format("%b %y")) +
    labs(y = "") +
    scale_y_continuous(labels = comma) +
    labs(title = "Memrise followers and following")

plot(mem_stats_plot, "followersing")