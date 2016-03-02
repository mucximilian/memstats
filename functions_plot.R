################################################################################
add_plot_labels <- function(plot, labels) {
    return(plot +
            labs(x = labels[1]) +
            labs(y = labels[2]) +
            labs(title = labels[3])
            )
}

save_plot <- function(plot, name){
    print(plot)
    file <- paste("output/plots/", name, ".png", sep = "")
    print(paste("Saving plot", file, sep=" "))
    ggsave(file, plot=plot, dpi=96)
    dev.off()
}

################################################################################

# Graph plot (for comulative and average data)
plot_daily_graph <- function(stats, label) {

    labels <- get_labels(stats, label)

    # Plot the graph
    stats_plot <- ggplot(stats, aes(x=stats[, c(1)], y=stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        scale_y_continuous(labels = comma)
        
    stats_plot <- add_plot_labels(stats_plot, labels)
    
    save_plot(stats_plot, labels[4])
}

################################################################################
# Functions for total plots

plot_daily_scatterplot <- function(stats, label) {
    
    labels <- get_labels(stats, label)
    
    # Plot the graph
    stats_plot <- ggplot(stats, aes(x=DATE, y=stats[, c(2)])) +
        geom_point(shape=20, size=1, color="grey30") +
        geom_smooth(method=lm) +
        geom_hline(yintercept=mean(stats[, c(2)], na.rm=TRUE),
                   colour="lightblue") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        scale_y_continuous(labels = comma)

    stats_plot <- add_plot_labels(stats_plot, labels)
    
    save_plot(stats_plot, labels[4])
}



plot_followersing <- function(followersing, dir) {

    # Plot the graph
    mem_stats_plot <- ggplot() +
        geom_line(data=followersing, aes(x=DATE, y=FOLLOWERS, color="green")) +
        geom_line(data=followersing, aes(x=DATE, y=FOLLOWING, color="blue")) +
        scale_color_manual(name="", labels=c("Following", "Followers"), values = c("blue", "green")) +
        labs(x = "Date") +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        labs(y = "People") +
        scale_y_continuous(labels = comma) +
        labs(title = "Followers and following")
    
    save_plot(mem_stats_plot, paste(dir, "followersing", sep = ""))
}