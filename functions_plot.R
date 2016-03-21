################################################################################
# 
# This script contains all plotting related functions
#
# ggplot point shapes refrence: 
# http://sape.inf.usi.ch/quick-reference/ggplot2/shape
#
################################################################################
# General functions

get_filename <- function(dir, a, b) {
    return(paste(dir, paste(tolower(a), b, sep="_"), sep =""))
}

get_labels <- function(stats, label) {
    x_label <- "Date" # The X-axis is always the date
    y_label <- get_y_label(colnames(stats)[2])
    title <- get_pretty_title(label, tolower(y_label))
    filename <- paste(label, tolower(y_label), sep="_")
    
    return(c(x_label, y_label, title, filename))
}

get_y_label <- function(colname) {
    
    colname <- gsub("_", " ", tolower(colname))
    y_label <- unlist(strsplit(colname, " "))[1] # Get first part of column name
    y_label <- get_capitalized(y_label)
    
    return(y_label)
}

get_pretty_title <- function(label, y_type) {
    
    label_new <- get_capitalized(label)
    
    label_new <- gsub("_", " ", label_new)
    label_new <- gsub("/", " ", label_new)
    label_new <- gsub("cum", "cumulative", label_new)
    label_new <- gsub("abs", "absolute", label_new)
    
    label_new <- paste(label_new, paste("(", y_type, ")", sep=""))
    
    return(label_new)
}

get_capitalized <- function(text) {
    text <- paste(
        toupper(substring(text, 1, 1)),
        substring(text, 2,),
        sep=""
    )
    
    return(text)
}

add_plot_labels <- function(plot, labels) {
    plot <- plot +
        labs(x = labels[1]) +
        labs(y = labels[2]) +
        labs(title = labels[3])

    return(plot)
}

save_plot <- function(plot, name){
    print(plot)
    file <- paste("output/plots/", name, ".png", sep = "")
    print(paste("Saving plot", file, sep=" "))
    ggsave(file, plot=plot, dpi=96)
    dev.off()
}

################################################################################
# Plotting functions

# Graph plot (for comulative and average data)
plot_daily_graph <- function(stats, label, point=TRUE) {

    labels <- get_labels(stats, label)

    # Plot the graph
    stats_plot <- ggplot(stats, aes(x=stats[, c(1)], y=stats[, c(2)])) +
        geom_line(colour = "red", size = 0.5) +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                     labels=date_format("%b %y")) +
        scale_y_continuous(labels = comma)
    
    if(point) {
        stats_plot <- stats_plot + geom_point(shape=3, size=2, color="grey30")
    }
        
    stats_plot <- add_plot_labels(stats_plot, labels)
    
    save_plot(stats_plot, labels[4])
}

# Scatterplot with indicated mean
plot_daily_scatterplot <- function(stats, label) {
    
    labels <- get_labels(stats, label)

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

# Followers/-ing plot with two graph lines
plot_followersing <- function(followersing, dir) {

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
    
    save_plot(mem_stats_plot, dir)
}