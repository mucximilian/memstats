################################################################################
# 
# This script contains all plotting related functions
#
# ggplot point shapes refrence: 
# http://sape.inf.usi.ch/quick-reference/ggplot2/shape
#
# Formatted date values:
# http://www.statmethods.net/input/dates.html
#
################################################################################

# General functions

has_sufficient_rows <- function(stats) {
    if(nrow(stats) > 1) {
        return(TRUE)
    } else {
        message("Dataset has only one observation, skipping plot")
        return(FALSE)
    }
}

save_plot <- function(plot, name){
    print(plot)
    file <- paste(name, ".png", sep = "")
    print(paste("Saving plot", file, sep=" "))
    ggsave(file, plot=plot, dpi=96)
    dev.off()
}

# Labelling functions

get_labels <- function(outdir) {
    x_label <- "Date" # The X-axis is always the date
    y_label <- get_y_label(last(outdir[[3]]))
    title <- get_pretty_title(outdir)
    
    return(c(x_label, y_label, title))
}

get_y_label <- function(colname) {
    
    colname <- gsub("_", " ", tolower(colname))
    y_label <- unlist(strsplit(colname, " "))[1] # Get first part of column name
    y_label <- get_capitalized(y_label)
    
    return(y_label)
}

get_pretty_title <- function(label) {
    
    label_new <- paste(
        paste(label[[2]], collapse = " "),
        paste(label[[3]], collapse = " "),
        sep=": "
    )
    label_new <- tolower(label_new)
    label_new <- gsub("_", " ", label_new)
    label_new <- gsub("/", " ", label_new)
    label_new <- gsub("cum", "cumulative", label_new)
    label_new <- gsub("abs", "absolute", label_new)
    label_new <- get_capitalized(label_new)

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

################################################################################
# Plotting functions

# Graph plot (for comulative and average data)
create_plot <- function(stats, outdir, type="graph_points") {
    
    # Only plot more than one observation
    if(has_sufficient_rows(stats)) {
        
        labels <- get_labels(outdir)
        
        # Plot the graph
        stats_plot <- ggplot(stats, aes(x=stats[, c(1)], y=stats[, c(2)]))
        
        if(type =="graph" | type == "graph_points") {
            stats_plot <- stats_plot + geom_line(colour = "red", size = 0.5)
            if (type == "graph_points") {
                stats_plot <- stats_plot + geom_point(shape=3, size=2, color="grey30")
            }
        } else if (type == "scatterplot") {
            stats_plot <- stats_plot + geom_point(shape=20, size=1, color="grey30") +
                geom_smooth(method=lm) +
                geom_hline(
                    yintercept=mean(stats[, c(2)], na.rm=TRUE),
                    colour="lightblue"
                )
        }
        
        # Setting default date breaks and format
        breaks_major <- "1 month"
        breaks_minor <- "1 week"
        breaks_major_format <- date_format("%b %y")
        
        # Setting date breaks and format for weeks and month data
        if(outdir[[2]][[1]] == "week" | outdir[[2]][[1]] == "month") {
            breaks_major <- "1 week"
            breaks_minor <- "1 day"
            breaks_major_format <- date_format("%d.%m (%A)")
        }
        
        stats_plot <- stats_plot + 
            scale_x_date(
                date_breaks = breaks_major, 
                date_minor_breaks = breaks_minor,
                labels=breaks_major_format
            ) + 
            scale_y_continuous(labels = comma)
        
        # Add crosses as point value indicators
        if(type == "graph_points") {
            stats_plot <- stats_plot + geom_point(shape=3, size=2, color="grey30")
        }

        stats_plot <- add_plot_labels(stats_plot, labels)

        filename <- get_filepath(outdir)

        # print(stats_plot)
        save_plot(stats_plot, filename) # TO DO update function input
   }
}

# Followers/-ing plot with two graph lines
plot_followersing <- function(followersing, outdir) {

    # Only plot more than one observation
    if(has_sufficient_rows(followersing)) {
        mem_stats_plot <- ggplot() +
            geom_line(data=followersing, aes(x=DATE, y=FOLLOWERS, color="green")) +
            geom_line(data=followersing, aes(x=DATE, y=FOLLOWING, color="blue")) +
            scale_color_manual(
                name="",
                labels=c("Following", "Followers"),
                values = c("blue", "green")
            ) +
            labs(x = "Date") +
            scale_x_date(
                date_breaks = "1 month",
                date_minor_breaks = "1 week",
                labels=date_format("%b %y")
            ) +
            labs(y = "People") +
            scale_y_continuous(labels = comma) +
            labs(title = "Followers and following")
        
        filename <- get_filepath(outdir)
        
        save_plot(mem_stats_plot, filename)
    }
}