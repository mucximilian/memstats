library(ggplot2)
library(scales)
library(zoo)
library(xts)

source("functions.R")
source("functions_plot.R")

# Create output directories
dir.create("output", showWarnings = FALSE)
dir.create("output/plots", showWarnings = FALSE)
dir.create("output/csv", showWarnings = FALSE)