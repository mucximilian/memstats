library(ggplot2)
library(scales)
library(zoo)
library(xts)

source("functions.R")
source("functions_memstats.R")
source("functions_plot.R")

# TO DO:
# Check and create directory structure if not existing

memstats <- get_data(file)