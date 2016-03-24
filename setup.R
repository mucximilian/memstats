library(ggplot2)
library(scales)
library(zoo)
library(xts)
library(plyr)

source("functions.R")
source("functions_evaluation.R")
source("functions_plot.R")

# Ensure english plot labels for the time
Sys.setlocale("LC_TIME", "en_US.UTF-8")