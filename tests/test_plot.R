source("setup.R")
source("functions.R")
source("functions_plot.R")

set.seed(21)

df <- data.frame(
    as.Date(1:10, origin=Sys.Date()),
    sample(20, 10, replace = TRUE)
)
colnames(df) <- c("Date","Points abs")

outdir <- get_outdir("test")
outdir <- add_to_path(outdir, "week")
outdir <- add_to_path(outdir, "2016-04")
outdir <- add_to_path(outdir, "daily", TRUE)
outdir <- add_to_path(outdir, "sum_per_week", TRUE)
outdir <- add_to_path(outdir, colnames(df)[2], TRUE)

print(outdir)

get_path(outdir)

create_plot(df, outdir)