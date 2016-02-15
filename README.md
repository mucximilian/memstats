# memstats

A simple R script that analyzes and plots a user's recorded learning performance on memrise.com. The learning data is
retrieved daily via the [unofficial API](https://github.com/carpiediem/memrise-enhancement-suite/wiki/Unofficial-Documentation-for-the-Memrise-API) and stored in a CSV file. 

##NOTES

Currently only the all-time points are plotted.

The stats are retrieved daily via a cron-job controlled PHP script and stored
in a MySQL database. The # CSV file is created using the export function of
PHPMyAdmin. The POINTS_DAY data is no longer available via the API (as of 
2015-12-15)

##TO DOs
* POINTS WEEK
* POINTS MONTH
* FOLLOWERS/FOLLOWING
* ITEMS
