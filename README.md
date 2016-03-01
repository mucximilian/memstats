# memstats

A collection of R scripts that analyze and plot my recorded learning progress and performance with [Memrise](http://www.memrise.com). The learning data is retrieved once a day via the [unofficial API](https://github.com/carpiediem/memrise-enhancement-suite/wiki/Unofficial-Documentation-for-the-Memrise-API) and stored in a CSV file. 

##About

Currently only the cumulative and absolute all-time points and items are plotted for my [Memrise profile](http://www.memrise.com/user/mucx) when the scripts are executed.

A cron-job controlled PHP script obtains the raw learning data from the API and stores it into a MySQL database (__Note:__ The PHP script is not part of the repository). Using the quick export function of _PHPMyAdmin_, a CSV file can be created containing the following data:

* DATE:         Date of the data retrieval
* POINTS_TOTAL: Cumulative sum of all points
* POINTS_DAY:   No longer available via the API (as of 2015-12-15)
* POINTS_MONTH: Cumulative sum of points per month
* POINTS_WEEK:  Cumulative sum of points per week
* ITEMS:        Cumulative count of learned items
* FOLLOWERS:    Number of followers
* FOLLOWING:    Number of people I follow

##TO DOs

* Period subsets (all weeks, months, quarters, years) for points/items with averages and graphs
* Adding output CSV with statistical summary
* Enable direct access to a MySQL database (would require no CSV file)
* Time (x-axis) labels in English
* More variety in the output plot types (currently only two types are implemented)

##Examples

###Cumulative overall results
![Total points](./output/plots/total/daily_cum_points.png)
![Total items](./output/plots/total/daily_cum_items.png)

###Overall results (per day)
![Points per day](./output/plots/total/daily_abs_points.png)
![Items per day](./output/plots/total/daily_abs_items.png)

###Number of total followers/-ing
![Total followers/-ing](./output/plots/total/followersing.png)

* * *
License: GNU GENERAL PUBLIC LICENSE