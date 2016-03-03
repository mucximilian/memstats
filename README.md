# memstats

A collection of R scripts that analyze and plot my recorded learning progress and performance with [Memrise](http://www.memrise.com). The learning data is retrieved once a day via the [unofficial API](https://github.com/carpiediem/memrise-enhancement-suite/wiki/Unofficial-Documentation-for-the-Memrise-API) and stored in a CSV file. 

##About

Since March 2015, I keep records of my Memrise learning performance (this is my [Memrise profile](http://www.memrise.com/user/mucx)). I figured out that getting more diverse stats about my learning than the ones available on the profile page boosts my learning motivation. I can aim for new goals regularly, like increasing a certain mean value. For example, keeping the overall mean of points per day above a specific value.

A cron-job controlled PHP script obtains the raw learning data from the API and stores it into a MySQL database (__Note:__ The PHP script is not part of the repository). Using the quick export function of _PHPMyAdmin_, a CSV file can be created containing the following data:

* DATE:         Date of the data retrieval
* POINTS_TOTAL: Cumulative sum of all points
* POINTS_DAY:   No longer available via the API (as of 2015-12-15)
* POINTS_MONTH: Cumulative sum of points per month
* POINTS_WEEK:  Cumulative sum of points per week
* ITEMS:        Cumulative count of learned items
* FOLLOWERS:    Number of followers
* FOLLOWING:    Number of people I follow

The statistic values of the cumulative/absolute sums and means are computed for the entire dataset and each period subset (year, month, week) when the script is executed. Various graphs visualize the data and summaries are stored in CSV files. The total number of plotted values as follows:

* 33 on the total data
* 23 per yer
* 9 per month
* 5 per week

If you want me to keep track of your Memrise learning progress as well, just contact me!

##TO DOs

* Time (x-axis) labels in English
* Making the plots more adaptive (axis-labels, breaks)
* More sophisticeted CSV output (combination of the CSV data that is created now)
* Enable direct access to a MySQL database (would require no CSV file)
* OR: No storing in a database at all but direct in a CSV file (less workaround)
* Based on a direct CSV storage: Adding a simple D3.js version for a direct visualization in the browser
  Example: http://bl.ocks.org/d3noob/b3ff6ae1c120eea654b5

##Examples

###Cumulative overall results
![Total points](./output/plots/total/daily_cum_points.png)
![Total items](./output/plots/total/daily_cum_items.png)

###Overall results (per day)
![Points per day](./output/plots/total/daily_abs_points.png)
![Items per day](./output/plots/total/daily_abs_items.png)

###Daily means per week
![Points per day](./output/plots/total/daily_mean_per_week_points.png)
![Items per day](./output/plots/total/daily_mean_per_week_items.png)

###Number of total followers/-ing
![Total followers/-ing](./output/plots/total/followersing.png)

* * *
License: GNU GENERAL PUBLIC LICENSE