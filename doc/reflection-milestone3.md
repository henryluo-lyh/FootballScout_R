# Implemented Features

In milestone 3, we have completed the implementation of all panels. The specific functions of each panel are as follows.
**Panel 1**
Panel1 provides users with an easy-to-understand visual representation of player ratings, making it easier to compare and analyze football players based on their attributes.
The first chart displays the top 5 players with the highest ratings in the selected year, league, and team.
The second chart shows the density of game ratings in the selected year, league, and team using a histogram.
The histogram is a graphical representation of the frequency distribution of player ratings.
The last is a box plot that summarizes the distribution of player. ratings by showing the median, quartiles, and outliers.

**Panel 2**
Panel 2 is used to compare selected football players' performance based on the selected data. After the user selects the players to be compared and the corresponding year, the players' game ratings, goals, assists, shots, and key passes in the seleted years are displayed as line charts.

**Panel 3**
Panel 3 is used to predict a player's performance. We use line chart to show the prediction of selected player's rating according to the data from previous years in the selected year range.

# Brief thoughts about dashR

The following is a summary of the feelings by using two languages:
- **In terms of deployment**
Since there are less packages in the mirror, the underlying packages need to be installed. However, there is no tutorial in the public, you need to read the log to install one by one, which takes a lot of time.

- **In terms of logic**
1. The operation logic of ploty r and python is different.
Example A: When dashR passively does not pass a value, it will return a list with only null; if it actively makes dashR empty, it will return an empty list.
Example B: In the chained callback, if you do not delete the lower-level dropdown option, but directly modify the upper-level dropdown option, R will still read the value selected by the lower-level dropdown and cause an error.

2. The difference between ggplot and Altair
If it is an empty dataset, ggplot will report an error, and Altair will draw an empty graph. The solution is to judge whether the dataframe is empty. If it is empty, use ggplot's theme_void() to draw a blank image.