### Overview:
This partially interactive visualization app was built to easily glance at the **2020 regular season of the [Overwatch League](https://overwatchleague.com/)**, a premier international esports league for the game [Overwatch](https://playoverwatch.com/). The visualizations include the final scores from the teams accompanied with betting odds for each match as aggregated by [OddsPortal](https://www.oddsportal.com/esports/usa/overwatch-overwatch-league/results/); from which the data was web-scraped. 

The main visualization quickly summarizes the performances of all 20 teams including how many of their wins and loses happened when they were favoured to lose and win respectively. More detailed information about a team's season performance can be viewed by clicking on their logo. 

Team specific charts visualize a summary of their 21 regular season matches: their opponents, the final map score and the betting odds for and against them winning. A summary of their opponents can be seen below. Click on any opponent brings up the match or matches that they played against them. Double clicking an opponent takes you to their season summary. 

#### Some notes:
This does not include any of the mid-season tournament matches that took place either during the season or the post-season. It also does not include the bonus wins that winning teams received from the mid-season tournaments and the resulting order of teams differs slightly from the [final regular season standings](https://overwatchleague.com/en-us/standings).

This app was written in RShiny; code for which can be found at my [GitHub repository](https://github.com/fishghost/owl-2020-summary).