# Visualized Summary of Overwatch League 2020 Regular Season
View the published app on [shinyapps](https://fishghost.shinyapps.io/OddsPortal/).

This is an R Shiny app made to summarize the regular season matches of Overwatch League 2020 season. It contains 5 partially interactive, unique visualizations: 2 comparing every team's season performance and 3 dedicated to summarizing a selected team's regular season performance. 

All code is contained within app.R. It is roughly separated into 3 sections: 
- **data import and wrangling**: imports cleaned data scraped from [OddsPortal](https://www.oddsportal.com/esports/usa/overwatch-overwatch-league/results/) and creates summary tables
- **Shiny UI code**: code to create fluid page 
- **Shiny server code**: code to dynamically create visualizations and react to interactivity 

More information about the visualizations and data can be found on the [app's](https://fishghost.shinyapps.io/OddsPortal/) Read Me tab. 