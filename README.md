# Inflation Adjustment in Shiny

Australian Bureau of Statistics (ABS) publishes the nominal gross domestic product, series id A2303730T.

This app retrieves the most recent published data and calculates the nominal GDP deflator (appropriate multipliers for the years in which you have price/cost data).

The app may be found here: https://connor-ballinger.shinyapps.io/shiny_price_adjustment/.

The heavy lifting is managed by the [readabs](https://mattcowgill.github.io/readabs/) package.
