# corona-shiny

This shiny-app visualizes covid-19 infections in Germany since March 2020.
The data is aggregated with respect to German Landkreise.

## Data Source
The data which is used is provided by [Pavel Mayer](https://pavelmayer.de/covid/risks/#tabletop).
He uses data provided by Robert Koch Institut (RKI).

## Differences in Shown Infections
I'm showing an infection at the date it noted at the local health department (MeldedatumKlar).
Pavel uses the date an infection is reported to the RKI. There may be an delay
of several days.

## Running the App
This app is hosted at Shinyapps.io: https://rstats-tips.shinyapps.io/corona-shiny/

You can also run it using binder:
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/rstats-tips/corona-shiny/master?urlpath=shiny%2F)

## More information
You can find more information about this shiny app at my blogpost at
https://www.rstats-tips.net/2020/09/08/shiny-app-to-explore-covid-19-in-germany/
