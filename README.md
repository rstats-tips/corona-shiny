# corona-shiny

This shiny-app visualizes SARS-CoV-2 infections in Germany since March 2020.
The data is aggregated with respect to German Landkreise.

## Data Source
The data mainly used is provided by Robert Koch Institut (RKI)
at https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv
The data provided by [Pavel Mayer](https://pavelmayer.de/covid/risks/#tabletop) is used
to get information about the residents per Landkreis.


## Running the App
This app is hosted at https://rstats-tips.net/_shiny/corona/

### Docker
Given a Docker installation you can run
`docker run -d --rm  corona-shiny` to build a Docker image.
To run this image use `docker run -d --rm -p 3838:3838 corona-shiny`.
Now you can access the shiny app by pointing your browser to http://localhost:3838

To rebuild container run `docker build   -t corona-shiny .`

## More information
You can find more information about this shiny app at my blogpost at
https://www.rstats-tips.net/2020/09/08/shiny-app-to-explore-covid-19-in-germany/
