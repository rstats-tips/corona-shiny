#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
options(shiny.host = "0.0.0.0")
options(shiny.port = 4242)

library(tidyverse)
library(scales)
library(rlang)
library(lubridate)
library(plotly)
library(zoo)
library(DT)

library(plotly)

library(leaflet)
library(geojsonio)

cache_filename <- "raw-data.Rda"

if (file.exists(cache_filename)) {
  load(cache_filename)
  refetch <- (data_landkreise_detail_converted %>% pull(MeldedatumKlar) %>% max()) < Sys.Date() - days(1)
} else{
  refetch <- TRUE
}

# refetch <- FALSE

if(refetch) {
  url_landkreise_full <- "https://pavelmayer.de/covid/risks/full-data.csv"
  data_landkreise_detail <- read_csv(url_landkreise_full, col_types = cols(
    Bundesland = col_character(),
    Landkreis = col_character(),
    Altersgruppe = col_character(),
    Geschlecht = col_character(),
    IdLandkreis = col_character(),
    Datenstand = col_character(),
    Altersgruppe2 = col_character(),
    LandkreisName = col_character(),
    LandkreisTyp = col_character(),
    NeuerFallKlar = col_character(),
    RefdatumKlar = col_character(),
    MeldedatumKlar = col_character(),
    NeuerTodesfallKlar = col_character(),
    missingSinceDay = col_integer(),
    missingCasesInOldRecord = col_integer(),
    poppedUpOnDay = col_integer()
  ))

  data_landkreise_detail_converted <- data_landkreise_detail %>%
    mutate(
      MeldedatumKlar = as.Date(
        strptime(MeldedatumKlar, format = "%a, %d.%m.%Y %H:%M")
      ),
      Datenstand = as.Date(
        strptime(Datenstand, format = "%d.%m.%Y, %H:%M Uhr")
      ),
    )

  save(data_landkreise_detail_converted, file = cache_filename)
}

data_landkreise_per_day <- data_landkreise_detail_converted %>%
  # filter(NeuerFall %in% c(-1,1)) %>%
  arrange(MeldedatumKlar) %>%
  group_by(Landkreis, MeldedatumKlar) %>%
  summarize(
    infected = sum(AnzahlFall)
  ) %>%
  ungroup() %>%
  complete(MeldedatumKlar, Landkreis, fill = list(infected = 0)) %>%
  arrange(MeldedatumKlar) %>%
  group_by(Landkreis) %>%
  mutate(
    infected_7 = rollsum(infected, 7, fill = NA, align = "right"),
    infected_7_before = rollsum(lag(infected, n = 7), 7, fill = NA, align = "right"),
    delta_7 = infected_7 - infected_7_before
  )

landkreise <- data_landkreise_detail_converted %>%
  select(Landkreis, IdLandkreis, Bevoelkerung, Bundesland) %>%
  unique()

bundeslaender <- landkreise %>%
  select(Bundesland) %>%
  unique()

data_landkreise_per_day <- data_landkreise_per_day %>%
  left_join(landkreise) %>%
  mutate(
    infected_7_per_100k = round(infected_7 / Bevoelkerung * 100000, 1),
    delta_7_per_100k = round(delta_7 / Bevoelkerung * 100000, 1)
  ) %>%
  mutate(
    infected_7_per_100k_fill = if_else(infected_7_per_100k < 0, 0, infected_7_per_100k),
    infected_7_per_100k_fill = if_else(infected_7_per_100k > 50, 51, infected_7_per_100k),
    delta_7_per_100k_fill = delta_7_per_100k
  )

data_bundeslaender_per_day <- data_landkreise_per_day %>%
  group_by(MeldedatumKlar, Bundesland) %>%
  summarize(
    infected = sum(infected),
    Bevoelkerung = sum(Bevoelkerung)
    ) %>%
  ungroup() %>%
  complete(MeldedatumKlar, Bundesland, fill = list(infected = 0)) %>%
  arrange(MeldedatumKlar) %>%
  group_by(Bundesland) %>%
  mutate(
    infected_7 = rollsum(infected, 7, fill = NA, align = "right"),
    infected_7_before = rollsum(lag(infected, n = 7), 7, fill = NA, align = "right"),
    delta_7 = infected_7 - infected_7_before
  ) %>%
  mutate(
    infected_7_per_100k = round(infected_7 / Bevoelkerung * 100000, 1),
    delta_7_per_100k = round(delta_7 / Bevoelkerung * 100000, 1)
  ) %>%
  mutate(
    infected_7_per_100k_fill = if_else(infected_7_per_100k < 0, 0, infected_7_per_100k),
    infected_7_per_100k_fill = if_else(infected_7_per_100k > 50, 51, infected_7_per_100k),
    delta_7_per_100k_fill = delta_7_per_100k
  )


# # http://opendatalab.de/projects/geojson-utilities/
# bundeslaender_geo <- geojson_read("bundeslaender_simplify200.geojson",
#   what = "sp"
# )

# https://public.opendatasoft.com/explore/dataset/bundesland/export/
bundeslaender_geo <- geojson_read("bundesland.geojson",
  what = "sp"
)

# https://public.opendatasoft.com/explore/dataset/landkreise-in-germany/download/?format=geojson&timezone=Europe/Berlin&lang=en
landkreise_geo <- geojson_read("landkreise-in-germany.geojson",
  what = "sp"
)

landkreise_geo@data <- landkreise_geo@data %>%
  mutate(
    cca_2 = if_else(cca_2 == "03152", "03159", cca_2), # fix for Göttingen
    cca_2 = if_else(cca_2 == "03156", "03159", cca_2) # fix for Göttingen
  ) %>%
  select(cca_2)

# Berlin: https://github.com/funkeinteraktiv/Berlin-Geodaten
berlin_geo <- geojson_read("berlin_bezirke.geojson", what = "sp")
berlin_geo@data <- berlin_geo@data %>% 
  mutate(cca_2 = as.character(11000 + cartodb_id)) %>% 
  select(cca_2)


# Combine landkreise and bezirke of Berlin
landkreise_geo <- rbind(landkreise_geo, berlin_geo)

get_landkreis_by_id <- function(IdLandkreis) {
  landkreise %>% 
    filter(IdLandkreis == !!IdLandkreis) %>% 
    pull(Landkreis)
}

initial_Bundesland <- function(){
  "Hessen"
}

initial_Landkreis <- function() {
  data_landkreise_per_day %>% 
    ungroup() %>% 
    filter(Bundesland == initial_Bundesland()) %>% 
    filter(MeldedatumKlar == max(MeldedatumKlar)) %>% 
    slice_max(1, order_by = infected_7_per_100k) %>% 
    pull(IdLandkreis)
}

generate_leaflet <- function(data, kennzahl, title, landkreis_highlighted = NA) {
  landkreise_geo <- subset(landkreise_geo, cca_2 %in% data$IdLandkreis)

  landkreise_geo@data <- landkreise_geo@data %>%
    right_join(data %>% select(IdLandkreis, Landkreis, 
                               infected_7_per_100k,  infected_7_per_100k_fill, 
                               delta_7_per_100k, delta_7_per_100k_fill), 
               by = c("cca_2" = "IdLandkreis"))

  landkreise_geo@data <- landkreise_geo@data %>% 
    mutate(
      fill_value = .data[[paste0(kennzahl, "_fill")]]
    )
  
  landkreis_highlighted_geo <- subset(landkreise_geo, cca_2 %in% landkreis_highlighted)

   # Create a color palette for the map:
  if (kennzahl == "delta_7_per_100k") {
    mypalette <- colorNumeric(c("blue", "green", "red"), -51:51)
  }else{
    mypalette <- colorNumeric(c("green", "yellow", "red", "magenta"), 0:51)
  }
  
  labels_normal <- paste(
    "Landkreis: ", landkreise_geo@data$Landkreis, "<br/>",
    "Infizierte(7) pro 100k: ", landkreise_geo@data$infected_7_per_100k, "<br/>",
    "Delta in 7 Tagen Infizierte(7) pro 100k: ", landkreise_geo@data$delta_7_per_100k,
    sep = ""
  ) %>%
    lapply(htmltools::HTML)
  
  labels_highlighted <- paste(
    "Landkreis: ", landkreis_highlighted_geo@data$Landkreis, "<br/>",
    "Infizierte(7) pro 100k: ", landkreis_highlighted_geo@data$infected_7_per_100k, "<br/>",
    "Delta in 7 Tagen Infizierte(7) pro 100k: ", landkreis_highlighted_geo@data$delta_7_per_100k,
    sep = ""
  ) %>%
    lapply(htmltools::HTML)

  leaflet(data = landkreise_geo) %>%
    addLegend(pal = mypalette, values = ~fill_value, opacity = 0.5, title = title, position = "bottomleft") %>%
    addPolygons(
      fillColor = ~ mypalette(fill_value),
      fillOpacity = 0.8,
      opacity = 1,
      stroke = TRUE,
      color = "black",
      weight = 0.3,
      label = labels_normal,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      ),
      layerId = ~cca_2
    ) %>%
    addPolygons(
      data = landkreis_highlighted_geo,
      fillColor = ~ mypalette(fill_value),
      fillOpacity = 0.8,
      stroke = TRUE,
      color = "red",
      weight = 1.5,
      label = labels_highlighted,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      ),
      layerId = ~cca_2
    )
}

generate_leaflet_germany <- function(data, kennzahl, title) {
  bundeslaender_geo@data <- bundeslaender_geo@data %>%
    left_join(data %>% select(Bundesland,
                               infected_7_per_100k,  infected_7_per_100k_fill,
                               delta_7_per_100k, delta_7_per_100k_fill),
               by = c("gen" = "Bundesland"))

  bundeslaender_geo@data <- bundeslaender_geo@data %>%
    mutate(
      fill_value = .data[[paste0(kennzahl, "_fill")]]
    )

  # Create a color palette for the map:
  if (kennzahl == "delta_7_per_100k") {
    mypalette <- colorNumeric(c("blue", "green", "red"), -51:51)
  }else{
    mypalette <- colorNumeric(c("green", "yellow", "red", "magenta"), 0:51)
  }

  labels_normal <- paste(
    "Bundesland: ", bundeslaender_geo@data$gen, "<br/>",
    "Infizierte(7) pro 100k: ", bundeslaender_geo@data$infected_7_per_100k, "<br/>",
    "Delta in 7 Tagen Infizierte(7) pro 100k: ", bundeslaender_geo@data$delta_7_per_100k,
    sep = ""
  ) %>%
    lapply(htmltools::HTML)

  leaflet(data = bundeslaender_geo) %>%
    addLegend(pal = mypalette, values = ~fill_value, opacity = 0.5, title = title, position = "bottomleft") %>%
    addPolygons(
      fillColor = ~ mypalette(fill_value),
      fillOpacity = 0.8,
      opacity = 1,
      stroke = TRUE,
      color = "black",
      weight = 0.3,
      label = labels_normal,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      ),
      layerId = ~gen
    )
}

ui <- fluidPage(

  # Application title
  titlePanel("Corona in Deutschland im zeitlichen Verlauf auf Landkreisebene"),
  tags$p("Auf der linken Seite kann man durch Anklicken ein Bundesland wählen."),
  tags$p("Auf der rechten Seite wird dieses Bundesland dann auf Landkreisebene dargestellt."),
  tags$p("Es wird zum einen der Stand der Neu-Infizierten pro 100.000 Einwohner dargestellt wie auch
         die Veränderung dieses Wertes im Vergleich zum Vorzeitraum von 7 Tagen."),
  sidebarLayout(
    sidebarPanel(
      tags$b("Bundesland wählen"),
      leafletOutput("map_germany"),
      sliderInput("date",
        "Datum:",
        min = data_landkreise_per_day$MeldedatumKlar %>% min(),
        max = data_landkreise_per_day$MeldedatumKlar %>% max(),
        value = data_landkreise_per_day$MeldedatumKlar %>% max()
      ),
      tags$a(href="https://pavelmayer.de/covid/risks/#tabletop", "Zur Darstellung werden Daten von 
      Pavel Mayer genutzt." ),
      tags$p("Fehler in der Auswertung gehen zu meinen Lasten."),
      tags$a(href="https://www.rstats-tips.net/2020/09/08/shiny-app-to-explore-covid-19-in-germany/", "Mehr Infos zur App in meinem Blog"),
      tags$p("Kontakt per E-Mail: rstats.tips@gmail.com")
    ),


    mainPanel(
      fluidRow(
        column(12, tags$h1(textOutput("bundesland_datum_text")),
                    tags$p("Durch Anklicken eines Landkreises wird dessen zeitlicher Verlauf darunter angezeigt.")),
      ),
      fluidRow(
        column(6,
               tags$b("Infizierte pro 100.000 Einwohner innerhalb von 7 Tagen"),
               leafletOutput("map_leaflet", width = "100%")
               ),
        column(6, 
               tags$b("Änderung Infizierte pro 100.000 Einwohner innerhalb von 7 Tagen zur Vorwochenzeitraum"),
               leafletOutput("map_leaflet_delta", width = "100%")
               )
      ),
      fluidRow(
        column(12,
               plotlyOutput("landkreis_plot")),
               tags$p("Durch Anklicken eines Datenpunktes wird das Datum auf diesen Termin gesetzt.
                      Die beiden senkrechten Geraden zeigen das darstellte Datum und den 7-Tage-Zeitraum davor an.")
              ),
      DTOutput("table_of_bundesland")
      # plotOutput("map_plot"),
    )
  )
)


server <- function(input, output, session) {
  data_show <- reactive({
    data_landkreise_per_day %>%
      filter(MeldedatumKlar == input$date) %>%
      filter(Bundesland == active_bundesland()) %>%
      ungroup()
  })

  data_show_table <- reactive({
    # compute dataframe for datatable output
    value <- data_show() %>%
      select(-Bundesland) %>%
      arrange(desc(infected_7_per_100k))
    
    value
  })

  active_landkreis <- reactiveVal(initial_Landkreis())
  active_bundesland <- reactiveVal(initial_Bundesland())
  
  observeEvent(input$table_of_bundesland_rows_selected, {
      selected_row <- ifelse(!is.null(input$table_of_bundesland_rows_selected),
        input$table_of_bundesland_rows_selected,
        1
      )
      IdLandkreis <- data_show_table() %>%
        slice(selected_row) %>%
        pull(IdLandkreis)
      active_landkreis(IdLandkreis)
    }
  )

  output$bundesland_datum_text <- renderText({
    paste0(active_bundesland(), " am ", input$date)
  })

  output$landkreis <- renderText({
    active_landkreis()
  })

  output$table_of_bundesland <- renderDT(
    {
      data_show_table() %>% 
        select(-infected, -infected_7, -delta_7, -infected_7_before, 
               -IdLandkreis, -ends_with("_fill"))
    },
    server = TRUE,
    # selection = "single",
    selection = list(mode = 'single', selected = c(
      data_show_table() %>% 
        mutate(row = row_number()) %>%
        filter(IdLandkreis == active_landkreis()) %>% 
        pull(row)
        )
      )
  )

  output$landkreis_plot <- renderPlotly({
    gg <- data_landkreise_per_day %>%
      filter(IdLandkreis == active_landkreis()) %>%
      ggplot(aes(x = MeldedatumKlar, y = infected_7_per_100k)) +
      geom_point(aes(color = infected_7_per_100k)) +
      geom_line(alpha = 0.5, color = "lightGrey") +
      geom_vline(xintercept = as.numeric(ymd(input$date)), color = "black") +
      geom_vline(xintercept = as.numeric(ymd(input$date) - days(7)), color = "grey") +
      scale_color_gradient2(midpoint = 15, low = "green", mid = "yellow",
                           high = "red", space = "Lab", limits=c(0, 50), oob=squish,
                           name = NULL, guide = FALSE) +
      
      labs(
        title = get_landkreis_by_id(active_landkreis()),
        x = "Date",
        y = "Infections during 7 Days per 100k"
      ) +
      theme_bw()

    ggplotly(gg) %>% 
      event_register("plotly_click")
  })

  observeEvent(event_data("plotly_click"), {
    new_date <- data_landkreise_per_day %>%
      filter(IdLandkreis == active_landkreis()) %>% 
      filter(
        row_number() == event_data("plotly_click")$pointNumber + 1
      ) %>% 
      pull(MeldedatumKlar) 

    updateSliderInput(session, "date", value = as.Date(new_date))
  })
  
  output$map_germany <- renderLeaflet({
    gg <- data_bundeslaender_per_day %>%
      filter(MeldedatumKlar == input$date) %>%
      generate_leaflet_germany("infected_7_per_100k", "Inf. pro 100k")

    gg
  })
  observeEvent(input$map_germany_shape_click, { # update the location selectInput on map clicks
    Bundesland <- input$map_germany_shape_click
    active_bundesland(Bundesland$id)
    
    # set first row to active_landkreis for plot
    active_landkreis(
      data_show_table() %>% 
        slice(1) %>%
        pull(IdLandkreis)
    )
  })  
  
  output$map_leaflet <- renderLeaflet({
    gg <- data_show() %>%
      generate_leaflet("infected_7_per_100k", "Inf. pro 100k", active_landkreis())

    gg
  })
  observeEvent(input$map_leaflet_shape_click, { # update the location selectInput on map clicks
    IdLandkreis <- input$map_leaflet_shape_click
    active_landkreis(IdLandkreis$id)
  })
  
  output$map_leaflet_delta <- renderLeaflet({
    gg <- data_show() %>%
      generate_leaflet("delta_7_per_100k", "Delta Inf. pro 100k", active_landkreis())

    gg
  })
  observeEvent(input$map_leaflet_delta_shape_click, { # update the location selectInput on map clicks
    IdLandkreis <- input$map_leaflet_delta_shape_click
    active_landkreis(IdLandkreis$id)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
