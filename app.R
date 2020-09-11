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

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

source("preprocess-data.R")
source("leaflet-functions.R")

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
