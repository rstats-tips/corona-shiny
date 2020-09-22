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

generate_leaflet_germany <- function(data, kennzahl, title, legend = FALSE) {
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
  
  if (legend) {
    tmp <- leaflet(data = bundeslaender_geo) %>%
      addLegend(pal = mypalette, values = ~fill_value, opacity = 0.5, title = title, position = "bottomleft")
  } else {
    tmp <- leaflet(data = bundeslaender_geo) 
  }
  tmp %>%
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


generate_leaflet_detail <- function(data, kennzahl, title, landkreis_highlighted = NA) {
  landkreise_geo <- subset(landkreise_geo, cca_2 %in% data$IdLandkreis)
  data <- data %>% ungroup()
  
  landkreise_geo@data <-landkreise_geo@data %>%
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
