cache_filename <- "raw-data.Rda"

get_initial_landkreis_data <- function(force_refresh = FALSE) {

  if (file.exists(cache_filename) & force_refresh == FALSE) {
    load(cache_filename)
    # refetch <- (data_landkreise_detail_converted %>% pull(MeldedatumKlar) %>% max()) < Sys.Date() - days(1)
    force_refresh <- FALSE
  } else{
    force_refresh <- TRUE
  }
  
  # Refresh only every hour
  if (force_refresh && file.exists(cache_filename)) {
    if(difftime(Sys.time() , file.info(cache_filename)$ctime , units = "mins") < 60) {
      force_refresh <- FALSE
      load(cache_filename)
    }
  }
  
  if(force_refresh) {
    Sys.setlocale(category = "LC_ALL", locale = "de_DE.UTF-8")
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
  return(data_landkreise_per_day)
}

get_initial_bundesland_data <- function(data_landkreise_per_day){
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
  return(data_bundeslaender_per_day)
}

get_initial_germany_data <- function(data_bundeslaender_per_day) {
  data_germany_per_day  <- data_bundeslaender_per_day %>% 
    group_by(MeldedatumKlar) %>%
    summarize(
      infected = sum(infected),
      Bevoelkerung = sum(Bevoelkerung)
    ) %>%
    ungroup() %>%
    complete(MeldedatumKlar, fill = list(infected = 0)) %>%
    arrange(MeldedatumKlar) %>%
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
  return(data_germany_per_day)
}

data_landkreise_per_day_initial <- get_initial_landkreis_data(force_refresh = FALSE)
data_bundeslaender_per_day_initial <- get_initial_bundesland_data(data_landkreise_per_day_initial)
data_germany_per_day_initial <- get_initial_germany_data(data_bundeslaender_per_day_initial)

landkreise <- data_landkreise_per_day_initial %>%
  select(Landkreis, IdLandkreis, Bevoelkerung, Bundesland) %>%
  unique()

bundeslaender <- landkreise %>%
  select(Bundesland) %>%
  unique()


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

# Landkreisteile von Göttingen vereinen
landkreise_geo <- raster::aggregate(landkreise_geo, by = 'cca_2') 

# Berlin: https://github.com/funkeinteraktiv/Berlin-Geodaten
berlin_geo <- geojson_read("berlin_bezirke.geojson", what = "sp")
berlin_geo@data <- berlin_geo@data %>% 
  mutate(cca_2 = as.character(11000 + cartodb_id)) %>% 
  select(cca_2)


# Combine landkreise and bezirke of Berlin
landkreise_geo <- rbind(landkreise_geo, berlin_geo)

