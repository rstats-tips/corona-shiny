cache_filename <- "raw-data.Rda"

get_meta_data_landkreise <- function(force = FALSE) {
  landkreise_meta_filename <- "landkreise_meta.Rda"
  if(!file.exists(landkreise_meta_filename) | force) {
    url_landkreise_full <- "https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.csv"
    data_landkreise_detail <- read_csv(url_landkreise_full)
    
    landkreise <- data_landkreise_detail %>% 
      select(Landkreis = county, IdLandkreis = RS, Bevoelkerung = EWZ, Bundesland = BL) %>%
      unique()
    save(landkreise, file = landkreise_meta_filename)
  } else {
    load(landkreise_meta_filename)
  }
  return(landkreise)
}

landkreise <- get_meta_data_landkreise()

bundeslaender <- landkreise %>%
  select(Bundesland) %>%
  unique()


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
    # url_rki <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"
    url_rki <- "https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data"
    data_landkreise_detail_converted <- read_csv(url_rki) %>%
      mutate(
        Meldedatum = as.Date(
          strptime(Meldedatum, format = "%Y/%m/%d %H:%M:%S")
        ),
        Datenstand = as.Date(
          strptime(Datenstand, format = "%d.%m.%Y, %H:%M Uhr")
        )
      )    
    
    save(data_landkreise_detail_converted, file = cache_filename)
  }
  
  data_landkreise_per_day <- data_landkreise_detail_converted %>%
    mutate(IdLandkreis = if_else(IdLandkreis == "05354", "05334", IdLandkreis)) %>% # LK Aachen
    filter(NeuerFall %in% c(1, 0)) %>%
    complete(Meldedatum = seq(min(Meldedatum), max(Meldedatum), by = "day"), 
             IdLandkreis, fill = list(AnzahlFall = 0, NeuerFall = 1)) %>%
    group_by(IdLandkreis, Meldedatum) %>%
    summarize(
      infected = sum(AnzahlFall)
    ) %>% 
    ungroup() %>%
    complete(Meldedatum, IdLandkreis, fill = list(infected = 0)) %>%
    arrange(Meldedatum) %>%
    group_by(IdLandkreis) %>%
    mutate(
      infected_7 = rollsum(infected, 7, fill = NA, align = "right"),
      infected_7_before = rollsum(lag(infected, n = 7), 7, fill = NA, align = "right"),
      delta_7 = infected_7 - infected_7_before,
      gesamt = cumsum(infected)
    ) %>% 
    left_join(landkreise) %>% 
    mutate(
      infected_7_per_100k = round(infected_7 / Bevoelkerung * 100000, 1),
      delta_7_per_100k = round(delta_7 / Bevoelkerung * 100000, 1),
      R = round(infected_7_per_100k / (infected_7_per_100k - delta_7_per_100k), 2)
    ) %>%
    mutate(
      infected_7_per_100k_fill = if_else(infected_7_per_100k < 0, 0, infected_7_per_100k),
      infected_7_per_100k_fill = if_else(infected_7_per_100k > max_infections, max_infections + 1, infected_7_per_100k_fill),
      delta_7_per_100k_fill = if_else(delta_7_per_100k > max_delta_infections, max_delta_infections + 1, delta_7_per_100k),
      delta_7_per_100k_fill = if_else(delta_7_per_100k < -max_delta_infections, -max_delta_infections - 1, delta_7_per_100k_fill),
    )
  return(data_landkreise_per_day)
}

get_initial_bundesland_data <- function(data_landkreise_per_day){
  data_bundeslaender_per_day <- data_landkreise_per_day %>%
    group_by(Meldedatum, Bundesland) %>%
    summarize(
      infected = sum(infected),
      Bevoelkerung = sum(Bevoelkerung)
    ) %>%
    ungroup() %>%
    complete(Meldedatum, Bundesland, fill = list(infected = 0)) %>%
    arrange(Meldedatum) %>%
    group_by(Bundesland) %>%
    mutate(
      infected_7 = rollsum(infected, 7, fill = NA, align = "right"),
      infected_7_before = rollsum(lag(infected, n = 7), 7, fill = NA, align = "right"),
      delta_7 = infected_7 - infected_7_before
    ) %>%
    mutate(
      infected_7_per_100k = round(infected_7 / Bevoelkerung * 100000, 1),
      delta_7_per_100k = round(delta_7 / Bevoelkerung * 100000, 1),
      R = round(infected_7_per_100k / (infected_7_per_100k - delta_7_per_100k), 2)
    ) %>%
    mutate(
      infected_7_per_100k_fill = if_else(infected_7_per_100k < 0, 0, infected_7_per_100k),
      infected_7_per_100k_fill = if_else(infected_7_per_100k > max_infections, max_infections + 1, infected_7_per_100k_fill),
      delta_7_per_100k_fill = if_else(delta_7_per_100k > max_delta_infections, max_delta_infections + 1, delta_7_per_100k),
      delta_7_per_100k_fill = if_else(delta_7_per_100k < -max_delta_infections, -max_delta_infections - 1, delta_7_per_100k_fill),
    )
  return(data_bundeslaender_per_day)
}

get_initial_germany_data <- function(data_bundeslaender_per_day) {
  data_germany_per_day  <- data_bundeslaender_per_day %>% 
    group_by(Meldedatum) %>%
    summarize(
      infected = sum(infected),
      Bevoelkerung = sum(Bevoelkerung)
    ) %>%
    ungroup() %>%
    complete(Meldedatum, fill = list(infected = 0)) %>%
    arrange(Meldedatum) %>%
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
      infected_7_per_100k_fill = if_else(infected_7_per_100k > max_infections, max_infections + 1, infected_7_per_100k_fill),
      delta_7_per_100k_fill = if_else(delta_7_per_100k > max_delta_infections, max_delta_infections + 1, delta_7_per_100k),
      delta_7_per_100k_fill = if_else(delta_7_per_100k < -max_delta_infections, -max_delta_infections - 1, delta_7_per_100k_fill),
    )
  return(data_germany_per_day)
}

data_landkreise_per_day_initial <- get_initial_landkreis_data(force_refresh = FALSE)
data_bundeslaender_per_day_initial <- get_initial_bundesland_data(data_landkreise_per_day_initial)
data_germany_per_day_initial <- get_initial_germany_data(data_bundeslaender_per_day_initial)

# landkreise <- data_landkreise_per_day_initial %>%
#   select(Landkreis, IdLandkreis, Bevoelkerung, Bundesland) %>%
#   unique()
# 
# bundeslaender <- landkreise %>%
#   select(Bundesland) %>%
#   unique()


# https://public.opendatasoft.com/explore/dataset/bundesland/export/
bundeslaender_geo <- geojson_read("bundesland.geojson",
                                  what = "sp"
)

# https://public.opendatasoft.com/explore/dataset/landkreise-in-germany/download/?format=geojson&timezone=Europe/Berlin&lang=en
landkreise_geo <- geojson_read("landkreise_simplify200.geojson",
                               what = "sp"
)

landkreise_geo@data <- landkreise_geo@data %>%
  rename(cca_2 = AGS) %>% 
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

