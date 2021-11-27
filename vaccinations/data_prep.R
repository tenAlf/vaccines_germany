# Setup -------------------------------------------------------------------

library(tidyverse)

# Load Data ---------------------------------------------------------------

# get the newest Data from the RKI github repo
data_url <- 
  paste0("https://raw.githubusercontent.com",
         "/robert-koch-institut/COVID-19-Impfungen_in_Deutschland",
         "/master/Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv")

vac_data <-
  read_csv(
    data_url,
    col_types = cols(
      "Impfdatum"           = col_date(),
      "LandkreisId_Impfort" = col_factor(),
      "Altersgruppe"        = col_factor(),
      "Impfschutz"          = col_factor(),
      "Anzahl"              = col_integer()
    )
  )

vac_data <- # Make sure the table is correctly sorted
  vac_data %>% 
  arrange(LandkreisId_Impfort, Impfdatum) 

# load the county-id information aquiered from regionalstatistik.de
suppressWarnings(
  suppressMessages(
  ags_data <- 
    read_csv2("ext_data/ags.csv", 
            locale         = locale("de", encoding = "ISO-8859-1"), 
            skip           = 6,
            col_names      = c("date", "county_id", "county", "size"),
            show_col_types = FALSE)
))
# The following throws a warning due to the very last rows being filled with
# arbitrary information that doesn't concern us
suppressWarnings(
  ags_data <- 
    ags_data %>% 
    select(county_id, county) %>% 
    drop_na()
  )

# Clean up and prep -------------------------------------------------------

# County-ID in vac_data is always 5 characters long (except u = unknown). 
# ID column in ags_data varies between 2 and 8 (AGS is max. 8 characters long)
# Hamburg and Berlin do not match between the tables due to different character
# length so first we need to correct that.

# even though it's just for two entries, let's make it a general correction for
# training purposes in using helper functions and purrr

extend_string <- # helper to extend short ids by a 0 to match the id in vac_data
  function(x) {
    while(str_length(x) < 5) {
      x <- paste0(x, "0")
    } 
    x
  }
check_length <- # helper for the prediction in modify
  function(x) if_else(str_length(x) < 5, TRUE, FALSE)

# Now correct too short ids and let's also keep only the county name and remove
# unnecessary information in the name column
ags_data <- 
  ags_data %>% 
  mutate(
    county_id = modify_if(county_id, check_length, extend_string),
    county = str_remove_all(ags_data$county, ", .*"))



# Join --------------------------------------------------------------------

vac_data <- # extend vac_data with county name
  left_join(vac_data, ags_data, by = c("LandkreisId_Impfort" = "county_id")) %>% 
  # combine the ID and name for the search later in the App)
  mutate(LandkreisId_Impfort = modify2(LandkreisId_Impfort, 
                                       county, 
                                       ~ paste0(.x, " (", .y, ")")))



# create a set of custom labels and captions for later use
custom_labs <- 
  c("1" = "Erstimpfung", 
    "2" = "Zweitimpfung", 
    "3" = "Drittimpfung")
custom_cols <- 
  c("1" = "#D81B60", 
    "2" = "#FFC107", 
    "3" = "#1E88E5")
custom_cap <- 
  c("x-Achse: Monat \ny-Achse: Gesamtimpfungen im Zeitraum")


# Get the last Update Date ------------------------------------------------

source("update_date.R")
