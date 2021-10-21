# Setup -------------------------------------------------------------------
# loading requiered packages
library(tidyverse)

# retrieving data
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


# Aggregating Data --------------------------------------------------------

cumulative_vac_data <-
  vac_data %>%
    arrange(LandkreisId_Impfort, Impfdatum) %>%
    group_by(LandkreisId_Impfort, Impfdatum) %>%
    summarise(Impfungen_am_Tag = sum(Anzahl)) %>%
    mutate(Gesamtimpfungen_seit_Beginn = cumsum(Impfungen_am_Tag))


# Visualization -----------------------------------------------------------

# select single county and visualize vaccines over time
selected_county <- "05314"
cumulative_vac_data %>% 
  filter(LandkreisId_Impfort == selected_county) %>% 
  ggplot(mapping = aes(x = Impfdatum, y = Gesamtimpfungen_seit_Beginn)) +
    geom_line()

