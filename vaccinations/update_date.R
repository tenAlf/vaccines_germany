
# Setup -------------------------------------------------------------------

url_for_date <- 
  paste0("https://github.com/robert-koch-institut",
         "/COVID-19-Impfungen_in_Deutschland/blob/master/",
         "Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv")


# Read Page and extract Date ----------------------------------------------

last_update <- 
  rvest::session(url_for_date) %>% 
  rvest::html_nodes("relative-time") %>% 
  rvest::html_attr("datetime") %>% 
  lubridate::as_datetime() %>% 
  lubridate::with_tz("Europe/Berlin")
  
