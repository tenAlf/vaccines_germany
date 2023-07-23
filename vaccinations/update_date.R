
# Setup -------------------------------------------------------------------

url_for_date <- 
  paste0("https://raw.githubusercontent.com/",
         "robert-koch-institut/COVID-19-Impfungen_in_Deutschland/",
         "main/Metadaten/zenodo.json")



# Grab the meta file and find the last update -----------------------------

last_update <- 
  jsonlite::fromJSON(url_for_date)$version

