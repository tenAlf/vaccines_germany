# This is a Rstudio shiny app with the purpose to graphically show the number
# of vaccines administered on a county level

library(shiny)
library(readr)
library(tidyverse)

# Load Data ---------------------------------------------------------------

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



# Define UI for county selection and group variables ----------------------

ui <- fluidPage(

    # Application title
    titlePanel("Verabreichte Impfungen"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "county_id", 
                        label   = strong("Impfort (Landkreis)"),
                        choices = unique(vac_data$LandkreisId_Impfort)),
            
            checkboxGroupInput("vac_group", 
                               label   = strong("Impfkategorie"),
                               choices = list(
                                   "Erstimpfung"  = 1,
                                   "Zweitimpfung" = 2,
                                   "Drittimpfung" = 3))
            ),
        
        mainPanel(
            plotOutput("vac_plot"),
            htmlOutput("total_vac"))
    )
)



# Define server logic for displaying a cumulative plot --------------------
server <- function(input, output) {
        
    output$vac_plot <- renderPlot({
        # keep selected county and selected Groups    
        plot_df <- 
            vac_data %>% 
            filter(LandkreisId_Impfort %in% input$county_id,
                   Impfschutz          %in% input$vac_group) %>% 
        # generate grouped cumsum based on checkboxInputs from ui.R
            group_by(Impfdatum, Impfschutz) %>% 
            summarise(Impfungen_am_Tag = sum(Anzahl), .groups = "drop") %>%
            group_by(Impfschutz) %>% 
            mutate(Gesamtimpfungen_seit_Beginn = cumsum(Impfungen_am_Tag))
        
        # draw the graph 
        ggplot(plot_df, mapping = aes(Impfdatum, Gesamtimpfungen_seit_Beginn)) +
            geom_line(aes(color = Impfschutz))
        
    })
        
    output$total_vac <- renderUI({
        # Print out the total number of admin. vaccines. 
        cum_vac <- 
            vac_data %>% 
            filter(LandkreisId_Impfort %in% input$county_id,
                   Impfschutz          %in% input$vac_group) %>%
            mutate(kumulierte_Impfungen = cumsum(Anzahl))
        
        HTML(paste(strong("Insgesamt verabreichte Impfungen: "),
              max(cum_vac$kumulierte_Impfungen)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
