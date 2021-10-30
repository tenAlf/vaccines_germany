#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)

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
                               label = strong("Impfkategorie"),
                               choices = list(
                                   "Erstimpfung"  = 1,
                                   "Zweitimpfung" = 2,
                                   "Drittimpfung" = 3)),
            
            checkboxGroupInput("age_group",
                               label = "Altersgruppen",
                               choices = list(
                                   "12-17"     = 1,
                                   "18-59"     = 2,
                                   "60+"       = 3,
                                   "unbekannt" = 0))
            ),
        
        mainPanel(
            plotOutput("vac_plot"))
    )
)



# Define server logic for displaying a cumulative plot --------------------
server <- function(input, output) {

    output$vac_plot <- renderPlot({
        # keep selected county and selected Groups
        plot_df <- 
            vac_data %>% 
            filter(LandkreisId_Impfort  == input$county_id,
                   Impfschutz         %in% input$vac_group,
                   Altersgruppe       %in% input$age_group) %>% 
            # generate grouped cumsum based on checkboxInputs from ui.R
            group_by(Impfschutz, Altersgruppe) %>% 
            mutate(Gesamtimpfungen_seit_Beginn = cumsum(Anzahl)) 
        # draw the graph 
        ggplot(plot_dfmapping = aes(Impfdatum, Gesamtimpfungen_seit_Beginn)) +
            geom_line(aes(color = Altersgruppe))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
