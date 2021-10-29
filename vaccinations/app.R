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


# Define UI for application that draws a histogram
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
                                   "Erstimpfung" = 1,
                                   "Zweitimpfung" = 2,
                                   "Drittimpfung" = 3)),
            checkboxGroupInput("age_group",
                               label = "Altersgruppen",
                               choices = unique(vac_data$Altersgruppe))
        ),
        mainPanel(
            plotOutput("distPlot")
    )
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
