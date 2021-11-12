# This is a Rstudio shiny app with the purpose to graphically show the number
# of vaccines administered on a county level

library(shiny)
library(readr)
library(tidyverse)

# Get Data ----------------------------------------------------------------

source("data_prep.R")

# Define UI for county selection and group variables ----------------------

ui <- fluidPage(

    # Application title
    titlePanel("Verabreichte Impfungen 2021"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "county_id", 
                        label   = strong("Impfort (Landkreis)"),
                        choices = unique(vac_data$LandkreisId_Impfort)),
            
            checkboxGroupInput(inputId = "vac_group", 
                               label   = strong("Impfkategorie"),
                               choices = list(
                                   "Erstimpfung"  = 1,
                                   "Zweitimpfung" = 2,
                                   "Drittimpfung" = 3)),
            dateRangeInput(inputId   = "date_span",
                           label     = strong("Zeitraum"),
                           start     = min(vac_data$Impfdatum),
                           end       = max(vac_data$Impfdatum),
                           min       = "2020-12-01",
                           format    = "dd-M-yyyy",
                           weekstart = 1,
                           language  = "de"
                           )
            ),
        
        mainPanel(
            plotOutput("vac_plot", click = "plot_click", hover = "plot_hover"),
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
        custom_labs <- c("1" = "Erstimpfung", 
                         "2" = "Zweitimpfung", 
                         "3" = "Drittimpfung")
        custom_cols <- c("1" = "#D81B60", 
                         "2" = "#FFC107", 
                         "3" = "#1E88E5")
        
        ggplot(plot_df, mapping = aes(Impfdatum, Gesamtimpfungen_seit_Beginn)) +
            geom_line(aes(color = Impfschutz)) +
            scale_color_manual(values = custom_cols, labels = custom_labs) +
            labs(caption = "x-Achse: Monat 
y-Achse: Gesamtimpfungen seit Beginn",
                 x = "", y = "") +
            scale_y_continuous(
                labels   = function(x) format(x, scientific = FALSE),
                n.breaks = 5) +
            scale_x_date(date_breaks = "2 month", date_labels = "%b")+
            theme(axis.text.y  = element_text(face = "bold", size = 13),
                  axis.text.x  = element_text(face = "bold", size = 13),
                  plot.caption = element_text(hjust = 0, size = 10))
        
    })
        
    output$total_vac <- renderUI({
        # Print out the total number of admin. vaccines. 
        date_start <- as.Date(input$date_span[1])
        cum_vac <- 
            vac_data %>% 
            filter(LandkreisId_Impfort %in% input$county_id,
                   Impfschutz          %in% input$vac_group) %>%
            mutate(kumulierte_Impfungen = cumsum(Anzahl))
        suppressWarnings(
            HTML(paste(strong("Insgesamt verabreichte Impfungen im Zeitraum: "),
              max(cum_vac$kumulierte_Impfungen)))
        )
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
