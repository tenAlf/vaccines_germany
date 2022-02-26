# This is a Rstudio shiny app with the purpose to graphically show the number
# of vaccines administered on a county level

library(shiny)
library(readr)
library(tidyverse)
library(plotly)
# Get Data ----------------------------------------------------------------

source("data_prep.R")
link <- '<a href="https://github.com/tenAlf/vaccines_germany">GitHub</a>'

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
                                   "Drittimpfung" = 3),
                               selected = c(1, 2, 3)),
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
            plotlyOutput("vacplot", width = "100%"),
            tableOutput("total_vac"))
    ),
      hr(),
      HTML(paste0("Letzte Datenaktualisierung: ", last_update)),
      br(),
      HTML(paste0("Projektbeschreibung und Quellenverweise auf ", link))
)

# Define server logic for displaying a cumulative plot --------------------
server <- 
    function(input, output) {
    output$vacplot <- 
        renderPlotly({
        # prepare date range values
        date_start <-
            as.Date(input$date_span[1])
        date_end   <-
            as.Date(input$date_span[2])
            
        col_line_condition <-
            as.numeric(difftime(date_start, date_end, units = "days")) == 0
        
        # keep selected county and selected Groups within date range
        plot_df <-
            vac_data %>%
            filter(
                LandkreisId_Impfort %in% input$county_id,
                Impfschutz          %in% input$vac_group,
                between(Impfdatum, date_start, date_end)) %>%
            # generate grouped cumsum based on checkboxInputs from ui.R
            group_by(Impfdatum, Impfschutz) %>%
            summarise(Impfungen_am_Tag = sum(Anzahl), .groups = "drop") %>%
            group_by(Impfschutz) %>%
            mutate(Gesamtimpfungen_seit_Beginn = cumsum(Impfungen_am_Tag))
        
        
        # prep the graph
        vac_plot_spec <- 
            ggplot(data = plot_df, 
                mapping = aes(
                  x     = Impfdatum, 
                  y     = Gesamtimpfungen_seit_Beginn,
                  label = Impfungen_am_Tag)) +
            scale_color_manual(
              values = custom_cols, 
              labels = custom_labs) +
            labs(
              caption = custom_cap, 
              x       = "", 
              y       = "") +
            
            theme(
              axis.text.y  = element_text(face = "bold", size = 13), 
              axis.text.x  = element_text(face = "bold", size = 13),
              plot.caption = element_text(hjust = 0, size = 10),
              legend.text  = element_text(size = 12),
              legend.title = element_text(size = 12.5))
        
        if(col_line_condition == TRUE) {
            vac_plot <- 
                vac_plot_spec + geom_col(aes(fill   = Impfschutz), 
                                         position = "dodge")
        } else {
            vac_plot <- 
                vac_plot_spec + geom_line(aes(color = Impfschutz))
            
        }

        # draw the graph
        ggplotly(vac_plot, 
                 dynamicTicks = TRUE, 
                 tooltip = c("Impfdatum", 
                             "Impfungen_am_Tag", 
                             "Gesamtimpfungen_seit_Beginn"))
            
        
    })
    
    # Print out the total number of admin. vaccines in timespan.   
    output$total_vac <- renderTable({
         
        # prepare date range
        date_start <- 
            as.Date(input$date_span[1])
        date_end   <- 
            as.Date(input$date_span[2])
        
        # Prepare value change for readability
        name_change <- 
            function(x){
                case_when(
                    x == 1 ~ "Erstimpfung",
                    x == 2 ~ "Zweitimpfung",
                    x == 3 ~ "Drittimpfung")
            }
        
        # prepare data for the output
        cum_vac <- 
            vac_data %>% 
            filter(LandkreisId_Impfort %in% input$county_id,
                   Impfschutz          %in% input$vac_group,
                   between(Impfdatum, date_start, date_end)) %>%
            mutate(Kategorie = "Alle",
                   kumulierte_Impfungen = cumsum(Anzahl)) %>% 
            select(Kategorie, 
                   "Summe Impfungen im Zeitraum" = kumulierte_Impfungen) %>% 
            slice_tail()
        
        cum_vac_grouped <- 
            vac_data %>% 
            filter(LandkreisId_Impfort %in% input$county_id,
                   Impfschutz          %in% input$vac_group,
                   between(Impfdatum, date_start, date_end)) %>%
            mutate(Impfschutz = name_change(Impfschutz)) %>% 
            group_by(Impfschutz) %>% 
            summarise(kumulierte_Impfungen = cumsum(Anzahl), 
                      .groups              = "keep") %>% 
            select(Kategorie = Impfschutz,
                   "Summe Impfungen im Zeitraum" = kumulierte_Impfungen) %>% 
            slice_tail()
            
        # Message to print
        suppressWarnings(
            bind_rows(cum_vac, cum_vac_grouped) %>% 
            arrange((factor(Kategorie, 
                            levels = c("Alle", 
                                       "Erstimpfung", 
                                       "Zweitimpfung", 
                                       "Drittimpfung"))))
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

