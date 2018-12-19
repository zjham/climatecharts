library(tidyverse)
library(shiny)
library(lubridate)
library(extrafont)
library(DT)
library(RColorBrewer)
library(shinythemes)
library(plotly)


theme_set(theme_minimal() +
          theme(text = element_text(family = "Verdana"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank()))

climate_daily <- read_csv("data/climate_daily.csv")
climate_monthly <- read_csv("data/climate_monthly.csv")
climate_yearly <- read_csv("data/climate_yearly.csv")
stations <- read_csv("data/station_info.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("simplex"),
    # Application title
    titlePanel("Canadian Climate Charts"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "cities",
                        label = "Cities to compare",
                        choices = unique(climate_daily$city),
                        selected = c("Vancouver", "Toronto"),
                        multiple = TRUE),
            
            selectInput(inputId = "observe",
                        label = "Observation Type",
                        choices = c("Temperature" = "temp",
                                    "Precipitation" = "precip"),
                        selected = "temp"),
            
            conditionalPanel("input.observe == 'precip'",
                             checkboxInput(inputId = "cumulative",
                                           label = "Show cumulative totals",
                                           value = FALSE)),
            
            radioButtons(inputId = "freq", 
                         label = "Frequency",
                         choices = c("Daily" = "daily",
                                     "Monthly" = "monthly",
                                     "Yearly" = "yearly"),
                         selected = "daily"),
            
            conditionalPanel("input.freq == `daily`",
                             dateRangeInput(inputId = "date_d", label = "Date range to display", 
                                            start = "2018-01-01", end = "2018-12-01",
                                            min = "2004-01-01", max = today(),
                                            startview = "month" )),
                            
            
            conditionalPanel("input.freq == `monthly`",
                             dateRangeInput(inputId = "date_m", label = "Date range to display", 
                                            start = "2016-01-01", end = "2018-12-01",
                                            min = "2004-01-01", max = today(),
                                            startview = "year", format = "yyyy-mm")),
            
            conditionalPanel("input.freq == `yearly`",
                             dateRangeInput(inputId = "date_y", label = "Date range to display", 
                                            start = "2010-01-01", end = "2018-12-01",
                                            min = "2004-01-01", max = today(),
                                            startview = "decade", format = "yyyy"))
        ),
        


        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Chart",
                         plotlyOutput("lineplot"),
                         DT::dataTableOutput(outputId = "extremes")),
                tabPanel("Station Info",
                         DT::dataTableOutput("stations")),
                tabPanel("About",
                         textOutput("copy"),
                         br(),
                         textOutput("about")))
           
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
     datasetInput <- reactive({
        date_range <- switch(input$freq,
                       "daily" = input$date_d,
                       "monthly" = input$date_m, 
                       "yearly" = input$date_y)
                        
        switch(input$freq,
                "daily" = climate_daily,
                "monthly" = climate_monthly,
                "yearly" = climate_yearly) %>%
         filter(date >= as_date(date_range[1]),
                date <= as_date(date_range[2]),
                city %in% input$cities,
                !is.na(precip),
                !is.na(temp)) %>%
         select(city, date, temp, precip, temp_min, temp_max) %>%
         mutate(city = fct_reorder(city, get(input$observe), mean, .desc = TRUE))
         
    })
     output$lineplot <- renderPlotly({
         req(input$cities)
         if (input$observe == "temp") {
           p <- datasetInput() %>%
                ggplot(aes_string(x = "date", y = input$observe)) +
                geom_line(aes(col = city)) +
                geom_point(aes(col = city)) +
                geom_ribbon(aes(ymin = temp_min, ymax = temp_max, fill = city), alpha = 0.2) +
                scale_color_manual(name = "City", values = brewer.pal(9, "Set1")) +
                scale_fill_manual(values = brewer.pal(9, "Set1"), guide = FALSE) +
                theme(text = element_text(size = 14)) +
                guides(fill = FALSE) +
                labs(title = paste("Mean", input$freq, "temperature"),
                     subtitle = "Ribbons indicate min/max observations",
                     x = "",
                     y = "Temperature, (C)")
           
           ggplotly(p)
            
        } else if (input$cumulative) {
           p <-  datasetInput() %>%
                group_by(city) %>%
                mutate(cum_precip = cumsum(precip)) %>% 
                ggplot(aes(date, cum_precip, col = city)) +
                geom_line(size = .9) +
                geom_point() +
                theme(text = element_text(size = 14)) +
                scale_colour_discrete(name = "City") +
                labs(title = "Cumulative Precipitation",
                     x = "",
                     y = "Total Precipitation (mm)")  
            
           ggplotly(p)
           
        } else {
            p <- datasetInput() %>%
                ggplot(aes_string("date", input$observe, col = "city", fill = "city")) +
                geom_point() +
                geom_col(position = "identity", alpha = .5) +
                theme(text = element_text(size = 14)) +
                scale_colour_discrete(name = "City") +
                guides(fill = FALSE) +
                labs(title = paste("Total precipitation", input$freq, "values"),
                     x = "",
                     y = "Precipitation (mm)") 
            
            ggplotly(p)
        }
    })
     
    output$extremes <- DT::renderDataTable(caption = "Extreme values from selected time period",
                                           {
        req(input$cities)
        datasetInput() %>%
            group_by(city) %>%
            mutate(max_obs = max(get(input$observe)),
                   min_obs = min(get(input$observe))) %>%
            filter(get(input$observe) == max_obs | get(input$observe) == min_obs) %>%
            mutate("Observation" = case_when(get(input$observe) == max_obs ~ max_obs,
                                             get(input$observe) == min_obs ~ min_obs,
                                             TRUE ~ NA_real_),
                   "Observation Type" = case_when(get(input$observe) == max_obs ~ "Max",
                                                  get(input$observe) == min_obs ~ "Min",
                                                  TRUE ~ NA_character_)) %>%
            group_by(city, `Observation Type`, `Observation`) %>%
            slice(1) %>%
            ungroup() %>%
            select(city, date, `Observation Type`, `Observation`)
    })
    
    output$stations <- DT::renderDataTable({
        stations %>%
            filter(city %in% input$cities) %>%
            group_by(city) %>%
            arrange(desc(station_id)) %>%
            slice(1) %>%
            select(-climate_id, -WMO_id)
        
        
    })
    
    output$copy <- renderText({
        "Data obtained from Environment and Climate Change Canada (ECCC) using the weathercan package"
    })
    
    output$about <- renderText({
    "Major Canadian cities were chosen based on availability and consistency of data at airport weather stations"

    })
    
}

    
# Run the application 
shinyApp(ui = ui, server = server)
