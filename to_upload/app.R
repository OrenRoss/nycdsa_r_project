library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(tidyverse) 
library(data.table)
library(hrbrthemes)
library(tidycensus) 
library(zipcodeR)
hrbrthemes::import_roboto_condensed()

zillow <- fread("zillow_all.csv")
x<- sort(unique(zillow$State_Name))

ui <- fluidPage(sidebarLayout(
    sidebarPanel(
        selectInput("state_shiny", "Choose a State", x, selected = "Alabama"),
        selectInput("city_shiny", "Select City", "placeholder1", selected = "Birmingham"),
        selectInput("zip_shiny", "Select Zipcode", "placeholder2", selected = "35205"),
        radioButtons("radio", h3("Map Level"),
                     choices =  c("Population" = "population_estimate",
                                         "Income" = "income_estimate",
                                         "Poverty" = "poverty_index",
                                         "School Quality" = "school_index",
                                         "Jobs" = "jobs_index",
                                         "Labor" = "labor_index",
                                         "Transportation" = "transport_index",
                                         "Transportation Cost" = "transportation_cost_index",
                                         "Health Hazards" = "hazard_index"),
                     selected = "population_estimate")
    ),
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Housing", plotlyOutput("plot2")),
                    tabPanel("Maps", leafletOutput("my_tmap"))
    ))))

server <- function(input, output, session){
    observe({
        updateSelectInput(session, "city_shiny",
                          choices = (zillow %>% 
                                         filter(State_Name == input$state_shiny) %>% 
                                         distinct(City) %>% arrange(City))[[1]])
    })
    
    observeEvent(input$city_shiny, {
        city_levels <- (zillow %>% 
                            filter(State_Name == input$state_shiny) %>% 
                            distinct(City))[[1]]
        updateSelectInput(session, "zip_shiny", choices = (zillow %>% 
                                                               filter(State_Name == input$state_shiny) %>% 
                                                               filter(City == input$city_shiny) %>% 
                                                               distinct(Zipcode) %>% arrange(Zipcode))[[1]])
    })
    
    output$plot2 <- renderPlotly({
        old.y <- list(
            side = "left",
            title = "Zillow Home Value Index"
        )
        new.y <- list(
            overlaying = "y",
            side = "right",
            title = "Zillow Observed Rent Index"
        )
        plot_ly(zillow %>% 
                    filter(State_Name == input$state_shiny) %>% 
                    filter(City == input$city_shiny) %>% 
                    filter(Zipcode == input$zip_shiny)) %>%
            add_lines(x = ~Date, y = ~value_home, yaxis="y1", name = "ZHVI") %>%
            add_lines(x = ~Date, y = ~value_rental, yaxis = "y2", name = "ZORI") %>%
            layout(yaxis2 = new.y, yaxis = old.y, xaxis = list(title="Year"))
        
    })
    
    output$my_tmap = renderLeaflet({
        tm <- tm_shape(df_final %>% filter(State_Name == input$state_shiny)) +
            tm_polygons(input$radio, border.alpha = 0, id="NAME")
        tmap_leaflet(tm)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



############ Playing here ############


# if (input$radio == "State")  {tm_shape(df_final %>% filter(State_Name == input$state_shiny)) +
#         tm_polygons(Measurement, border.alpha = 0, id="NAME")}   
# if (input$radio == "County")  {tm_shape(df_final %>% filter(State_Name == input$state_shiny)) +
#         tm_polygons(Measurement, border.alpha = 0, id="NAME")}   
# 
# tmap_leaflet(tm)
# 
# test_data <- switch(input$radio,
#                State = df_final %>% 
#                             filter(str_detect(State_Name, input$state_shiny)),
#                County = df_final %>% 
#                             filter(NAME == (zillow_all %>% 
#                                 filter(City == input$city_shiny) %>% 
#                                     select(CountyName)[[1,1]])))
# tm <- tm_shape(test_data)  + tm_polygons(Measurement, border.alpha = 0, id="NAME")
# tmap_leaflet(tm)