library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(tidyverse) 
library(data.table)
library(hrbrthemes)
library(tidycensus) 
library(zipcodeR)

zillow <- fread("zillow_all.csv")
x<- sort(unique(zillow$State_Name))

ui <- fluidPage(sidebarLayout(
    sidebarPanel(
        selectInput("state_shiny", "choose a state", x, selected = "Alabama"),
        selectInput("city_shiny", "select city", "placeholder1", selected = "Birmingham"),
        selectInput("zip_shiny", "select zipcode", "placeholder2", selected = "35205")
    ),
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Housing", plotOutput("plot2")),
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
            title = "Home Value"
        )
        new.y <- list(
            overlaying = "y",
            side = "right",
            title = "Rental Value"
        )
        plot_ly(zillow %>% 
                    filter(State_Name == input$state_shiny) %>% 
                    filter(City == input$city_shiny) %>% 
                    filter(Zipcode == input$zip_shiny)) %>%
            add_lines(x = ~Date, y = ~value_home, yaxis="y1", name = "Home") %>%
            add_lines(x = ~Date, y = ~value_rental, yaxis = "y2", name = "Rental") %>%
            
            # add_lines(x = ~Date, y = ~(zillow %>% 
            #                                filter(State_Name == input$state_shiny) %>% 
            #                                filter(City == input$city_shiny) %>% 
            #                                summarise(mean(value_home, na.rm = T))) , yaxis="y1", name = "Avg Home") %>%
            layout(yaxis2 = new.y, yaxis = old.y, xaxis = list(title="Year"))
        
    })
    
    output$my_tmap = renderLeaflet({
        tm <- tm_shape(df_final %>% filter(str_detect(State_Name, input$state_shiny))) +
            tm_polygons(Measurement, border.alpha = 0, id="NAME")
        tmap_leaflet(tm)
    })
  
        
}

# Run the application 
shinyApp(ui = ui, server = server)
