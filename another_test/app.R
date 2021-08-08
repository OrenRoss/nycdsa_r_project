library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(tidyverse) 
library(data.table)
library(hrbrthemes)


zillow <- fread("zillow_all.csv")


ui <- fluidPage(sidebarLayout(
    sidebarPanel(
        selectInput("state_shiny", "choose a state", c("MA", "NY"), selected = "NY"),
        selectInput("city_shiny", "select city", "placeholder1", selected = "New York"),
        selectInput("zip_shiny", "select zipcode", "placeholder2", selected = "10001")
    ),
    mainPanel(
        plotlyOutput("plot2"))
    ))



server <- function(input, output, session){
    # dataset <- reactive({
    #     get(input$state)
    # })
    
    observe({
        updateSelectInput(session, "city_shiny",
                          choices = (zillow %>% 
                                         filter(State == input$state_shiny) %>% 
                                         distinct(City))[[1]])
        
    })
    
    observeEvent(input$city_shiny, {
        city_levels <- (zillow %>% 
                            filter(State == input$state_shiny) %>% 
                            distinct(City))[[1]]
        
    
        updateSelectInput(session, "zip_shiny", choices = (zillow %>% 
                                                               filter(State == input$state_shiny) %>% 
                                                               filter(City == input$city_shiny) %>% 
                                                               distinct(Zipcode))[[1]])
    })
    
    
    output$plot2 <- renderPlotly({
        
        
        old.y <- list(
            side = "left",
            title = "Home"
        )
        
        new.y <- list(
            overlaying = "y",
            side = "right",
            title = "Rental"
        )
        
        plot_ly(zillow %>% 
                    filter(State == input$state_shiny) %>% 
                    filter(City == input$city_shiny) %>% 
                    filter(Zipcode == input$zip_shiny)) %>%
            add_lines(x = ~Date, y = ~value_home, yaxis="y1", name = "Home") %>%
            add_lines(x = ~Date, y = ~value_rental, yaxis = "y2", name = "Rental") %>%
            layout(yaxis2 = new.y, yaxis = old.y, xaxis = list(title="Property Value"))
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
