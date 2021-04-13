#please install packages if you have not installed them before

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)


#reading data
covid_data <- read_csv("covid19.csv")
diabetes_data <- read.csv("diabetes.csv")

#UI
ui <- fluidPage(theme = shinytheme("superhero"),
                
                themeSelector(),
                titlePanel("Your health your wealth"), 
                img(src = "th.jpg", height = 80, width = 95),
                
                sidebarLayout(position = "right",
                              sidebarPanel(
                                  
                                  
                                  
                                  selectInput(inputId = "country", "Select country",
                                              choices = unique(covid_data$country),
                                              selected = "Singapore"),
                                  
                                  dateRangeInput("date", strong("Date range"), start = "2020-03-03", end = "2021-03-29",
                                                 min = "2020-03-03", max = "2021-03-29"),
                                  
                                  checkboxInput(inputId = "moving", label = strong("Add Moving Average?"), value = FALSE),
                                  
                                  
                                  conditionalPanel(condition = "input.moving == true",
                                                   sliderInput(inputId = "f", label = "Moving Average size:",
                                                               min = 0.01, max = 1, value = 0.01, step = 0.01,
                                                               animate = animationOptions(interval = 100, loop = FALSE, playButton = "Click button!", pauseButton = "Do you want to pause it? Click here!")),
                                                   
                                  
                              ),
                              
                              hr(),
                              uiOutput("selects1"),
                              uiOutput("selects2"),
                              
                              ),
                              
                              
                              
                              mainPanel(
                                  hr(),
                                  plotOutput(outputId = "lineplot", click = "plot_click", width = "102%", height = "330px"),
                                  textOutput(outputId = "desc"),
                                  br(),
                                  verbatimTextOutput("covidnumber"),
                                  br(),
                                  tags$a(href = "https://www.statista.com/", "Source: Covid-19 daily new cases from Statista website", target = "_blank"),
                                  br(),
                                  tags$a(href = "https://www.kaggle.com/", "Source: Health indicator data is from Kaggle website", target = "_blank"),
                                  hr(),
                                  
                                  br(),
                                 
                                 
                                  column(5, plotlyOutput("plot2", width = 560 , height = 350)),
                                  
                                  
                                
                              )
                )
)

#server
server <- function(input, output) {
    output$selects1 <- renderUI({
        selectInput("select1", "Select first variable", choices=names(diabetes_data), selected = "Age")
        
    })

    output$selects2 <- renderUI({
        selectInput("select2", "Select second variable", choices=names(diabetes_data), selected = "Pregnancies")
        
    })
   
    
    plot_gg <- reactive({
        if(is.null(input$select1) || is.null(input$select2))
            return()
        else
        {
            g <- ggplot(diabetes_data, aes(x = get(input$select1), y = get(input$select2))) + geom_point(aes(), shape= 23, size = 1) + ggtitle("Health Plot")
            gg <- ggplotly(g) 
            return(gg)
        }
        
    })
 
    output$plot2 <- renderPlotly({
        if(is.null(plot_gg())) return()
        plot_gg()
        
        
    })
    
    selected_country <- reactive({
        
        req(input$date)
        
        validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
        covid_data %>%
            filter(
                country == input$country,
                date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2]
                ))
    })
    
    output$lineplot <- renderPlot({
        color = "#434343"
        par(bg = "gray")
        plot(x = selected_country()$date, y = selected_country()$covid19_cases, type = "l",
             xlab = "Date", ylab = "covid19cases", col = color, fg = color, col.lab = color, col.axis = color)
        if(input$moving){
            smooth_curve <- lowess(x = as.numeric(selected_country()$date), y = selected_country()$covid19_cases, f = input$f)
            lines(smooth_curve, col = "#E6553A", lwd = 3)
        }
    })
    output$covidnumber <- renderText({
        paste0("Covid-19 cases=", input$plot_click$y)
    })
    
    
}

# Create Shiny app
shinyApp(ui, server)