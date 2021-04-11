#please install the packages if you have not installed them before

library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

#reading data
covid_data <- read_csv("covid19.csv")

#UI
ui <- fluidPage(theme = shinytheme("journal"),
                titlePanel("Covid-19 daily new cases"), 

                sidebarLayout(position = "right",
                    sidebarPanel(
                        tags$style("body{background-color:linen; color:brown}"),
                        
                        
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
                        )
                    ),
                    
                    mainPanel(
                        plotOutput(outputId = "lineplot", click = "plot_click", width = "102%", height = "330px"),
                        textOutput(outputId = "desc"),
                        br(),
                        verbatimTextOutput("covidnumber"),
                        br(),
                    
                        tags$a(href = "https://www.statista.com/", "Source: Covid-19 daily new cases from Statista website", target = "_blank"),
                        
                    )
                )
)

#server
server <- function(input, output) {

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