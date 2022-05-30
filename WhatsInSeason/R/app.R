library(shiny)
library(WhatsInSeason)
library(gt)

ui <- fluidPage(
  # Title
  headerPanel("Water Footprint Data for Food Production"),

  # All input
  textInput(inputId = "food",
            label = "search food",
            placeholder = "search food item here"),
  textInput(inputId = "country",
            label = "Country:",
            placeholder = "input country as countrycode"),
  selectInput(inputId = "continent",
              label = "Continent:",
              choices = c("Americas", "Africa", "Asia", "Oceania", "Europe")),

  # All output
  plotOutput(outputId = "pic"),
  tableOutput(outputId = "globaltable"),
  plotOutput(outputId = "globalplot"),
  tableOutput(outputId = "countrycodes"),
  tableOutput(outputId = "nationaltable"),
  plotOutput(outputId = "nationalplot"),
  tableOutput(outputId = "regionaltable"),
  plotOutput(outputId = "regionalplot"),

)

server <- function(input, output) {
  output$pic <- renderPlot({
    food_pic(input$food)
  })
  output$globalplot <- renderPlot({
    plot_wf_global(input$food)
  })
  output$globaltable <- renderTable({
    as.data.frame(wf_global(input$food))
  })
  output$nationaltable <- renderTable({
    as.data.frame(wf_country(input$food, input$country))
  })
  output$nationalplot <- renderPlot({
    wf_country_plot(input$food, input$continent)
  })
  output$countrycodes <- renderTable({
    as.data.frame(countrycodes(input$continent)[, c(1, 3)])
  })
  output$regionaltable <- renderTable({
    as.data.frame(wf_region(input$food, input$country))
  })
  output$regionalplot <- renderPlot({
    wf_region_plot(input$food, input$country)
  })
}

shinyApp(ui, server)








