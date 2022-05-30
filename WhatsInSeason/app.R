library(shiny)
library(WhatsInSeason)

# Define UI WhatsInSeason app ----
ui <- fluidPage(

  navbarPage(title = "Water Footprint Data",
    tabPanel("Global Water Footprint",
             sidebarLayout(
               sidebarPanel(textInput(inputId = "food",
                                      label = "search food",
                                      placeholder = "search food item here"),
                            plotOutput(outputId = "pic")),
               mainPanel(
                 tableOutput(outputId = "globaltable"),
                 plotOutput(outputId = "globalplot")))),

    # tabPanel("National Water Footprint",
    #          sidebarLayout(
    #            sidebarPanel(textInput(inputId = "foodnational",
    #                                   label = "Search food",
    #                                   placeholder = "search food item here"),
    #                         textInput(inputId = "countrycode",
    #                                   label = "Country Code:",
    #                                   placeholder = "enter FIPS code"),
    #                         selectInput(inputId = "continent",
    #                                     label = "Continent:",
    #                                     choices = c("Americas", "Africa", "Asia", "Oceania", "Europe")),
    #                         tableOutput(outputId = "countrycodes")),
    #            mainPanel(
    #              tableOutput(outputId = "nationaltable"),
    #              plotOutput(outputId = "nationalplot")))),

    tabPanel("Regional Water Footprint",
             sidebarLayout(
               sidebarPanel(textInput(inputId = "foodregional",
                                      label = "search food",
                                      placeholder = "search food item here"),
                            textInput(inputId = "countryname",
                                      label = "Country:",
                                      placeholder = "input country")),
               mainPanel(
                 tableOutput(outputId = "regionaltable"),
                 plotOutput(outputId = "regionalplot")))),
    )

)



# Define server logic ----
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
    as.data.frame(wf_country(input$foodnational, input$countrycode))
  })
  output$nationalplot <- renderPlot({
    wf_country_plot(input$foodnational, input$continent)
  })
  output$countrycodes <- renderTable({
    as.data.frame(countrycodes(input$continent))[, c(1, 3)]
  })
  output$regionaltable <- renderTable({
    as.data.frame(wf_region(input$foodregional, input$countryname))
  })
  output$regionalplot <- renderPlot({
    wf_region_plot(input$foodregional, input$countryname)
  })

}

shinyApp(ui, server)







