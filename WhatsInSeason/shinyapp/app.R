library(shiny)
library(WhatsInSeason)

# Define UI WhatsInSeason app ----
ui <- fluidPage(

  # App title ----
  headerPanel("What's In Season?"),

  # Sidebar panel for inputs ----
  sidebarPanel(

    # Input: Selector for variable to plot  ----
        # selectInput(inputId = "datatype",
        #             label = "Type of Data:",
        #             c("Global",
        #               "National",
        #               "Regional")),
        #
        # textInput("food", "Food:",
        #           ),
        #
        # textInput("country", "Country:",
        #            ),

    textInput(inputId = "search",
              label = "search food",
              placeholder = "search food item here")

  ),

  # Main panel for displaying outputs ----
  mainPanel(

    # # Output: Formatted text for caption ----
    #     h3(textOutput("caption")),
    #
    # Output: Plot of the requested variable ----
    #imageOutput(outputId = "image"),
    #dataTableOutput(outputId = "table"),
    #plotOutput(outputId = "plot")

  )

  )

# Define server logic ----
server <- function(input, output) {

  # save objects to display to output$
  # use input values with input$
  #
  # output$plot <- renderPlot({
  #   # code here
  # })
  #
  # output$table <- renderTable({
  #   wf_country(input$food, input$country)

  output$

  })

}

shinyApp(ui, server)
