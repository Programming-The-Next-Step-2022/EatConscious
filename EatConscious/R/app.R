#' @import shiny
#' @import EatConscious

# GUI wrapped in function ----

#'@export
GUI <- function() {

  # Define UI ----
  ui <- fluidPage(

    navbarPage(title = "Eat Conscious",

               tabPanel("Welcome!",
                        tags$body("enter info about app here"),
                        # img(src="image.jpg",
                        #     height = 500)),
               ),
                        # Global Water Footprint page
                        tabPanel("Global Water Footprint",
                                 sidebarLayout(
                                   sidebarPanel(textInput(inputId = "food",
                                                          label = "search food",
                                                          placeholder = "search food item here"),
                                                plotOutput(outputId = "pic")),
                                   mainPanel(
                                     tableOutput(outputId = "globaltable"),
                                     plotOutput(outputId = "globalplot")))),

                        # National Water Footprint page
                        tabPanel("National Water Footprint",
                                 sidebarLayout(
                                   sidebarPanel(textInput(inputId = "foodnational",
                                                          label = "Search food",
                                                          placeholder = "search food item here"),
                                                selectInput(inputId = "continent",
                                                            label = "Continent:",
                                                            choices = c("Americas", "Africa", "Asia", "Oceania", "Europe")),
                                                textInput(inputId = "countrycode",
                                                          label = "Country Code:",
                                                          placeholder = "enter FIPS code"),
                                                tags$body("Country code should be entered according to the FIPS countrycodes.
                                      See the table below for an overview of country codes within the selected continent."),
                                                tags$body(" "),
                                                tableOutput(outputId = "countrycodes")),
                                   mainPanel(tabsetPanel(
                                     tabPanel("Table", tableOutput(outputId = "nationaltable")),
                                     tabPanel("Plot", plotOutput(outputId = "nationalplot",
                                                                 width = 900,
                                                                 height = 900)))))),

                        # Regional Water Footprint Page
                        tabPanel("Regional Water Footprint",
                                 sidebarLayout(
                                   sidebarPanel(textInput(inputId = "countryname",
                                                          label = "Country:",
                                                          placeholder = "input country"),
                                                tags$body("Input the full country name starting with a Capital letter."),
                                                tags$body(" "),
                                                textInput(inputId = "foodregional",
                                                          label = "search food",
                                                          placeholder = "search food item here")),
                                   mainPanel(tabsetPanel(
                                     tabPanel("Table", tableOutput(outputId = "regionaltable")),
                                     tabPanel("Plot", plotOutput(outputId = "regionalplot",
                                                                 width = 1200,
                                                                 height = 900)))))),
                        # tableOutput(outputId = "regionaltable"),
                        # plotOutput(outputId = "regionalplot",
                        #            width = 1200,
                        #            height = 900)))),

                        # Nutritional Data Page
                        tabPanel("Nutritional Data",
                                 sidebarLayout(
                                   sidebarPanel(textInput(inputId = "foodnutrition",
                                                          label = "search food",
                                                          placeholder = "search food item here"),
                                                tags$h4("Compare nutritional data"),
                                                textInput(inputId = "foodnutrition1",
                                                          label = "Food Item 1",
                                                          placeholder = "search food item here"),
                                                textInput(inputId = "foodnutrition2",
                                                          label = "Food Item 2",
                                                          placeholder = "search food item here"),
                                                selectInput(inputId = "nutritionmeasures_comp",
                                                            label = "Select nutrition measure",
                                                            choices = c("calories",
                                                                        "percent_nutrients",
                                                                        "fat",
                                                                        "carbohydrates",
                                                                        "fibre",
                                                                        "all")),
                                                tags$body("Please note: selection [all] does not work for plotting.")),

                                   mainPanel(tabsetPanel(
                                     tabPanel(title = "Table",
                                              tableOutput("tablenutrition"),
                                              tableOutput("tablenutrition1")),
                                     tabPanel(title = "Plot",
                                              plotOutput("plotnutrition"),
                                              plotOutput("comparenutrition"))))
                                 ))
               )

    )



    # Define server logic ----
    server <- function(input, output) {

      output$pic <- renderPlot({
        food_pic(input$food)
      })
      output$globalplot <- renderPlot({
        wf_global_plot(input$food)
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
      output$tablenutrition <- renderTable({
        as.data.frame(get_nutrients(input$foodnutrition, "all"))
      })
      output$tablenutrition1 <- renderTable({
        a <- as.data.frame(get_nutrients(input$foodnutrition1, input$nutritionmeasures_comp))[1, ]
        b <- as.data.frame(get_nutrients(input$foodnutrition2, input$nutritionmeasures_comp))[1, ]
        rbind(a, b)
      })
      output$plotnutrition <- renderPlot({
        plot_nutrients(input$foodnutrition)
      })
      output$comparenutrition <- renderPlot({
        compare_nutrients(input$foodnutrition1, input$foodnutrition2, input$nutritionmeasures_comp)
      })

    }

    # Run App ----
    shinyApp(ui, server)

}








