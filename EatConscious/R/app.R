#' @import shiny
#' @import shinythemes
#' @import gt
NULL

#' @importFrom magick image_read
NULL

# GUI wrapped in function ----

#'@export
GUI <- function() {

    # Define UI ----
    ui <- fluidPage(theme = shinytheme("flatly"),

      navbarPage(title = "Eat Conscious",
                 # Welcome page -----
                 tabPanel(icon("home"),

                          # General information -----
                          fluidRow(
                            column(width = 10,
                                   tags$h1("Welcome to the Graphical User Interface of the package EatConscious!"),
                                   tags$body("EatConscious is an r package that retrieves and visualizes nutritional
                                      and climate-impact data about most types of fresh produce."),
                                   HTML("<br>"),
                                   tags$body("Use the navigation pane above to view different food-related information."))),

                          # Water footprint information -----
                          fluidRow(
                            column(width = 8,
                                   tags$h3(icon("tint", lib = "glyphicon"), "What is a water footprint?"),
                                   tags$body("Over 2.7 billion people are affected by scarce water resources yearly.
                              The water footprint metric, created by Arjen Hoekstra, is a type of environmental
                              footprint that helps us to understand how the human consumption of different products
                              is affecting the earth's natural water sources. Making conscious choices about which products
                              to consume will aid in reducing our water foot print and ultimately preserve water for people and nature.
                              The water footprint data used in this package comes from The Water Footprint Network (https://waterfootprint.org/en/).
                              The dataset comprises the green, blue, and grey water footprints for a wide range of food
                                              products on a global, national, and regional scale."),
                                   HTML("<br><br><br>"),
                                   gt::gt_output("wf_types")),
                            column(width = 4,
                                   plotOutput("waterimg2"))),
                          tags$body("Read  more about the Water Footprint:"),
                          tags$a("https://waterfootprint.org/en/water-footprint/what-is-water-footprint/"),
                          HTML("<br><br>"),

                          # Nutritional data information -----
                          fluidRow(
                            column(width = 8,
                                   tags$h3(icon("carrot"), "Nutritional Data"),
                                   tags$body("You can also view and compare nutritional data of all sorts of food items."),
                                   tags$body("This includes the nutritional measures"),
                                   HTML("<ul>
                                   <li>Calories (kcal / serving)</li>
                                   <li>Percent nutrients (the percentage of necessary daily nutrients that one serving provides)</li>
                                   <li>Fat (grams / serving)</li>
                                   <li>Carbohydrates (grams / serving)</li>
                                   <li>Fibre (grams / serving)</li>
                                   </ul>")))),

                # Global Water Footprint page -----
                tabPanel("Global Water Footprint",
                         h3("Global Water Footprint Data"),
                         sidebarLayout(
                           sidebarPanel(textInput(inputId = "food",
                                                  label = "Search food",
                                                  placeholder = "Search food item here"),
                                        plotOutput(outputId = "pic")),
                           mainPanel(
                             tableOutput(outputId = "globaltable"),
                             plotOutput(outputId = "globalplot")))),

                # National Water Footprint page -----
                tabPanel("National Water Footprint",
                         h3("National Water Footprint Data"),
                         sidebarLayout(
                           sidebarPanel(textInput(inputId = "foodnational",
                                                  label = "Search food",
                                                  placeholder = "Search food item here"),
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

                # Regional Water Footprint Page -----
                tabPanel("Regional Water Footprint",
                         h3("Regional Water Footprint Data"),
                         sidebarLayout(
                           sidebarPanel(textInput(inputId = "countryname",
                                                  label = "Country:",
                                                  placeholder = "input country"),
                                        tags$body("Input the full country name starting with a Capital letter."),
                                        HTML("<br><br><br>"),
                                        textInput(inputId = "foodregional",
                                                  label = "Search food",
                                                  placeholder = "Search food item here")),
                           mainPanel(tabsetPanel(
                             tabPanel("Table", tableOutput(outputId = "regionaltable")),
                             tabPanel("Plot", plotOutput(outputId = "regionalplot",
                                                         width = 1200,
                                                         height = 900)))))),

                # Nutritional Data Page -----
                tabPanel("Nutritional Data",
                         h3("Nutritional Data"),
                         navlistPanel(widths = c(2, 10),
                           tabPanel("Search foods",
                                    sidebarLayout(
                                      sidebarPanel(textInput(inputId = "foodnutrition",
                                                             label = "Search food",
                                                             placeholder = "Search food item here")),
                                      mainPanel(tabsetPanel(
                                        tabPanel(title = "Table",
                                                 tableOutput("tablenutrition")),
                                        tabPanel(title = "Plot",
                                                 plotOutput("plotnutrition")))))),
                           tabPanel("Compare foods",
                                    sidebarLayout(
                                      sidebarPanel(textInput(inputId = "foodnutrition1",
                                                             label = "Food Item 1",
                                                             placeholder = "Search food item here"),
                                                   textInput(inputId = "foodnutrition2",
                                                             label = "Food Item 2",
                                                             placeholder = "Search food item here"),
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
                                                 tableOutput("tablenutrition1")),
                                        tabPanel(title = "Plot",
                                                 plotOutput("comparenutrition")))))))),
      ))


    # Define server logic ----
    server <- function(input, output) {

      output$waterimg2 <- renderPlot({
        img_url <- "https://images.unsplash.com/photo-1515150144380-bca9f1650ed9?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=687&q=80"
        pic <- image_read(img_url)
        par(mar = c(0, 0, 0, 0))
        plot(pic)
      })
      output$wf_types <- render_gt({
        dat <- data.frame(Type = c("Green", "Blue", "Grey"),
                          Explanation = c("Water acquired by the crop/plant directly from natural sources, such as precipitation.",
                                          "Water that has been sourced from surface or groundwater resources.",
                                          "The amount of freshwater that needs to be discharged into the water source to remove pollutants and meet water quality standards."))
        gt(dat) %>%
          tab_options(column_labels.hidden = TRUE)%>%
          data_color(columns = Type,
                     colors = c("lightblue", "darkseagreen", "slategrey"),
                     apply_to = c("fill", "text"),
                     autocolor_text = FALSE) %>%
          data_color(columns = Explanation,
                     colors = c("slategrey", "darkseagreen", "lightblue"),
                     apply_to = c("fill", "text"),
                     autocolor_text = FALSE,
                     alpha = 0.3)
      })
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


