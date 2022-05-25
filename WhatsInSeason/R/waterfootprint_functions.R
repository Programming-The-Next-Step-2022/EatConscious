# ============================================================================ #
#          Water footprint functions for the package "WhatsInSeason"           #
# ============================================================================ #

#' @import tidyverse
#' @importFrom plotly ggplotly
#' @import countrycode
NULL

# ------------------------------------------------------------------------------
# Data
# ------------------------------------------------------------------------------

dat <- read.csv("waterfootprintReport.csv", sep = ";", header = FALSE)
dat <- dat[, c(4, 9:ncol(dat))]

dat_global <- dat[-c(1:6, 1066:nrow(dat)), 1:3]
colnames(dat_global) <- c("Product", "WF_Type", "AverageWF")

all_products <- data.frame(product = dat_global[, 1])
all_products$product <- ifelse(all_products$product == "", NA, all_products$product)
products <- rep(all_products$product, each = 3)
nona_products = products[!is.na(products)]
all_products$product <- nona_products
dat_global$Product <- all_products$product

dat_bycountry <- dat[-c(1,3, 1066:nrow(dat)), 9:ncol(dat)]
colnames(dat_bycountry) <- dat_bycountry[1,]
dat_bycountry <- dat_bycountry[-c(1:4), grep("CNTRY-average", dat_bycountry)]
dat_bycountry$Product <- all_products$product
dat_bycountry$WF_Type <- dat_global$WF_Type
dat_bycountry <- dat_bycountry %>%
  select(Product, WF_Type, everything())
countries = colnames(dat_bycountry)[3:ncol(dat_bycountry)]
continents <- c(rep(NA, 2), countrycode(countries, origin = "fips", destination = "continent"))
dat_bycountry <- rbind(continents, dat_bycountry)


dat_byregion <- dat[-c(1:3, 1066:nrow(dat)), ]
colnames(dat_byregion) <- dat_byregion[2,]
dat_byregion <- dat_byregion[-c(2:3), -c(1, grep("CNTRY-average", dat_byregion))]
dat_byregion$Product <- c(NA, all_products$product)
dat_byregion$WF_Type <- c(NA, dat_global$WF_Type)
dat_byregion <- dat_byregion %>%
  select(Product, WF_Type, everything())

# ------------------------------------------------------------------------------
# Global water footprint
# ------------------------------------------------------------------------------

#' \emph{wf_global}: retrieve the global water footprint of a food item.
#'
#' @param food Any food item.
#' @return A list, including a table with the global blue, green, and grey water footprint of the food item and nutritional information about the food item.
#' @examples
#' wf_global(potato)
#'
#' @export
wf_global <- function(food) {

  food <- enquo(food)
  food <- as.character(food)[2]

  global_wf_table <- dat_global[grep(food, dat_global$Product, ignore.case = TRUE),]
  global_wf_table$AverageWF <- as.numeric(global_wf_table$AverageWF)
  global_wf_table$WF_Type <- as.factor(global_wf_table$WF_Type)

  return(list("water footprint" = global_wf_table[1:3, ], "nutrition" = search_food(food)))

}

# ------------------------------------------------------------------------------
# Visualize global water footprint
# ------------------------------------------------------------------------------

#' \emph{plot_wf_global}: visualize the global water footprint of a food item.
#'
#' @param food Any food item.
#' @return A plot with the global blue, green, and grey water footprint of the food item.
#' @examples
#' plot_wf_global(potato)
#'
#' @export
plot_wf_global <- function(food) {

  food <- enquo(food)
  food <- as.character(food)[2]

  dat_plot <- dat_global[grep(food, dat_global$Product, ignore.case = TRUE),]
  dat_plot <- dat_plot[1:3, ]
  dat_plot$AverageWF <- as.numeric(dat_plot$AverageWF)

  p <- ggplot(data = dat_plot,
         mapping = aes(x = WF_Type, y = AverageWF, fill = WF_Type)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(labels = c("sourced water", "water from precipitation", "fresh water required to assimilate pollutants"),
                      values = c("lightblue", "darkseagreen", "slategrey")) +
    labs(fill = "Water Footprint") +
    ggtitle(paste0("Global Water Footprint of Food Product: ", food)) +
    xlab("Water Footprint") +
    ylab("Global Average WF (cubicL/ton)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

  p

}

# ------------------------------------------------------------------------------
# Water footprint per country
# ------------------------------------------------------------------------------

#' \emph{wf_country}: retrieve the water footprint of a food item in a specific country.
#'
#' @param food Any food item.
#' @param country A two-letter country code according to the FIPS country codes.
#' @return A table with the blue, green, and grey water footprint of the food item in that country.
#' @examples
#' wf_country(potato, NL)
#'
#' @export
wf_country <- function(food, country) {

  food <- enquo(food)
  food <- as.character(food)[2]
  country <- enquo(country)
  country <- as.character(country)[2]

  country_wf_table <- dat_bycountry[grep(food, dat_bycountry$Product, ignore.case = TRUE),]
  country_wf_table <- country_wf_table[, c("Product", "WF_Type", country)]

  country_wf_table[1:3, ]

}

# ------------------------------------------------------------------------------
# Visualize water footprint across countries
# ------------------------------------------------------------------------------

#' \emph{wf_country_plot}: plot the water footprint of a food item across countries in a continent.
#'
#' @param food Any food item.
#' @param continent Specify the continent. Options: Americas, Africa, Asia, Oceania, Europe.
#' @return A plot visualizing the blue, green, and grey water footprint of the food item per country in the specified continent.
#' @examples
#' wf_country_plot(potato, Europe)
#'
#' @export
wf_country_plot <- function(food, continent) {

  food <- enquo(food)
  food <- as.character(food)[2]
  continent <- enquo(continent)
  continent <- as.character(continent)[2]

  country_wf_table <- dat_bycountry[grep(food, dat_global$Product, ignore.case = TRUE),][, grep(continent, dat_bycountry)]
  dat_plot <- as.data.frame(t(country_wf_table[1:3, ]))[-c(1:2), ]
  colnames(dat_plot) <- c("Green", "Blue", "Grey")

  dat_plot <- data.frame(Country = rep(colnames(country_wf_table)[3:ncol(country_wf_table)], each = 3),
                         WF_Type = NA,
                         avgWF = NA)
  dat_plot$WF_Type <- rep(c("Green", "Blue", "Grey"), times = nrow(dat_plot)/3)
  dat_plot$avgWF[seq(1, nrow(dat_plot), by = 3)] <- as.numeric(t(country_wf_table[1, 3:ncol(country_wf_table)]))
  dat_plot$avgWF[seq(2, nrow(dat_plot), by = 3)] <- as.numeric(t(country_wf_table[2, 3:ncol(country_wf_table)]))
  dat_plot$avgWF[seq(3, nrow(dat_plot), by = 3)] <- as.numeric(t(country_wf_table[3, 3:ncol(country_wf_table)]))
  dat_plot <- dat_plot %>%
    arrange(avgWF)

  p <- ggplot(data = dat_plot, aes(x = Country, y = avgWF, fill = WF_Type)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(labels = c("sourced water", "water from precipitation", "fresh water required to assimilate pollutants"),
                      values = c("lightblue", "darkseagreen", "slategrey")) +
    labs(fill = "Water Footprint") +
    ylab("Global Average WF (cubicL/ton)") +
    ggtitle(paste0("Water Footprint of ", food, " per country in ", continent)) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank()) +
    coord_polar(theta = "x")

  p


  # Need to revert to standard barchart to make interactive chart work:
  # ggplotly(p) %>%
  #   layout(showlegend = FALSE,
  #          xaxis = list(tite = FALSE,
  #                       showticklabels=FALSE))

}


# ------------------------------------------------------------------------------
# Water footprint per region in country
# ------------------------------------------------------------------------------

#' \emph{wf_region}: retrieve the water footprint of a food item across regions of a country.
#'
#' @param food Any food item.
#' @param country A two-letter country code.
#' @return A table of the blue, green, and grey water footprint of the food item per region in the specified country.
#' @examples
#' wf_region(potato, Netherlands)
#'
#' @export
wf_region <- function(food, country) {

  food <- enquo(food)
  food <- as.character(food)[2]
  country <- enquo(country)
  country <- as.character(country)[2]

  country_wf_table <- dat_byregion[, c(1:2, grep(country, dat_byregion))]
  country_wf_table <- country_wf_table[grep(food, dat_byregion$Product, ignore.case = TRUE), ]

  country_wf_table[1:3, ]

}

# ------------------------------------------------------------------------------
# Visualize water footprint per region in country
# ------------------------------------------------------------------------------

#' \emph{wf_region_plot}: plot the water footprint of a food item across regions of a country.
#'
#' @param food Any food item.
#' @param country A two-letter country code.
#' @return A plot of the blue, green, and grey water footprint of the food item per region in the specified country.
#' @examples
#' wf_region_plot(potato, Netherlands)
#'
#' @export
wf_region_plot <- function(food, country) {

  food <- enquo(food)
  food <- as.character(food)[2]
  country <- enquo(country)
  country <- as.character(country)[2]

  country_wf_table <- dat_byregion[, c(1:2, grep(country, dat_byregion))]
  country_wf_table <- country_wf_table[grep(food, dat_byregion$Product, ignore.case = TRUE), ]

  dat_plot <- as.data.frame(t(country_wf_table[1:3, ]))[-c(1:2), ]
  colnames(dat_plot) <- c("Green", "Blue", "Grey")

  dat_plot <- data.frame(Region = rep(colnames(country_wf_table)[3:ncol(country_wf_table)], each = 3),
                         WF_Type = NA,
                         avgWF = NA)
  dat_plot$WF_Type <- rep(c("Green", "Blue", "Grey"), times = nrow(dat_plot)/3)
  dat_plot$avgWF[seq(1, nrow(dat_plot), by = 3)] <- as.numeric(t(country_wf_table[1, 3:ncol(country_wf_table)]))
  dat_plot$avgWF[seq(2, nrow(dat_plot), by = 3)] <- as.numeric(t(country_wf_table[2, 3:ncol(country_wf_table)]))
  dat_plot$avgWF[seq(3, nrow(dat_plot), by = 3)] <- as.numeric(t(country_wf_table[3, 3:ncol(country_wf_table)]))

  p <- dat_plot %>%
    group_by(WF_Type) %>%
    arrange(avgWF) %>%

    ggplot(aes(x = Region, y = avgWF, fill = WF_Type)) +
    geom_bar(stat = "identity", width = 0.9) +
    scale_fill_manual(labels = c("sourced water", "water from precipitation", "fresh water required to assimilate pollutants"),
                      values = c("lightblue", "darkseagreen", "slategrey")) +
    labs(title = paste0("Water Footprint of ", food, " per Region in ", country),
         fill = "Water Footprint") +
    ylab("Average WF (cubicL/ton)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust=1),
          plot.title = element_text(hjust = 0.5))

  p

  #ggplotly(p)

}


# Green = water from precipitation
# Blue = water that has been sourced from surface or groundwater resources
# Grey = the amount of fresh water required to assimilate pollutants to meet specific water quality standards



