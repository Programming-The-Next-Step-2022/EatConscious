# ============================================================================ #
#          Water footprint functions for the package "WhatsInSeason"           #
# ============================================================================ #

#' @import tidyverse
#' @importFrom plotly ggplotly

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

GlobalWF <- function(food) {

  food <- enquo(food)
  food <- as.character(food)[2]

  global_wf_table <- dat_global[grep(food, dat_global$Product, ignore.case = TRUE),]
  global_wf_table$AverageWF <- as.numeric(global_wf_table$AverageWF)
  global_wf_table$WF_Type <- as.factor(global_wf_table$WF_Type)

  global_wf_table[1:3, ]
}

# ------------------------------------------------------------------------------
# Visualize global water footprint
# ------------------------------------------------------------------------------

GlobalWF_plot <- function(food) {

  food <- enquo(food)
  food <- as.character(food)[2]

  dat_plot <- dat_global[grep(food, dat_global$Product, ignore.case = TRUE),]
  dat_plot <- dat_plot[1:3, ]
  dat_plot$AverageWF <- as.numeric(dat_plot$AverageWF)

  p <- ggplot(data = dat_plot,
         mapping = aes(x = WF_Type, y = AverageWF, fill = WF_Type)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("lightblue", "darkseagreen", "slategrey")) +
    labs(fill = "Water Footprint") +
    ggtitle(paste0("Global Water Footprint of Food Product: ", food)) +
    ylab("Global Average WF (cubicL/ton)") +
    theme_minimal()

  p

}

# ------------------------------------------------------------------------------
# Water footprint per country
# ------------------------------------------------------------------------------

WFCountry <- function(food, country) {

  food <- enquo(food)
  food <- as.character(food)[2]
  country <- enquo(country)
  country <- as.character(country)[2]

  country_wf_table <- dat_bycountry[grep(food, dat_global$Product, ignore.case = TRUE),]
  country_wf_table <- country_wf_table[, c("Product", "WF_Type", country)]

  country_wf_table[1:3, ]

}

# ------------------------------------------------------------------------------
# Visualize water footprint per country
# ------------------------------------------------------------------------------

WFCountry_plot <- function(food) {

  food <- enquo(food)
  food <- as.character(food)[2]

  country_wf_table <- dat_bycountry[grep(food, dat_global$Product, ignore.case = TRUE),]
  dat_plot <- as.data.frame(t(country_wf_table[1:3, ]))[-c(1:2), ]
  colnames(dat_plot) <- c("Green", "Blue", "Grey")

  dat_plot <- data.frame(country = rep(colnames(country_wf_table)[3:ncol(country_wf_table)], each = 3),
                         WF_Type = NA,
                         avgWF = NA)
  dat_plot$WF_Type <- rep(c("Green", "Blue", "Grey"), times = nrow(dat_plot)/3)
  dat_plot$avgWF[seq(1, nrow(dat_plot), by = 3)] <- as.numeric(t(country_wf_table[1, 3:ncol(country_wf_table)]))
  dat_plot$avgWF[seq(2, nrow(dat_plot), by = 3)] <- as.numeric(t(country_wf_table[2, 3:ncol(country_wf_table)]))
  dat_plot$avgWF[seq(3, nrow(dat_plot), by = 3)] <- as.numeric(t(country_wf_table[3, 3:ncol(country_wf_table)]))
  dat_plot <- dat_plot %>%
    arrange(avgWF)

  p <- ggplot(data = dat_plot, aes(x = country, y = avgWF, fill = WF_Type)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("lightblue", "darkseagreen", "slategrey")) +
    labs(fill = "Water Footprint") +
    ylab("Global Average WF (cubicL/ton)") +
    ggtitle(paste0("Water Footprint of ", food, " per country")) +
    theme_minimal()

  ggplotly(p)
}


# ------------------------------------------------------------------------------
# Water footprint per region in country
# ------------------------------------------------------------------------------


WFRegion <- function(food, country, rows = 3) {

  food <- enquo(food)
  food <- as.character(food)[2]
  country <- enquo(country)
  country <- as.character(country)[2]

  country_wf_table <- dat_byregion[, c(1:2, grep(country, dat_byregion))]
  country_wf_table <- country_wf_table[grep(food, dat_byregion$Product, ignore.case = TRUE), ]

  country_wf_table[1:rows, ]

}

# ------------------------------------------------------------------------------
# Visualize water footprint per region in country
# ------------------------------------------------------------------------------

WFRegion_plot <- function(food, country) {

  food <- enquo(food)
  food <- as.character(food)[2]
  country <- enquo(country)
  country <- as.character(country)[2]

  country_wf_table <- dat_byregion[, c(1:2, grep(country, dat_byregion))]
  country_wf_table <- country_wf_table[grep(food, dat_byregion$Product, ignore.case = TRUE), ]

  dat_plot <- as.data.frame(t(country_wf_table[1:3, ]))[-c(1:2), ]
  colnames(dat_plot) <- c("Green", "Blue", "Grey")

  dat_plot <- data.frame(region = rep(colnames(country_wf_table)[3:ncol(country_wf_table)], each = 3),
                         WF_Type = NA,
                         avgWF = NA)
  dat_plot$WF_Type <- rep(c("Green", "Blue", "Grey"), times = nrow(dat_plot)/3)
  dat_plot$avgWF[seq(1, nrow(dat_plot), by = 3)] <- as.numeric(t(country_wf_table[1, 3:ncol(country_wf_table)]))
  dat_plot$avgWF[seq(2, nrow(dat_plot), by = 3)] <- as.numeric(t(country_wf_table[2, 3:ncol(country_wf_table)]))
  dat_plot$avgWF[seq(3, nrow(dat_plot), by = 3)] <- as.numeric(t(country_wf_table[3, 3:ncol(country_wf_table)]))

  p <- dat_plot %>%
    group_by(WF_Type) %>%
    arrange(avgWF) %>%

    ggplot(aes(x = region, y = avgWF, fill = WF_Type)) +
    geom_bar(stat = "identity", width = 0.9) +
    scale_fill_manual(values = c("lightblue", "darkseagreen", "slategrey")) +
    labs(fill = "Water Footprint") +
    ylab("Global Average WF (cubicL/ton)") +
    ggtitle(paste0("Water Footprint of ", food, " per Region in ", country)) +
    theme_minimal()

  ggplotly(p)

}


# Green = water from precipitation
# Blue = water that has been sourced from surface or groundwater resources
# Grey = the amount of fresh water required to assimilate pollutants to meet specific water quality standards


