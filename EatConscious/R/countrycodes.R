# ============================================================================ #
#             Function to retrieve country codes by continent                  #
# ============================================================================ #


#'@import countrycode
#'@import dplyr
NULL

# -----

#'\emph{countrycodes}: retrieve country codes for the EatConscious package.
#'@param continent String argument; for which continent would you like to retrieve the country codes?
#'@return List of country codes within the specified continent.

#'@export
countrycodes <- function(continent) {

  dat <- wf_data
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


  country_codes <- data.frame(country = countrycode(countries, origin = "fips", destination = "country.name"),
                              continents = countrycode(countries, origin = "fips", destination = "continent"),
                              code = countries)
  na.omit(country_codes[country_codes$continents == continent, ])

}





