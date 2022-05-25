# ============================================================================ #
#           Nutrients functions for the package "WhatsInSeason"                #
# ============================================================================ #

# Required packages ------------------------------------------------------------

#' @import httr
#' @import dplyr
#' @import ggplot2
#' @import patchwork
#' @importFrom jsonlite fromJSON
NULL
#' @importFrom magick image_read
NULL

# ------------------------------------------------------------------------------
# search_food
# ------------------------------------------------------------------------------

#' \emph{search_food}: search for food items.
#'
#' @param food The food item, inputed as a string, that you would like to search for.
#' @param picture Logical argument. Should a picture accompany the search result? Default = TRUE.
#' @return A table of nutritional information for each search hit and a picture of the first hit.
#' @examples
#' search_food("potato")
#'
#' @export
search_food <- function(food, picture = TRUE) {

  enquo(food)
  if (mode(food) != "character") {
    stop ("search query must be entered as a string")
  }


  url <- "https://edamam-food-and-grocery-database.p.rapidapi.com/parser"
  queryString <- list(ingr = food)
  food_raw <- VERB("GET", url,
                   add_headers('X-RapidAPI-Host' = 'edamam-food-and-grocery-database.p.rapidapi.com',
                               'X-RapidAPI-Key' = '1da758cd3bmsha511e784e601b31p14129ejsn7bcd3da0b34e'),
                   query = queryString, content_type("application/octet-stream"))
  food_char <- rawToChar(food_raw$content)
  food_dat <- fromJSON(food_char)
  search_result <- as.data.frame(food_dat$hints)
  search_result <- search_result %>%
    select(-measures)
  search_result <- data.frame(matrix(unlist(search_result), nrow=nrow(search_result), byrow=FALSE))
  colnames(search_result) = c("ID", "url", "search_result", "calories", "percent_nutrients", "fat", "carbohydrates",
                              "fibre", "category", "cat_label", "image", "contents_label", "brand")
  search_result <- search_result %>%
    filter(category == "Generic foods")

  if (picture == TRUE) {
    img_url <- search_result$image[1]
    pic <- image_read(img_url)
    print(pic)
  }

  search_result <- as.data.frame(search_result[,3:8])
  return(search_result)

}


# ------------------------------------------------------------------------------
# get_nutrients
# ------------------------------------------------------------------------------

#' \emph{get_nutrients}: get nutritional information for food items.
#'
#' @param food  The food item, inputed as a string, you would like to search for.
#'
#' @param measure The type of nutritional information, inputed as a string, per serving of the specified food item.
#'                "calories" = The amount of calories (kcal).
#'                "percent_nutrients" = the percentage of the daily needed intake of nutrients in the food item.
#'                "fat" = amount of fat in grams.
#'                "carbohydrates" = amount of carbohydrates in grams.
#'                "fibre" = amount of fibre in grams.
#'                "all" = all nutrional information.
#' @return A table of nutritional information for the food item.
#' @examples
#' get_nutrients("potato", "all") # get all nutritional information for all search hits of "potato"
#' get_nutrients("potato", "fibre")[1,] # get fibre information for the first search hit of "potato"
#'
#' @export
get_nutrients <- function(food, measure) {

  food <- enquo(food)
  food <- as.character(food)[2]
  measure <- enquo(measure)
  measure <- as.character(measure)[2]

  url <- "https://edamam-food-and-grocery-database.p.rapidapi.com/parser"

  queryString <- list(ingr = food)
  food_raw <- VERB("GET", url,
                   add_headers('X-RapidAPI-Host' = 'edamam-food-and-grocery-database.p.rapidapi.com',
                               'X-RapidAPI-Key' = '1da758cd3bmsha511e784e601b31p14129ejsn7bcd3da0b34e'),
                   query = queryString, content_type("application/octet-stream"))

  food_char <- rawToChar(food_raw$content)
  food_dat <- fromJSON(food_char)
  search_result <- as.data.frame(food_dat$hints)
  search_result <- search_result %>%
    select(-measures)

  search_result <- data.frame(matrix(unlist(search_result), nrow=nrow(search_result), byrow=FALSE))
  colnames(search_result) = c("ID", "url", "label", "calories", "percent_nutrients", "fat", "carbohydrates",
                              "fibre", "category", "cat_label", "image", "contents_label", "brand")

  search_result <- search_result %>%
    filter(category == "Generic foods")

  if (measure == "fat") {
    return((search_result[,c("label", "fat")]))
  } else if (measure == "calories") {
    return(search_result[,c("label", "calories")])
  } else if (measure == "percent_nutrients") {
    return(search_result[,c("label", "percent_nutrients")])
  } else if (measure == "carbohydrates") {
    return(search_result[,c("label", "carbohydrates")])
  } else if (measure == "fibre") {
    return(search_result[,c("label", "fibre")])
  } else {
    return(search_result[,c(3:8)])
  }

}

# ------------------------------------------------------------------------------
# compare_nutrients
# ------------------------------------------------------------------------------

#' \emph{compare_nutrients}: compare the nutritional information of two food items.
#'
#' @param itemA The first food item, inputed as a string, which you'd like to compare.
#' @param itemB The second food item, inputed as a string, which you'd like to compare.
#' @param measure The type of nutritional information, inputed as a string, which you'd like to compare the food on.
#'                "calories" = The amount of calories (kcal).
#'                "percent_nutrients" = the percentage of the daily needed intake of nutrients in the food item.
#'                "fat" = amount of fat in grams.
#'                "carbohydrates" = amount of carbohydrates in grams.
#'                "fibre" = amount of fibre in grams.
#' @return A barplot with the two items on the x-axis and the specified nutritional measure on the y-axis.
#' @examples
#' compare_nutrients("potato", "sweet potato", "carbohydrates")
#' @export
compare_nutrients <- function(itemA, itemB, measure) {

  itemA <- enquo(itemA)
  itemA <- as.character(itemA)[2]
  itemB <- enquo(itemB)
  itemB <- as.character(itemB)[2]
  measure <- enquo(measure)
  measure <- as.character(measure)[2]

  a <- search_food(itemA, picture = FALSE)[1,]
  b <- search_food(itemB, picture = FALSE)[1,]
  dat <- rbind(a, b)

  ggplot(data = dat, aes(x = search_result, y = as.numeric(dat[, measure]))) +
    geom_bar(stat = "identity", aes(fill = search_result)) +
    ylab(measure) +
    theme_minimal() +
    scale_fill_manual(values=c('plum','lightblue4')) +
    xlab("food")

}

# ------------------------------------------------------------------------------
# plot_nutrients
# ------------------------------------------------------------------------------

#' \emph{plot_nutrients}: plot the nutritional information of a specified food item.
#' @param food The food item as a string.
#' @return Two pie charts with the percentage of nutrients based on the recommended daily intake that one serving of the food item gives.
#' @examples
#' plot_nutrients("potato")
#'
#' @export
plot_nutrients <- function(food) {

  food <- enquo(food)
  food <- as.character(food)[2]

  url <- "https://edamam-food-and-grocery-database.p.rapidapi.com/parser"
  queryString <- list(ingr = food)
  food_raw <- VERB("GET", url,
                   add_headers('X-RapidAPI-Host' = 'edamam-food-and-grocery-database.p.rapidapi.com',
                               'X-RapidAPI-Key' = '1da758cd3bmsha511e784e601b31p14129ejsn7bcd3da0b34e'),
                   query = queryString, content_type("application/octet-stream"))
  food_char <- rawToChar(food_raw$content)
  food_dat <- fromJSON(food_char)
  search_result <- as.data.frame(food_dat$hints)
  search_result <- search_result %>%
    select(-measures)
  search_result <- data.frame(matrix(unlist(search_result), nrow=nrow(search_result), byrow=FALSE))
  colnames(search_result) = c("ID", "url", "label", "calories", "percent_nutrients", "fat", "carbohydrates",
                              "fibre", "category", "cat_label", "image", "contents_label", "brand")
  search_result <- search_result %>%
    filter(category == "Generic foods")

  dat2 <- as.data.frame(t(search_result[1, 4:5]))
  dat2$label <- c("calories", "daily_nutrient_intake")
  colnames(dat2) <- c("value", "label")
  dat2$value <- as.numeric(dat2$value)
  dat2[1, 1] <- (dat2[1, 1]/2000)*100

  plot2 <- ggplot(dat2, aes(x = label, y = value, fill = label)) +
    geom_bar(stat = "identity") +
    coord_polar("y", start = 0) +
    ylim(0, 100) +
    scale_fill_manual(values = c('darksalmon', 'lightblue4')) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(legend.position="none")

  dat3 <- as.data.frame(t(search_result[1, 6:8]))
  dat3$label <- c("fat", "carbohydrates", "fibre")
  colnames(dat3) <- c("value", "label")
  dat3$value <- as.numeric(dat3$value)
  dat3[1, 1] <- (dat3[1, 1]/44)*100
  dat3[2, 1] <- (dat3[2, 1]/225)*100
  dat3[3, 1] <- (dat3[3, 1]/25)*100

  plot3 <- ggplot(dat3, aes(x = label, y = value, fill = label)) +
    geom_bar(stat = "identity") +
    coord_polar("y", start = 0) +
    ylim(0, 100) +
    scale_fill_manual(values = c('darkseagreen', 'lightgoldenrod', 'plum')) +
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(legend.position="none")

  plot2 + plot3

}


# ============================================================================ #
# ============================================================================ #
# ============================================================================ #

# notes

# a way to not require string input in argument?
# make default and optional arguments in functions? (e.g. "all" default if nothing else specified)
# get_nutrients and search_food too similar, find way to make different!
# make error messages
# update documentation on get_nutrients (all does not need to be specified)



