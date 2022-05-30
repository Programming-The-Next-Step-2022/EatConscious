# ============================================================================ #
#               Search function for the package "EatConscious"                 #
# ============================================================================ #

#'@import httr
#'@import jsonlite
#'@import tidyverse
#'@import magick
#'@import EatConscious
NULL

# ------------------------------------------------------------------------------
# search_food
# ------------------------------------------------------------------------------

#' \emph{search_food}: search for food items.
#'
#' @param food The food item, input as a string, that you would like to search for.
#' @param picture Logical argument. Should a picture accompany the search result? Default = TRUE.
#' @return A list of nutritional and climate-impact information for each search hit and a picture of the first hit.
#' @examples
#' search_food("potato")
#'
#' @export
search_food <- function(food, picture = TRUE) {

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

    food_pic(food)
  }

  search_result <- as.data.frame(search_result[,3:8])

  list <- return(list("nutrients" = search_result, "water_footprint" = wf_global(food)))
  print(list)

}

# ------------------------------------------------------------------------------
# return picture of food
# ------------------------------------------------------------------------------

#'@export
food_pic <- function(food) {
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

  img_url <- search_result$image[1]
  pic <- image_read(img_url)
  plot(pic)
}

