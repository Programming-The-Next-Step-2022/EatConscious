## Read an image of food items

library(httr)
library(jsonlite)
library(tidyverse)
library(magick)

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




