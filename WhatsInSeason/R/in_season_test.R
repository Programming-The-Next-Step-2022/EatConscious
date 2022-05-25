## A basic prototype to test what the WhatsInSeason package will do.

#' @export
whatsinseason <- function (season, country) {
foods <- data.frame(country = rep("Netherlands", 20),
                    season = rep(c("spring", "summer", "autumn", "winter"), each= 5),
                    food = c("asparagus", "beetroot", "radish", "cucumber", "rhubarb",
                               "courgette", "endive", "leek", "onion", "pea",
                               "cauliflower", "fennel", "kale", "pumpkin", "raddicio",
                               "brussels sprouts", "swede", "parsnip", "cabbage", "broccoli"))
country = foods$country
print(foods[foods$season == season,]$food)
}

