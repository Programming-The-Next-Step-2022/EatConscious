library(WhatsInSeason)

context("Core WhatsInSeason functionality")

# test_that("search_food returns the correct search hit", {
#
# })

test_that("search_food returns a table of the correct length", {

  sf <- search_food("potato")
  expect_equal(length(sf), 6)

})
