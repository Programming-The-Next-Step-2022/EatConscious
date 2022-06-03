# -------------------------------------------
# Tests for search_food function
# -------------------------------------------

test_that("search_food returns the corresponding search hit", {

  sf <- search_food("potato")$nutrients
  expect_equal(adist(sf[1, 1], "potato", ignore.case = TRUE)[1], 0)

})

test_that("all search results contain search query", {

  sf <- search_food("potato")$nutrients[, "search_result"]
  expect_equal(length(grepl("potato", sf, ignore.case = TRUE)), 9)

})
