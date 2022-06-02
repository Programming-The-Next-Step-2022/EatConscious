# -------------------------------------------
# Tests for water footprint functions
# -------------------------------------------

test_that("wf_global returns correct dataframe", {

  wf_global <- wf_global("potato")
  df <- data.frame(Product = rep("Potatoes, fresh or chilled nes", 3),
                   WF_Type = as.factor(c("Green", "Blue", "Grey")),
                   AverageWF = c(191, 33, 63))
  row.names(df) <- c(154:156)

  expect_equal(wf_global, df)

})
