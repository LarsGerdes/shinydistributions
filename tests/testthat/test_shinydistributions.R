app <- shinytest::ShinyDriver$new("inst/app")

testthat::test_that("exponential", {
  app$setInputs(dist = "Exponential - dEXP")
  output <- app$getValue(name = "info_text")
  testthat::expect_equal(output, "Location = 1")
})

app$stop()
