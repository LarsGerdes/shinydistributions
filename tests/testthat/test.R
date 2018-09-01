library(shinytest)
library(testthat)

context("Ranges of x-axis")

# open shiny app
app <- ShinyDriver$new(path = "inst/app")

test_that("poison", {
  app$setInputs(dist = "Exponential - dEXP")
  output <- app$getValue(name = "info_text")
  expect_equal(output, "Location = 1")
})

# stop shiny app
app$stop()

