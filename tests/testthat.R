testthat::context("exponential")
#open shiny app
app <- shinytest::ShinyDriver$new(path = "inst/app")

testthat::test_that("exponential", {
  #set numeric input
  app$setInputs(dist = "Exponential - dEXP")
  #get output
  output <- app$getValue(name = "info_text")
  #test
  testthat::expect_equal(output, "Location = 1")
})

#stop shiny app
app$stop()

testthat::test_check("shinydistributions")

