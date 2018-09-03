app <- shinytest::ShinyDriver$new("inst/app")
app$snapshotInit("sepal_width_bcpe")

app$snapshot()
app$setInputs(r_dataset = "iris")
# Input 'contents_rows_current' was set, but doesn't have an input binding.
# Input 'contents_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$snapshot()
app$setInputs(variable = "Sepal.Width")
app$snapshot()
app$setInputs(parameters = "4")
app$snapshot()
