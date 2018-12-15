# Introduction
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")
devtools::install_github("r-lib/devtools")
devtools::has_devel()

# Tests
usethis::use_testthat()
# for unit tests with shiny
install.packages("shinytest")
devtools::install_github("rstudio/shinytest")
shinytest::dependenciesInstalled()
shinytest::installDependencies()


devtools::document()
testthat::test_dir("tests/testthat")
devtools::check()

# Reinstall
remove.packages("shinydistributions")
.rs.restartR()
devtools::install_github("LarsGerdes/shinydistributions")
shinydistributions::launch()
