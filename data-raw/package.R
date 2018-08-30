# Introduction
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
install.packages("rstudioapi")
rstudioapi::isAvailable("0.99.149")
devtools::install_github("r-lib/devtools")
devtools::has_devel()

# Tests
usethis::use_testthat()

devtools::document()
devtools::check()
devtools::install_github("LarsGerdes/shinydistributions")
shinydistributions::launch()
