# read csv containing full distribution names and default parameters for 109
# density functions
distributions <- read.csv(
  file = "data-raw/distributions_default_params_109_bd_discrete_flag.csv",
  stringsAsFactors = FALSE)

# read csv containing relevant columns for tab "Properties of Distributions"
distributions_tab <- read.csv("data-raw/distributions_tab.csv", header = TRUE,
                              stringsAsFactors = FALSE, check.names=FALSE)
# replace NA's by "-" for visual purposes
distributions_tab[which(is.na(distributions_tab), arr.ind = TRUE)] <- "-"

# save as sysdata.rda
devtools::use_data(distributions, distributions_tab, internal = TRUE,
                   overwrite = TRUE)

# use testthat
usethis::use_testthat()
devtools::install_github("LarsGerdes/shinydistributions")
devtools::check()
shinydistributions::launch()
