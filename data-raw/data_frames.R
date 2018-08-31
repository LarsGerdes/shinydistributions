# read csv containing full distribution names and default parameters for 109
# density functions
distributions <- read.csv(file = "data-raw/distributions_ranges.csv",
                          header = TRUE, sep = ';', stringsAsFactors = FALSE)
# omit conditions for ranges
distributions <- distributions[, - c(12:22)]

# read csv containing relevant columns for tab "Properties of Distributions"
distributions_tab <- read.csv(file = "data-raw/distributions_tab_ranges.csv",
                              header = TRUE, sep = ';',
                              stringsAsFactors = FALSE, check.names = FALSE)
# replace NA's by "-" for visual purposes
distributions_tab[which(is.na(distributions_tab), arr.ind = TRUE)] <- "-"

# save as sysdata.rda
devtools::use_data(distributions, distributions_tab, internal = TRUE,
                   overwrite = TRUE)
