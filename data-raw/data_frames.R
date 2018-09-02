# read csv containing full distribution names and default parameters for 109
# density functions
distributions <- read.csv(file = "data-raw/distributions_ranges.csv",
                          header = TRUE, sep = ';', stringsAsFactors = FALSE)
# omit conditions for ranges
distributions <- distributions[, - c(12:22)]
#distributions[which(distributions$location_lower_bound == 0e+00), "location_lower_bound"] <- 1e-10
#distributions[which(!is.na(distributions$location_upper_bound)), "location_upper_bound"] <- 0.9999999
#distributions[which(!is.na(distributions$scale_lower_bound)), "scale_lower_bound"] <- 1e-10
#distributions[which(!is.na(distributions$skewness_lower_bound)), "skewness_lower_bound"] <- 1e-10
#distributions[which(distributions$skewness_upper_bound == 1), "skewness_upper_bound"] <- 0.9999999
#distributions[which(distributions$kurtosis_lower_bound == 0e+00), "kurtosis_lower_bound"] <- 1e-10

# read csv containing relevant columns for tab "Properties of Distributions"
distributions_tab <- read.csv(file = "data-raw/distributions_tab_ranges.csv",
                              header = TRUE, sep = ';',
                              stringsAsFactors = FALSE, check.names = FALSE)
# replace NA's by "-" for visual purposes
distributions_tab[which(is.na(distributions_tab), arr.ind = TRUE)] <- "-"

# save as sysdata.rda
devtools::use_data(distributions, distributions_tab, internal = TRUE,
                   overwrite = TRUE)
