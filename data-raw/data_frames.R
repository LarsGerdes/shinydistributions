# read csv containing full distribution names and default parameters for 109
# density functions
distributions <- read.csv(file = "data-raw/distributions_ranges.csv",
                          header = TRUE, sep = ';', stringsAsFactors = FALSE)
# omit conditions for ranges
distributions <- distributions[, - c(12:22)]
# improving bounds
distributions[which(distributions$scale_lower_bound == 1e-10),
              "scale_lower_bound"] <- 1e-6
distributions[which(distributions$scale_lower_bound == 0),
              "scale_lower_bound"] <- 1e-2
distributions[distributions$dist_density == "Logit Normal - dLOGITNO",
              "x_upper_bound"] <- 1




# read csv containing relevant columns for tab "Properties of Distributions"
distributions_tab <- read.csv(file = "data-raw/distributions_tab_ranges.csv",
                              header = TRUE, sep = ';',
                              stringsAsFactors = FALSE, check.names = FALSE)
# replace NA's by "-" for visual purposes
distributions_tab[which(is.na(distributions_tab), arr.ind = TRUE)] <- "-"

# save as sysdata.rda
usethis::use_data(distributions, distributions_tab, internal = TRUE,
                  overwrite = TRUE)

# distributions[which(distributions$location_lower_bound == 1e-10),
#               "location_lower_bound"] <- 1e-6
# distributions[which(distributions$location_lower_bound == 0),
#               "location_lower_bound"] <- 1e-2
# distributions[which(distributions$skewness_lower_bound == 1e-10),
#               "skewness_lower_bound"] <- 1e-6
# distributions[which(distributions$skewness_lower_bound == 0),
#               "skewness_lower_bound"] <- 1e-2
# distributions[which(distributions$kurtosis_lower_bound == 1e-10),
#               "kurtosis_lower_bound"] <- 1e-6
# distributions[which(distributions$kurtosis_lower_bound == 0),
#               "kurtosis_lower_bound"] <- 1e-2
# distributions[which(distributions$location_upper_bound == 0.99999999),
#               "location_upper_bound"] <- 0.999999
# distributions[which(distributions$scale_upper_bound == 0.99999999),
#               "scale_upper_bound"] <- 0.999999
# distributions[which(distributions$skewness_upper_bound == 0.99999999),
#               "skewness_upper_bound"] <- 0.999999
# distributions[which(distributions$kurtosis_upper_bound == 0.99999999),
#               "kurtosis_upper_bound"] <- 0.999999
