library(gamlss.dist)

distributions <- read.csv("distributions_df.csv", header = TRUE, sep = ";", 
                          stringsAsFactors = FALSE)

# last 5 rows are universal functions for specific usage (no explicit density 
# functions)
distributions <-  distributions[ - (110 : 114), ]

################################################################################
# get default parameters from density functions

### default_location (mu)
default_location <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  
  default_location[i] <- formals(distributions$density_fun[i])$mu[1]
}
default_location


### default_scale (sigma)
default_scale <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  if ( is.numeric(formals(distributions$density_fun[i])$sigma[1]) ) {
    
    default_scale[i] <- formals(distributions$density_fun[i])$sigma[1]
  }
}
default_scale


### default_skewness (nu)
default_skewness <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  if ( is.numeric(formals(distributions$density_fun[i])$nu[1]) ) {
    
    default_skewness[i] <- formals(distributions$density_fun[i])$nu[1]
  }
}
default_skewness

# There are cases of parameter "nu" where the value is not stored as class
# "numeric" but as class "call". Therefore it is not accessible with "$nu[1]"
# but "$nu" and needs to be converted from its class to "numeric".
# This does not occur for other parameters (mu, sigma, tau, bd).
nu_test <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)){
  # class "call" has structure "language"
  if (is.language(formals(distributions$density_fun[i])$nu[1])) {
    nu_test[i] <- 1
  } else {
    nu_test[i] <- 0
  }
}
# Positions of affected density functions
pos <- which(nu_test == 1)

# Affected density functions
distributions$density_fun[pos]
# "dSI", "dSICHEL", "dZASICHEL", "dZISICHEL"

# Assign actual values
for (i in pos[1]:pos[length(pos)]){
  
  if (is.language(formals(distributions$density_fun[i])$nu[1])) {
    
    default_skewness[i] <- as.numeric(paste(as.character(formals(
      distributions$density_fun[i])$nu), collapse = ''))
      # convert from class "call" to "numeric"
  }
}
# All affected density functions have the same default value of "-0.5" for "nu", 
# this is correct.
default_skewness[pos]


### default_kurtosis (tau)
default_kurtosis <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  if ( is.numeric(formals(distributions$density_fun[i])$tau[1]) ) {
    
    default_kurtosis[i] <- formals(distributions$density_fun[i])$tau[1]
  }
}
default_kurtosis


### default_binomial_denominator (bd)
default_binomial_denominator <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  if ( is.numeric(formals(distributions$density_fun[i])$bd[1]) ) {
    
    default_binomial_denominator[i] <- formals(
      distributions$density_fun[i])$bd[1]
  }
}
default_binomial_denominator


### Add fields for dafault parameters to df "distributions"
distributions$default_location <- default_location
distributions$default_scale <- default_scale
distributions$default_skewness <- default_skewness
distributions$default_kurtosis <- default_kurtosis
distributions$default_binomial_denominator <- default_binomial_denominator


### Add flag discrete distribution (discrete_flag)
discrete_gamlss_dist <- read.csv("discrete_gamlss_distributions_names.csv", 
                                 header = FALSE, sep = ';', 
                                 stringsAsFactors = FALSE)[, 2]
discrete_gamlss_dist <- data.frame("density_fun" = discrete_gamlss_dist, 
                                   stringsAsFactors = FALSE)
discrete_gamlss_dist$discrete_flag <- rep(1, nrow(discrete_gamlss_dist)) 
discrete_gamlss_dist

# left join distributions data frame by field "density_fun"
distributions <- dplyr::left_join(x = distributions,
                                      y = discrete_gamlss_dist)

# Add discrete_flag = 1 to 3 multinomial distributions (appearing only 
# aggregated to one record "dMULTIN" in discrete_gamlss_distributions_names) 
distributions[distributions$density_fun == "dMN3", "discrete_flag"] <- 1
distributions[distributions$density_fun == "dMN4", "discrete_flag"] <- 1
distributions[distributions$density_fun == "dMN5", "discrete_flag"] <- 1

# Add discrete_flag = 1 to dDBI (not appearing in 
# discrete_gamlss_distributions_names)
distributions[distributions$density_fun == "dDBI", "discrete_flag"] <- 1


# Add mixed distributions to discrete_flag assign a value of "2"
mixed_dist <- c("dBEOI", "dBEINF0", "dBEZI", "dBEINF1", "dBEINF", "dZAGA", 
                "dZAIG")
distributions$discrete_flag[match(mixed_dist, distributions$density_fun)] <- 2


# Assign "0" to continuous distributions in field "discrete_flag"
pos <- which(is.na(distributions$discrete_flag))
distributions$discrete_flag[pos] <- 0

# Add field dist_type containing string "discrete", "continuous" or "mixed"
distributions$dist_type <- rep("continuous", nrow(distributions))
distributions$dist_type[which(distributions$discrete_flag == 1)] <- "discrete"
distributions$dist_type[which(distributions$discrete_flag == 2)] <- "mixed"

# Add number of parameters to ALL records as quite a few are wrong in 
# gamlss.family documentation
# The binomial specific parameter "bd" is not included in count.

# Delete/replace all entries in field no_of_paramteres by NA's 
distributions$no_of_parameters <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions) ){
  
  #if ( is.na(distributions$no_of_parameters[i]) ) {
    # 5:8 = fields "default_location" to "default_kurtosis"
    distributions$no_of_parameters[i] <- sum(
                                      ifelse(is.na(distributions[i, 5:8]), 0, 1)
                                      )
  #}
}

# write to csv
# (seperated by semicolons since "," contained in names of normal distributions)
write.table(distributions,
          file = "distributions_default_params_109_bd_discrete_flag.csv",
          sep = ';',
          row.names = FALSE)

################################################################################
# Check for which numbers of paramters "bd" appears 
unique(distributions[! is.na(distributions$default_binomial_denominator), 
                     "no_of_parameters"])
# bd appears for functions with 1 to 3 parameters only


# Create default functions for reactive plot (based on number of parameters)
default_funct_per_param_number <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)){
  
  if (distributions$no_of_parameters[i] == 1) {
    
    if (is.na(distributions$default_binomial_denominator[i])){
      default_funct_per_param_number[i] <- paste("\"", 
        distributions$dist_density[i], "\" = ", "function(y) gamlss.dist::", 
        distributions$density_fun[i], "(y, mu = input$location)", ",", sep = '')
    } else { # add bd
      default_funct_per_param_number[i] <- paste("\"", 
        distributions$dist_density[i], "\" = ", "function(y) gamlss.dist::", 
        distributions$density_fun[i], 
        "(y, mu = input$location, bd = input$binomial_denominator)", 
        ",", sep = '')
    }
    
  } else if (distributions$no_of_parameters[i] == 2) {
    
    if (is.na(distributions$default_binomial_denominator[i])){
      default_funct_per_param_number[i] <- paste("\"", 
        distributions$dist_density[i], "\" = ", "function(y) gamlss.dist::", 
        distributions$density_fun[i],
        "(y, mu = input$location, sigma = input$scale)", ",", sep = '')
    } else { # add bd
      default_funct_per_param_number[i] <- paste("\"", 
        distributions$dist_density[i], "\" = ", "function(y) gamlss.dist::", 
        distributions$density_fun[i],
        "(y, mu = input$location, sigma = input$scale, bd = input$binomial_denominator)",
        ",", sep = '')
    }
    
  } else if (distributions$no_of_parameters[i] == 3) {
    
    if (is.na(distributions$default_binomial_denominator[i])){
      default_funct_per_param_number[i] <- paste("\"", 
        distributions$dist_density[i], "\" = ", "function(y) gamlss.dist::", 
        distributions$density_fun[i], 
        "(y, mu = input$location, sigma = input$scale, nu = input$skewness)", 
        ",", sep = '')
    } else {# add bd
      default_funct_per_param_number[i] <- paste("\"", 
        distributions$dist_density[i], "\" = ", "function(y) gamlss.dist::", 
        distributions$density_fun[i], 
        "(y, mu = input$location, sigma = input$scale, nu = input$skewness, bd = input$binomial_denominator)",
        ",", sep = '')
    }
    
  } else if (distributions$no_of_parameters[i] == 4) {
    
    default_funct_per_param_number[i] <- paste("\"", 
      distributions$dist_density[i], "\" = ", "function(y) gamlss.dist::", 
      distributions$density_fun[i], 
      "(y, mu = input$location, sigma = input$scale, nu = input$skewness, tau = input$kurtosis)", 
      ",", sep = '')
  }
}
default_funct_per_param_number


# write to csv
# write.csv(default_funct_per_param_number,
#          file = "default_funct_per_param_number_109_bd.csv",
#          row.names = FALSE)