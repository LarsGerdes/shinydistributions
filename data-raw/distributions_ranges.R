distributions_ranges <- read.csv(
  file = "distributions_ranges_conditions.csv",
  header = TRUE,
  sep = ';',
  stringsAsFactors = FALSE)

# lower only
unique(distributions_ranges$location_lower_condition)
unique(distributions_ranges$scale_lower_condition)
unique(distributions_ranges$skewness_lower_condition)
unique(distributions_ranges$kurtosis_lower_condition)
unique(distributions_ranges$x_lower_condition)

# upper only
unique(distributions_ranges$location_upper_condition)
unique(distributions_ranges$scale_upper_condition)
unique(distributions_ranges$skewness_upper_condition)
unique(distributions_ranges$kurtosis_upper_condition)
unique(distributions_ranges$x_upper_condition)

# Exceptions
distributions_ranges[which(distributions_ranges$x_lower_condition == "<mu"), 
                     "density_fun"]
body(dRGE) # if (any(x < mu - (sigma/nu)))
distributions_ranges$x_lower_condition[which(
  distributions_ranges$x_lower_condition == "<mu")] <- "mu-(sigma/nu)"


# nu != 0, i.e. nu is fixed to zero! mu <= 0
distributions_ranges[which(
  distributions_ranges$skewness_lower_condition == "!=0"), "density_fun"]
body(dLNO) # if (any(nu != 0 & mu <= 0))


# Assign zero to both lower and upper condition as nu is fixed to zero for 
# "dLNO". (nu != 0)
distributions_ranges$skewness_lower_condition[which(
  distributions_ranges$skewness_lower_condition == "!=0")] <- 0

distributions_ranges[
  distributions_ranges$density_fun == "dLNO", "skewness_upper_condition"] <- 0


# Assign lower condition to location/mu (as it was included in condition 
# of skewness/nu)
distributions_ranges[distributions_ranges$density_fun == "dLNO", 
                     "location_lower_condition"] <- 0


# Other distributions with fixed parameters.
#body(dNET)
#body(dNOF)

# Additional condition for tau (dNET)
# if (any(tau < nu)) 
#   stop(paste(" tau must greater or equal than  nu",

# Actual kurtosis_lower_condition for dNET is "<=0 & tau<nu"
# Therefore final lower bound of kurtosis is "nu", i.e. the current value of nu!
distributions_ranges[distributions_ranges$density_fun == "dNET", 
                     "kurtosis_lower_condition"] <- "tau<nu"

# Rules lower bounds
# if 1st char "<" and 2nd char != "=" -> assign int as lower bound
# if 2nd char "=" -> 1e-10 # "=" appears for zeros only
# if "0" -> next
# kurtosis: if "tau<nu" -> "nu" -> "skewness"
# x: if "<mu-(sigma/nu)" -> "mu-(sigma/nu)" -> "location - (scale/skewness)"

# Rules upper bounds
# if 1st char ">" and 2nd char != "=" -> assign int as upper bound
# if 2nd char "=" -> 0.9999999999 # "=" appears for ones only


# Assign lower bounds ##########################################################

location_lower_bound <- rep(NA, nrow(distributions_ranges))

for (i in 1:nrow(distributions_ranges)) {
  if (is.na(distributions_ranges$location_lower_condition[i])) {
    next
  } else if (substr(distributions_ranges$location_lower_condition[i], 1, 1) == "<" &
      substr(distributions_ranges$location_lower_condition[i], 2, 2) != "=") {
    
    location_lower_bound[i] <- as.integer(substr(
      distributions_ranges$location_lower_condition[i], 2, 2)
    )
  } else if (substr(
    distributions_ranges$location_lower_condition[i], 2, 2) == "="){
    location_lower_bound[i] <- 1e-10
    
  } else if (distributions_ranges$location_lower_condition[i] == 0){
    next
  }
}


scale_lower_bound <- rep(NA, nrow(distributions_ranges))

for (i in 1:nrow(distributions_ranges)) {
  if (is.na(distributions_ranges$scale_lower_condition[i])) {
    next
  } else if (substr(distributions_ranges$scale_lower_condition[i], 1, 1) == "<" &
      substr(distributions_ranges$scale_lower_condition[i], 2, 2) != "=") {
    
    scale_lower_bound[i] <- as.integer(substr(
      distributions_ranges$scale_lower_condition[i], 2, 2)
    )
  } else if (substr(
    distributions_ranges$scale_lower_condition[i], 2, 2) == "="){
    scale_lower_bound[i] <- 1e-10
    
  } else if (distributions_ranges$scale_lower_condition[i] == 0){
    next
  }
}


skewness_lower_bound <- rep(NA, nrow(distributions_ranges))

for (i in 1:nrow(distributions_ranges)) {
  if (is.na(distributions_ranges$skewness_lower_condition[i])) {
    next
  } else if (substr(distributions_ranges$skewness_lower_condition[i], 1, 1) == "<" &
      substr(distributions_ranges$skewness_lower_condition[i], 2, 2) != "=") {
    
    skewness_lower_bound[i] <- as.integer(substr(
      distributions_ranges$skewness_lower_condition[i], 2, 2)
    )
  } else if (substr(
    distributions_ranges$skewness_lower_condition[i], 2, 2) == "="){
    skewness_lower_bound[i] <- 1e-10
    
  } else if (distributions_ranges$skewness_lower_condition[i] == 0){
    next
  }
}


kurtosis_lower_bound <- rep(NA, nrow(distributions_ranges))

for (i in 1:nrow(distributions_ranges)) {
  if (is.na(distributions_ranges$kurtosis_lower_condition[i])) {
    next
  } else if (substr(distributions_ranges$kurtosis_lower_condition[i], 1, 1) == "<" &
      substr(distributions_ranges$kurtosis_lower_condition[i], 2, 2) != "=") {
    
    kurtosis_lower_bound[i] <- as.integer(substr(
      distributions_ranges$kurtosis_lower_condition[i], 2, 2)
    )
  } else if (substr(
    distributions_ranges$kurtosis_lower_condition[i], 2, 2) == "="){
    kurtosis_lower_bound[i] <- 1e-10
    
  } else if (distributions_ranges$kurtosis_lower_condition[i] == 0){
    next
  } else if (distributions_ranges$kurtosis_lower_condition[i] == "tau<nu") {
    kurtosis_lower_bound[i] <- "Skewness"
  }
}


x_lower_bound <- rep(NA, nrow(distributions_ranges))

for (i in 1:nrow(distributions_ranges)) {
  if (is.na(distributions_ranges$x_lower_condition[i])) {
    next
  } else if (substr(distributions_ranges$x_lower_condition[i], 1, 1) == "<" &
      substr(distributions_ranges$x_lower_condition[i], 2, 2) != "=") {
    
    x_lower_bound[i] <- as.integer(substr(
      distributions_ranges$x_lower_condition[i], 2, 2)
    )
  } else if (substr(
    distributions_ranges$x_lower_condition[i], 2, 2) == "="){
    x_lower_bound[i] <- 1e-10
    
  } else if (distributions_ranges$x_lower_condition[i] == 0){
    next
  } else if (distributions_ranges$x_lower_condition[i] == "mu-(sigma/nu)") {
    x_lower_bound[i] <- "Location - (Scale / Skewness)"
  }
}


# Assign upper bounds ##########################################################

location_upper_bound <- rep(NA, nrow(distributions_ranges))

for (i in 1:nrow(distributions_ranges)) {
  if (is.na(distributions_ranges$location_upper_condition[i])) {
    next
  } else if (substr(distributions_ranges$location_upper_condition[i], 1, 1) == ">" &
      substr(distributions_ranges$location_upper_condition[i], 2, 2) != "=") {
    
    location_upper_bound[i] <- as.integer(substr(
      distributions_ranges$location_upper_condition[i], 2, 2)
    )
  } else if (substr(
    distributions_ranges$location_upper_condition[i], 2, 2) == "="){
    location_upper_bound[i] <- 0.9999999 #"0.9999999999"
  } 
}


scale_upper_bound <- rep(NA, nrow(distributions_ranges))

for (i in 1:nrow(distributions_ranges)) {
  if (is.na(distributions_ranges$scale_upper_condition[i])) {
    next
  } else if (substr(distributions_ranges$scale_upper_condition[i], 1, 1) == ">" &
      substr(distributions_ranges$scale_upper_condition[i], 2, 2) != "=") {
    
    scale_upper_bound[i] <- as.integer(substr(
      distributions_ranges$scale_upper_condition[i], 2, 2)
    )
  } else if (substr(
    distributions_ranges$scale_upper_condition[i], 2, 2) == "="){
    scale_upper_bound[i] <- 0.9999999 #"0.9999999999"
  }
}


skewness_upper_bound <- rep(NA, nrow(distributions_ranges))

for (i in 1:nrow(distributions_ranges)) {
  if (is.na(distributions_ranges$skewness_upper_condition[i])) {
    next
  } else if (substr(distributions_ranges$skewness_upper_condition[i], 1, 1) == ">" &
      substr(distributions_ranges$skewness_upper_condition[i], 2, 2) != "=") {
    
    skewness_upper_bound[i] <- as.integer(substr(
      distributions_ranges$skewness_upper_condition[i], 2, 2)
    )
  } else if (substr(
    distributions_ranges$skewness_upper_condition[i], 2, 2) == "="){
    skewness_upper_bound[i] <- 0.9999999 #"0.9999999999"
  }
}


kurtosis_upper_bound <- rep(NA, nrow(distributions_ranges))

for (i in 1:nrow(distributions_ranges)) {
  if (is.na(distributions_ranges$kurtosis_upper_condition[i])) {
    next
  } else if (substr(distributions_ranges$kurtosis_upper_condition[i], 1, 1) == ">" &
      substr(distributions_ranges$kurtosis_upper_condition[i], 2, 2) != "=") {
    
    kurtosis_upper_bound[i] <- as.integer(substr(
      distributions_ranges$kurtosis_upper_condition[i], 2, 2)
    )
  } else if (substr(
    distributions_ranges$kurtosis_upper_condition[i], 2, 2) == "="){
    kurtosis_upper_bound[i] <- 0.9999999 #"0.9999999999"
  } 
}


x_upper_bound <- rep(NA, nrow(distributions_ranges))

for (i in 1:nrow(distributions_ranges)) {
  if (is.na(distributions_ranges$x_upper_condition[i])) {
    next
  } else if (substr(distributions_ranges$x_upper_condition[i], 1, 1) == ">" &
      substr(distributions_ranges$x_upper_condition[i], 2, 2) != "=") {
    
    x_upper_bound[i] <- as.integer(substr(
      distributions_ranges$x_upper_condition[i], 2, 2)
    )
  } else if (substr(
    distributions_ranges$x_upper_condition[i], 2, 2) == "="){
    x_upper_bound[i] <- 0.9999999 #"0.9999999999"
  } 
}


# Add columns to data frame
distributions_ranges$location_lower_bound <- location_lower_bound
distributions_ranges$location_upper_bound <- location_upper_bound
distributions_ranges$scale_lower_bound <- scale_lower_bound
distributions_ranges$scale_upper_bound <- scale_upper_bound
distributions_ranges$skewness_lower_bound <- skewness_lower_bound
distributions_ranges$skewness_upper_bound <- skewness_upper_bound
distributions_ranges$kurtosis_lower_bound <- kurtosis_lower_bound
distributions_ranges$kurtosis_upper_bound <- kurtosis_upper_bound
distributions_ranges$x_lower_bound <- x_lower_bound
distributions_ranges$x_upper_bound <- x_upper_bound

# Change lower bounds of location (mu), scale (sigam) and kurtosis (tau) for
# dBCPE and dBCPEo as the conditions within function body are clearly 
# conflicting with documentation.
# E.g.: "The BCPE distribution is suitable for all combinations of the 
# parameters within their ranges [i.e. mu>0, sigma>0, nu=(-Inf,+Inf) and tau>0]"
# Conditions for mu, sigma and tau within function body are 
# "stop if any [parameter] < 0" and not "stop if any [parameter] <= 0" though.
distributions_ranges[distributions_ranges$density_fun == "dBCPE",
                     "location_lower_bound"] <- 1e-10
distributions_ranges[distributions_ranges$density_fun == "dBCPE",
                     "scale_lower_bound"] <- 1e-10
distributions_ranges[distributions_ranges$density_fun == "dBCPE",
                     "kurtosis_lower_bound"] <- 1e-10
distributions_ranges[distributions_ranges$density_fun == "dBCPEo",
                     "location_lower_bound"] <- 1e-10
distributions_ranges[distributions_ranges$density_fun == "dBCPEo",
                     "scale_lower_bound"] <- 1e-10
distributions_ranges[distributions_ranges$density_fun == "dBCPEo",
                     "kurtosis_lower_bound"] <- 1e-10

# dSST we assign the same lower bound for tau as in dST3 which is called within
# the function body of dSST (i.e. lower bound for kurtosis is not =2 explicitly,
# but tau is allowed to be > 0)
distributions_ranges[distributions_ranges$density_fun == "dSST",
                     "scale_lower_bound"] <- 1e-10
distributions_ranges[distributions_ranges$density_fun == "dSST",
                     "skewness_lower_bound"] <- 1e-10
distributions_ranges[distributions_ranges$density_fun == "dSST",
                     "kurtosis_lower_bound"] <- 1e-10

# dNO lacks the restricted range for the scale parameter within function body
distributions_ranges[distributions_ranges$density_fun == "dNO",
                     "scale_lower_bound"] <- 1e-10


# Write to CSV (seperated by semicolons since "," contained in names of normal
# distributions)
write.table(distributions_ranges, file = "distributions_ranges.csv", sep = ';',
            row.names = FALSE)
