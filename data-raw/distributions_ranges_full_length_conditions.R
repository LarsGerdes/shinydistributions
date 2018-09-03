distributions <- read.table(
  file = "distributions_default_params_109_bd_discrete_flag.csv",
  header = TRUE,
  sep = ';',
  stringsAsFactors = FALSE)


################################################################################
location_lower_condition <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  
  # Store function body as character vector 
  body_d_fun <- capture.output(print(body(distributions$density_fun[i])))
  
  # Check for condition specifying range within function body (if statement)
  range_identifier <- "(any(mu" # "if (any(mu"
  range_check <- grepl(range_identifier, body_d_fun, fixed = TRUE)
  
  if (! any(range_check)) {
    next
  } else {
    # Select string containing condition and split it by single spaces in between 
    # characters (default: splits into single characters)
    range_split <- strsplit(body_d_fun[range_check == TRUE], "\\s{1}") 
    
    # Start and stop positions of condition (relevant part of given if statement)
    pos_start <- which(range_split[[1]] == range_identifier) + 1
    #pos_stop <- pos_start + 1
    
    # Collapse condition to a single string
    range_string <- paste(range_split[[1]][pos_start:length(range_split[[1]])], 
                          collapse = '')
    
    # Remove bracket(s) in the end of string (remaining part of if statement)
    location_lower_condition[i] <- gsub(")", "",  range_string)
  }
}
location_lower_condition

# Add column to data frame
distributions$location_lower_condition <- location_lower_condition


################################################################################
location_upper_condition <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  
  # Store function body as character vector 
  body_d_fun <- capture.output(print(body(distributions$density_fun[i])))
  
  # Check for condition specifying range within function body (if statement)
  range_identifier <- "| any(mu" # | any(mu > 1))
  range_check <- grepl(range_identifier, body_d_fun, fixed = TRUE)
  
  if (! any(range_check)) {
    next
  } else {
    # Select string containing condition and split it by single spaces in between 
    # characters (default: splits into single characters)
    range_split <- strsplit(body_d_fun[range_check == TRUE], "\\s{1}") 
    
    # Start and stop positions of condition (relevant part of given if statement)
    pos_start <- which(range_split[[1]] == "any(mu") + 1
    #pos_stop <- pos_start + 1
    
    # Collapse condition to a single string
    range_string <- paste(range_split[[1]][pos_start:length(range_split[[1]])],
                          collapse = '')
    
    # Remove bracket(s) in the end of string (remaining part of if statement)
    location_upper_condition[i] <- gsub(")", "",  range_string)
  }
}
location_upper_condition

# Add column to data frame
distributions$location_upper_condition <- location_upper_condition


################################################################################
scale_lower_condition <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  
  # Store function body as character vector 
  body_d_fun <- capture.output(print(body(distributions$density_fun[i])))
  
  # Check for condition specifying range within function body (if statement)
  range_identifier <- "(any(sigma" # "if (any(sigma"
  range_check <- grepl(range_identifier, body_d_fun, fixed = TRUE)
  
  if (! any(range_check)) {
    next
  } else {
    # Select string containing condition and split it by single spaces in between 
    # characters (default: splits into single characters)
    range_split <- strsplit(body_d_fun[range_check == TRUE], "\\s{1}") 
    
    # Start and stop positions of condition (relevant part of given if statement)
    pos_start <- which(range_split[[1]] == range_identifier) + 1
    #pos_stop <- pos_start + 1
    
    # Collapse condition to a single string
    range_string <- paste(range_split[[1]][pos_start:length(range_split[[1]])], collapse = '')
    
    # Remove bracket(s) in the end of string (remaining part of if statement)
    scale_lower_condition[i] <- gsub(")", "",  range_string)
  }
}
scale_lower_condition

# Add column to data frame
distributions$scale_lower_condition <- scale_lower_condition


################################################################################
scale_upper_condition <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  
  # Store function body as character vector 
  body_d_fun <- capture.output(print(body(distributions$density_fun[i])))
  
  # Check for condition specifying range within function body (if statement)
  range_identifier <- "| any(sigma" # | any(sigma > 1))
  range_check <- grepl(range_identifier, body_d_fun, fixed = TRUE)
  
  if (! any(range_check)) {
    next
  } else {
    # Select string containing condition and split it by single spaces in between 
    # characters (default: splits into single characters)
    range_split <- strsplit(body_d_fun[range_check == TRUE], "\\s{1}") 
    
    # Start and stop positions of condition (relevant part of given if statement)
    pos_start <- which(range_split[[1]] == "any(sigma") + 1
    #pos_stop <- pos_start + 1
    
    # Collapse condition to a single string
    range_string <- paste(range_split[[1]][pos_start:length(range_split[[1]])],
                          collapse = '')
    
    # Remove bracket(s) in the end of string (remaining part of if statement)
    scale_upper_condition[i] <- gsub(")", "",  range_string)
  }
}
scale_upper_condition

# Add column to data frame
distributions$scale_upper_condition <- scale_upper_condition


################################################################################
skewness_lower_condition <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  
  # Store function body as character vector 
  body_d_fun <- capture.output(print(body(distributions$density_fun[i])))
  
  # Check for condition specifying range within function body (if statement)
  range_identifier <- "(any(nu" # "if (any(nu"
  range_check <- grepl(range_identifier, body_d_fun, fixed = TRUE)
  
  if (! any(range_check)) {
    next
  } else {
    # Select string containing condition and split it by single spaces in between 
    # characters (default: splits into single characters)
    range_split <- strsplit(body_d_fun[range_check == TRUE], "\\s{1}") 
    
    # Start and stop positions of condition (relevant part of given if statement)
    pos_start <- which(range_split[[1]] == range_identifier) + 1
    #pos_stop <- pos_start + 1
    
    # Collapse condition to a single string
    range_string <- paste(range_split[[1]][pos_start:length(range_split[[1]])],
                          collapse = '')
    
    # Remove bracket(s) in the end of string (remaining part of if statement)
    skewness_lower_condition[i] <- gsub(")", "",  range_string)
  }
}
skewness_lower_condition

# Add column to data frame
distributions$skewness_lower_condition <- skewness_lower_condition

################################################################################
skewness_upper_condition <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  
  # Store function body as character vector 
  body_d_fun <- capture.output(print(body(distributions$density_fun[i])))
  
  # Check for condition specifying range within function body (if statement)
  range_identifier <- "| any(nu" # | any(nu > 1))
  range_check <- grepl(range_identifier, body_d_fun, fixed = TRUE)
  
  if (! any(range_check)) {
    next
  } else {
    # Select string containing condition and split it by single spaces in between 
    # characters (default: splits into single characters)
    range_split <- strsplit(body_d_fun[range_check == TRUE], "\\s{1}") 
    
    # Start and stop positions of condition (relevant part of given if statement)
    pos_start <- which(range_split[[1]] == "any(nu") + 1
    #pos_stop <- pos_start + 1
    
    # Collapse condition to a single string
    range_string <- paste(range_split[[1]][pos_start:length(range_split[[1]])],
                          collapse = '')
    
    # Remove bracket(s) in the end of string (remaining part of if statement)
    skewness_upper_condition[i] <- gsub(")", "",  range_string)
  }
}
skewness_upper_condition

# Add column to data frame
distributions$skewness_upper_condition <- skewness_upper_condition


################################################################################
kurtosis_lower_condition <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  
  # Store function body as character vector 
  body_d_fun <- capture.output(print(body(distributions$density_fun[i])))
  
  # Check for condition specifying range within function body (if statement)
  range_identifier <- "(any(tau" # "if (any(tau"
  range_check <- grepl(range_identifier, body_d_fun, fixed = TRUE)
  
  if (! any(range_check)) {
    next
  } else {
    # Select string containing condition and split it by single spaces in between 
    # characters (default: splits into single characters)
    range_split <- strsplit(body_d_fun[range_check == TRUE], "\\s{1}") 
    
    # Start and stop positions of condition (relevant part of given if statement)
    pos_start <- which(range_split[[1]] == range_identifier) + 1
    #pos_stop <- pos_start + 1
    
    # Collapse condition to a single string
    range_string <- paste(range_split[[1]][pos_start:length(range_split[[1]])],
                          collapse = '')
    
    # Remove bracket(s) in the end of string (remaining part of if statement)
    kurtosis_lower_condition[i] <- gsub(")", "",  range_string)
  }
}
kurtosis_lower_condition

# Add column to data frame
distributions$kurtosis_lower_condition <- kurtosis_lower_condition


################################################################################
kurtosis_upper_condition <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  
  # Store function body as character vector 
  body_d_fun <- capture.output(print(body(distributions$density_fun[i])))
  
  # Check for condition specifying range within function body (if statement)
  range_identifier <- "| any(tau" # | any(tau > 1))
  range_check <- grepl(range_identifier, body_d_fun, fixed = TRUE)
  
  if (! any(range_check)) {
    next
  } else {
    # Select string containing condition and split it by single spaces in between 
    # characters (default: splits into single characters)
    range_split <- strsplit(body_d_fun[range_check == TRUE], "\\s{1}") 
    
    # Start and stop positions of condition (relevant part of given if statement)
    pos_start <- which(range_split[[1]] == "any(tau") + 1
    #pos_stop <- pos_start + 1
    
    # Collapse condition to a single string
    range_string <- paste(range_split[[1]][pos_start:length(range_split[[1]])],
                          collapse = '')
    
    # Remove bracket(s) in the end of string (remaining part of if statement)
    kurtosis_upper_condition[i] <- gsub(")", "",  range_string)
  }
}
kurtosis_upper_condition

# Add column to data frame
distributions$kurtosis_upper_condition <- kurtosis_upper_condition


################################################################################
x_lower_condition <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  
  # Store function body as character vector 
  body_d_fun <- capture.output(print(body(distributions$density_fun[i])))
  
  # Check for condition specifying range within function body (if statement)
  range_identifier <- "(any(x" # "if (any(x"
  range_check <- grepl(range_identifier, body_d_fun, fixed = TRUE)
  
  if (! any(range_check)) {
    next
  } else {
    # Select string containing condition and split it by single spaces in between 
    # characters (default: splits into single characters)
    range_split <- strsplit(body_d_fun[range_check == TRUE], "\\s{1}") 
    
    # Start and stop positions of condition (relevant part of given if statement)
    pos_start <- which(range_split[[1]] == range_identifier) + 1
    #pos_stop <- pos_start + 1
    
    # Collapse condition to a single string
    range_string <- paste(range_split[[1]][pos_start:length(range_split[[1]])],
                          collapse = '')
    
    # Remove bracket(s) in the end of string (remaining part of if statement)
    x_lower_condition[i] <- gsub(")", "",  range_string)
  }
}
x_lower_condition

# Add column to data frame
distributions$x_lower_condition <- x_lower_condition


################################################################################
x_upper_condition <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  
  # Store function body as character vector 
  body_d_fun <- capture.output(print(body(distributions$density_fun[i])))
  
  # Check for condition specifying range within function body (if statement)
  range_identifier <- "| any(x" #
  range_check <- grepl(range_identifier, body_d_fun, fixed = TRUE)
  
  if (! any(range_check)) {
    next
  } else {
    # Select string containing condition and split it by single spaces in between 
    # characters (default: splits into single characters)
    range_split <- strsplit(body_d_fun[range_check == TRUE], "\\s{1}") 
    
    # Start and stop positions of condition (relevant part of given if statement)
    pos_start <- which(range_split[[1]] == "any(x") + 1
    #pos_stop <- pos_start + 1
    
    # Collapse condition to a single string
    range_string <- paste(range_split[[1]][pos_start:length(range_split[[1]])],
                          collapse = '')
    
    # Remove bracket(s) in the end of string (remaining part of if statement)
    x_upper_condition[i] <- gsub(")", "",  range_string)
  }
}
x_upper_condition

# Add column to data frame
distributions$x_upper_condition <- x_upper_condition


################################################################################
# function call within function body (call of another density function)
fun_call <- rep(NA, nrow(distributions))

for (i in 1:nrow(distributions)) {
  for (j in 1:nrow(distributions)) {
    
    body_d_fun <- capture.output(print(body(distributions$density_fun[i])))
    d_fun <- distributions$density_fun[j]
    # check function body for another density function
    fun_check <- grepl(d_fun, body_d_fun, fixed = TRUE)
    
    if (any(fun_check)) {
      fun_call[i] <- d_fun
    }
  }
}
fun_call
length(na.omit(fun_call)) # 32

# Add column to data frame
distributions$fun_call <- fun_call

################################################################################
# Write to CSV (seperated by semicolons since "," contained in names of normal
# distributions)
write.table(distributions, 
            file = "distributions_ranges_full_length_conditions.csv", sep = ';',
            row.names = FALSE)


################################################################################
################################################################################
body(dSST) # calls function dST3 and uses its ranges

body(dMN4) # upper range should be 4 instead of 5.
# if (any(x < 1) | any(x > 5)) 
# stop(paste("x must be 1, 2, 3 or 4", "\n", 

################################################################################
# discrete cases in app that are not in the documentations table
discrete_gamlss_dist <- read.csv("discrete_gamlss_distributions_names.csv", 
                                 header = FALSE, sep = ';', 
                                 stringsAsFactors = FALSE)
discrete_gamlss_dist <- data.frame("density_fun" = discrete_gamlss_dist[, 2], 
                                   "dist" = discrete_gamlss_dist[, 1],
                                   stringsAsFactors = FALSE)
# left join distributions data frame by field "density_fun"
discrete_doc_app <- dplyr::left_join(x= distributions[
  distributions$dist_type == "discrete", c(1:5,11) ], y = discrete_gamlss_dist)
discrete_doc_app

# Continuous distributions not appearing in documentation
con_gamlss_dist <- read.csv("continuous_gamlss_density_fun_from_documentation.csv", 
                            sep = ';', 
                            stringsAsFactors = FALSE)
# left join distributions data frame by field "density_fun"
con_doc_app <- dplyr::left_join(x= distributions[
  distributions$dist_type == "continuous", c(1:5,11) ], y = con_gamlss_dist)
con_doc_app[is.na(con_doc_app$dist),]

