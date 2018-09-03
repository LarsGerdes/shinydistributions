library(sqldf)
library(dplyr)
library(shiny)
library(gamlss.dist)


funs <- ls(envir = as.environment("package:gamlss.dist"))
d_funs <- funs[grepl(pattern = "d", funs)]

################################################################################

d_funs <- data.frame(d_funs, stringsAsFactors = FALSE)

d_names <- read.csv("NamesOfDistributions.csv", header = TRUE, sep = ";", 
                    stringsAsFactors = FALSE)
d_names

distributions_df <- sqldf(
                   "select f.d_funs, 
                           n.Distributions, 
                           n.Distributions || ' - ' || f.d_funs as dist_density
                    from d_funs f
                    left join d_names n on n.density_names = f.d_funs")

# replace NA values by zeros for case statement in sqldf below (NA doesn't work)
distributions_df[is.na(distributions_df)] <- 0


distributions_df <- sqldf("select fn.d_funs, 
                                   case when fn.dist_density = '0' then 
                                     fn.d_funs 
                                   else 
                                     fn.dist_density
                                   end as dist_density,
                                   n.no_of_parameters
                            from distributions_df fn
                            left join d_names n on n.density_names = fn.d_funs")

distributions_df[distributions_df$dist_density == "dNO2", 
                 "dist_density"] <- "Normal (mean, var) - dNO2"
#distributions_df[distributions_df$dist_density == "dNO2", 
#"no_of_parameters"] <- 2
distributions_df[53, 3] <- 2
distributions_df[distributions_df$dist_density == "Normal - dNO", 
                 "dist_density"]  <- "Normal (mean, sd) - dNO"

# Write to csv
#write.csv(distributions_df, file = "distributions_df.csv", row.names=FALSE, 
#          col.names = TRUE)
#str(distributions_df)

# read csv containing full distribution names
# distributions_df <- read.csv("distributions_df.csv", header = TRUE, sep = ",", 
#                              stringsAsFactors = FALSE)