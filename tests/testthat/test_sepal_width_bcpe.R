testthat::context(desc = "Sepal.Width Box Cox Power Exponential")

# replicating the ml estimation
mle <- function(data, input) {
  distributions <- read.csv(file = "test_files/distributions_ranges.csv",
                            header = TRUE, sep = ';', stringsAsFactors = FALSE)
  distributions <- distributions[, - c(12:22)]
  default.parameter <- as.numeric(shinydistributions:::distributions[
    shinydistributions:::distributions$dist_density == input,
    c("default_location", "default_scale", "default_skewness",
      "default_kurtosis")])
  default.parameter[is.na(default.parameter)] <- 0
  binomial_denominator <- shinydistributions:::distributions[
    shinydistributions:::distributions$dist_density == input,
    "default_binomial_denominator"]
  log.likelihood <- function(parameter, data) {
    -sum(R.utils::doCall(
      gamlss.dist::dBCPE,
      x = data,
      log = TRUE,
      bd = ifelse(test = shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input,
        "x_upper_bound"] <= binomial_denominator,
        yes = binomial_denominator,
        no = shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == input,
          "x_upper_bound"]),
      mu = parameter[1],
      sigma = parameter[2],
      nu = parameter[3],
      tau = parameter[4])
    )
  }
  mle <- optim(
    par = default.parameter,
    fn = log.likelihood,
    data = data,
    lower = c(
      shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input,
        "location_lower_bound"],
      shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input,
        "scale_lower_bound"],
      shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input,
        "skewness_lower_bound"],
      if (input == "Normal Exponential t - dNET") {
        input$skewness
      } else {
        shinydistributions:::distributions[
          shinydistributions:::distributions$dist_density == input,
          "kurtosis_lower_bound"]
      }
    ),
    upper = c(
      shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input,
        "location_upper_bound"],
      shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input,
        "scale_upper_bound"],
      shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input,
        "skewness_upper_bound"],
      shinydistributions:::distributions[
        shinydistributions:::distributions$dist_density == input,
        "kurtosis_upper_bound"]
    ),
    method = "L-BFGS-B"
  )
  mle <- mle$par
  return(round(mle, digits = 2))
}

testthat::test_that("Sepal.Width Box Cox Power Exponential MLE", {
  mle <- mle(data = iris$Sepal.Width,
             input = "Box-Cox Power Exponential - dBCPE")
  testthat::expect_equal(mle, c(3.03, 0.14, 0.23, 1.58))
})
