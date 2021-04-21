#' Extract clustering assignments.
#'
#' @description This function extracts the clustering assignments
#' and the latent variables from the fit. This function
#' must receive in input exactly the same data used for the fit.
#'
#' @param x An object of class \code{bmix} that represents a fit.
#' @param data The data used to compute the fit \code{x}.
#'
#' @return The input \code{data} transformed into a tibble, augmented with a column for the cluster labels and the latent variables.
#'
#' @import tibble
#' @import dplyr
#'
#' @export
#'
#' @examples
#' # The same dataset used in the package vignette
#' data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)
#'
#' # BMix fit with default parameters
#' x = bmixfit(data)
#'
#' Clusters(x, data)
Clusters = function(x, data)
{
  data$cluster = x$labels
  cbind(data, x$z_nk) %>% as_tibble
}

#' Extract the fit parameters of the mixture.
#'
#' @description This function extracts the mixture
#' parameters as a tibble. The tibble contains
#' one row per component, a column for the mean of
#' the mixture component and a column for its overdispersion.
#' All the Binomial components have a fixed value of 0 for overdispersion..
#'
#' @param x An object of class \code{bmix} that represents a fit.
#'
#' @import dplyr
#' @import tibble
#'
#' @return A tibble for the the fit parameters of the mixture.
#' @export
#'
#' @examples
#' # The same dataset used in the package vignette
#' data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)
#'
#' # BMix fit with default parameters
#' x = bmixfit(data)
#'
#' Parameters(x)
Parameters = function(x)
{
  Bin = BBin = NULL

  if(x$K[1] > 0)
    Bin = tibble::enframe(x$B.params) %>%
      rename(cluster = name, mean = value) %>%
      mutate(overdispersion = 0)

  if(x$K[2] > 0)
  {
    BBin = x$BB.params %>% t %>% as_tibble %>%
      rename(mean = mu, overdispersion = rho)
    BBin$cluster = colnames(x$BB.params)
  }

  bind_rows(Bin, BBin) %>% select(cluster, mean, overdispersion)
}
