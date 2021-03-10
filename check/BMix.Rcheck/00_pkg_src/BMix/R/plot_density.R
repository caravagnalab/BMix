#' Plot the density of the mixture.
#'
#' @description
#'
#' This functions plots the density of the fit, coloured
#' according to the mixture components. This function
#' must receive in input exactly the same data used for the fit.
#'
#' @param x An object of class \code{bmix} that represents a fit.
#' @param data The data used to compute the fit \code{x}.
#' @param trials To compute the density a number of trials must be
#' fixed. This parameter represents exactly that. By default the median
#' number of trials in the input \code{data} are taken.
#'
#' @return A ggplot object.
#'
#' @import ggplot2
#' @import VGAM
#' @import cowplot
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
#' plot_density(x, data)
plot_density = function(x,
                        data,
                        trials = round(median(data[, 2]))
                        )
{
  K = x$K
  pi = x$pi

  # -=-=-=-=-=-=-=-=-
  # Binomial parameters
  # -=-=-=-=-=-=-=-=-
  B.params = x$B.params

  Binomial_densities = lapply(names(B.params), function(k){

    # non-normalized
    density = get_Binomial_density(p = x$B.params[k], trials = trials)

    density$component = k
    density$y = density$y * pi[k]

    density
  })

  Binomial_densities = Reduce(rbind, Binomial_densities)

  # -=-=-=-=-=-=-=-=-
  # Beta-binomial parameters
  # -=-=-=-=-=-=-=-=-
  BB.params = x$BB.params

  BetaBinomial_densities = lapply(colnames(BB.params), function(k){

    # non-normalized
    density = get_BetaBinomial_density(
      p = x$BB.params['mu', k],
      rho = x$BB.params['rho', k],
      trials = trials)

    density$component = k
    density$y = density$y * pi[k]

    density
  })

  BetaBinomial_densities = Reduce(rbind, BetaBinomial_densities)

  # -=-=-=-=-=-=-=-=-
  # Aggregated density
  # -=-=-=-=-=-=-=-=-
  densities = rbind(Binomial_densities, BetaBinomial_densities)

  ggplot(densities, aes(x = x, y = y, color = component)) +
    geom_line() +
    scale_color_brewer(palette = 'Set1') +
    labs(
      x = 'Successes',
      y = 'Density',
      title = 'BMix density',
      subtitle = paste0("Trials: ", trials)
    ) +
    geom_vline(
      xintercept = x$B.params * trials,
      linetype = 'dashed'
    ) +
    geom_vline(
      xintercept = x$BB.params['mu', ] * trials,
      linetype = 'dashed'
    ) +
    my_ggplot_theme() +
    guides(color = guide_legend('Cluster'))
}

# Non-scaled density for Binomial components
get_Binomial_density = function(trials, p)
{
  domain = seq(0, trials, 1)
  data.frame(
    x = domain,
    y = dbinom(domain, size = trials, prob = p)
  )
}

# Non-scaled density for Beta-Binomial components
get_BetaBinomial_density = function(trials, p, rho)
{
  domain = seq(0, trials, 1)

  # Beta-Bin
  y = VGAM::dbetabinom(domain,
                   size = trials,
                   prob = p,
                   rho = rho)

  data.frame(
    x = domain,
    y = y
  )
}
