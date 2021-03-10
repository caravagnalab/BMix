#' Plot the clustering of the data.
#'
#' @description
#'
#' This functions plots the data as histogram, coloured
#' according to the hard clustering assignments. This function
#' must receive in input exactly the same data used for the fit.
#'
#' @param x An object of class \code{bmix} that represents a fit.
#' @param data The data used to compute the fit \code{x}.
#'
#' @return A ggplot object.
#'
#' @import ggplot2
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
#' plot_clusters(x, data)
plot_clusters = function(x, data)
{
  K = x$K

  # Main histogram
  df = data.frame(Frequency = data[, 1]/data[, 2],
                  Cluster = x$labels)

  ggplot(df, aes(Frequency, fill = Cluster, y = ..count..)) +
    geom_histogram(position = 'identity', binwidth = 0.01) +
    labs(
      title = bquote("BMix fit"),
      subtitle = bquote(K[B]~"="~.(x$K[1])~","~K[BB]~"="~.(x$K[2])),
      x = "Success probability",
      y = "Observations") +
    xlim(0, 1) +
    scale_fill_brewer(palette = 'Set1')+
    my_ggplot_theme() +
    guides(fill = guide_legend(title = "Cluster"))
}
