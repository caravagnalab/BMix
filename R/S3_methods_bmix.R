#' Print a \code{bmix} object.
#'
#' @description Print to screen some information about the mixture.
#'
#' @param x A \code{bmix} object.
#' @param ... S3 parameters.
#'
#' @return Nothing, just print to screen some information about the mixture.
#' @export
#'
#' @examples
print.bmix = function(x, ...)
{
  cat("BMix model with K =", sum(x$K), "components:", x$K[1], "Binomials and", x$K[2], "Beta-Binomials.\n")

  cat("\nBinomials\n")
  print(x$B.params)

  cat("\nBeta-Binomials\n")
  print(x$BB.params)

  cat("\nICL:", x$ICL, '\n')
}


#' Plot a \code{bmix} object.
#'
#' @description
#'
#' Assemble a multi-panel plot using the plotting functions
#' of the \code{Bmix} package, with default parameters.
#'
#'
#' @param x An object of class \code{bmix} that represents a fit.
#' @param data The data used to compute the fit \code{x}.
#'
#' @return A \code{cowplot} figure.
#' @export
#'
#' @import cowplot
#'
#' @examples
plot.bmix = function(x, data)
{
  cowplot::plot_grid(
    plot_clusters(x, data),
    plot_density(x, data),
    plot_model_selection(x),
    nrow = 1,
    ncol = 3,
    align = 'h'
  )
}









