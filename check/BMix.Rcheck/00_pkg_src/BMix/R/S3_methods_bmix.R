#' Print a \code{bmix} object.
#'
#' @description Print to screen some information about the mixture.
#'
#' @param x A \code{bmix} object.
#' @param ... S3 parameters.
#'
#' @return Nothing, just print to screen some information about the mixture.
#'
#' @exportS3Method print bmix
#' @export print.bmix
#'
#' @examples
#' # The same dataset used in the package vignette
#' data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)
#'
#' # BMix fit with default parameters
#' x = bmixfit(data)
#'
#' print(x)
print.bmix = function(x, ...)
{
  # cat("BMix model with K =", sum(x$K), "components:", x$K[1], "Binomials and", x$K[2], "Beta-Binomials.\n")
  #
  # cat("\nBinomials\n")
  # print(x$B.params)
  #
  # cat("\nBeta-Binomials\n")
  # print(x$BB.params)
  #
  # cat("\nICL:", x$ICL, '\n')

  cli::cli_rule(
    paste(
      crayon::bgYellow(crayon::black("[ BMix ] {.value {x$description}}")),
      'n = {.value {dim(x$z_nk)[1]}} with {.field k = {dim(x$z_nk)[2]}} component(s) ({.field {x$K[1]}} + {.field {x$K[2]}})'
    )
  )

  # Pi proportions
  sor_p = sort(x$pi, decreasing = TRUE)
  sor_p = names(sor_p)

  pi = round(x$pi[sor_p], digits = 2) * 100
  pi = pi[pi > 0]
  pi_label = paste0(pi, '% [', crayon::yellow(names(pi)), ']')
  cli::cli_li(
    ' Clusters: \u03C0 = {.value {pi_label}}, with \u03C0 > 0.'
  )

  for (i in names(x$B.params))
    cli::cli_li(
      paste0(
        "Binomial {.field {i}} with mean = {.value {x$B.params[i]}}."
      )
    )

  for (i in names(x$BB.params))
    cli::cli_li(
      paste0(
        "Beta-Binomial {.field {i}} with mean = {.value {x$BB.params[i]}}."
      )
    )
  cli::cli_end()

  # cat('\n')
  xs = round(x$ICL, 2)

  if(x$use_entropy)
    cli::cli_alert_info('Score(s): ICL = {.value {xs}}.')
  else
    cli::cli_alert_info('Score(s): BIC = {.value {xs}}.')
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
#' @param ... The ellipsis should contain a \code{data} parameter storing the
#' actual data used to compute the fit \code{x}.
#'
#' @return A \code{cowplot} figure.
#'
#' @exportS3Method plot bmix
#' @export plot.bmix
#'
#' @examples
#'
#' # The same dataset used in the package vignette
#' data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)
#'
#' # BMix fit with default parameters
#' x = bmixfit(data)
#'
#' # Empty plot
#' plot(x)
#'
#' # Plot with data
#' plot(x, data)
plot.bmix = function(x, ...)
{
  params = list(...)

  if(length(params) >= 1)
  {
    data = params[[1]]

    f = cowplot::plot_grid(
      plot_clusters(x, data),
      plot_density(x, data),
      plot_model_selection(x),
      nrow = 1,
      ncol = 3,
      align = 'h'
    )

    return(f)
  }

  warning("No data given in input, returning empty plot.")

  return(ggplot())
}









