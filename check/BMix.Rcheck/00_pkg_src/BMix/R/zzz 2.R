#' @title \code{BMix}
#'
#' @description
#' @docType package
#' @name BMix
#'
#' @return BMix main package documentation.
#'
#' @author G.Caravagna \email{gcaravagn@@gmail.com}
#'


.onLoad <- function(libname, pkgname)
{
  # =-=-=-=-=-=-
  # Required packages will be listed here
  # =-=-=-=-=-=-
  # requirements = c('tidyverse', 'pio', 'crayon', 'ggpubr', 'peakPick', 'RColorBrewer')
  #
  # suppressMessages(sapply(requirements, require, character.only = TRUE))

  # =-=-=-=-=-=-
  # Package options
  # =-=-=-=-=-=-
  options(pio.string_fg_colour = crayon::bgYellow$black)

  # =-=-=-=-=-=-
  # Header
  # =-=-=-=-=-=-

  BMix_welcome_message =  getOption('BMix_welcome_message', default = TRUE)

  if(BMix_welcome_message)
  {
    # pio::pioHdr('BMix - Binomial and Beta-Binomial mixture models')
    # pio::pioStr("Author : ", "Giulio Caravagna <gcaravagn@gmail.com>", suffix = '\n')
    # pio::pioStr("GitHub : ", "caravagn/BMix", suffix = '\n')
    # pio::pioStr("   WWW : ", "https://caravagn.github.io/BMix/", suffix = '\n')
    #
    #
    # cat(
    #   "\n > BMix is part of the", crayon::green("\"evoverse\""),
    #   crayon::blue("[https://bit.ly/2orn94e]"),
    #   "- a collection of packages to implement Cancer Evolution analyses from cancer sequencing data.\n"
    # )

    pk = 'BMix'
    pk_l = 'Binomial and Beta-Binomial univariate mixtures'
    www = "https://caravagn.github.io/BMix/"
    em = "gcaravagn@gmail.com"

    cli::cli_alert_success(
      'Loading {.field {pk}}, {.emph \'{pk_l}\'}. Support : {.url { www}}' )


    options(BMix_welcome_message = FALSE)
  }

  invisible()
}
