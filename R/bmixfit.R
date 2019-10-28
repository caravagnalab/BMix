
#' Fit a \code{Bmix} mixture
#'
#' @description
#'
#' Fits a mixture of `k` components, where each component can be
#' either a Binomial or a Beta-Binomial random variable. This function
#' take as input two paramters that determine the possible numer of compoennets,
#' for both distributions, and creates all possible input combinations to fit the model.
#'
#' The best mode is scored using the Integrated Classification Likelihood, an extension
#' of the Bayesian Information Criterion. Through one parameters it is possible to
#' switch to the Bayesian Information Criterion.
#'
#' Multiple fits can be computed, and a parameter controls when the Expectation
#' Maximization algorithm should stop.
#'
#' @param data A matrix or dataframe with two columns, the first one must represent
#' the number of successes in the Binomial trials, the second the total number of
#' trials.
#' @param K.Binomials A vector of values that represents how many Binomial components
#' should be fit to the data.By default this parameter is set to `c(0:2)`.
#' @param K.BetaBinomials  A vector of values that represents how many Beta-Binomial components
#' should be fit to the data. By default this parameter is set to `0`.
#' @param epsilon The parameter that controls when the Expectation
#' Maximization algorithm should stop. This is compared to the variation in the
#' negative loglikelihood.
#' @param samples Number of Expectation Maximization fits that should be computed per
#' configuration of mixture.
#' @param entropy If `FALSE`, then the entropy term from the Integrated Classification
#' Likelihood is not included, and the model is then scored by the Bayesian Information
#' Criterion.
#'
#' @return An object of class \code{bmix} that represents a fit mixture of this package.
#'
#' @import pio
#' @export
#'
#' @examples
#' # The same dataset used in the package vignette
#' data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)
#'
#' # BMix fit with default parameters
#' x = bmixfit(data)
#'
#' print(x)
bmixfit = function(
  data,
  K.Binomials = 1:2,
  K.BetaBinomials = 0,
  epsilon = 1e-8,
  samples = 2,
  entropy = TRUE
)
{
  grid = expand.grid(Sample = 1:samples, B = K.Binomials, BB = K.BetaBinomials,  stringsAsFactors = FALSE)
  grid = grid[ grid$B + grid$BB > 0, , drop = FALSE]
  grid$ICL = NA

  best.score = .Machine$integer.max

  pio::pioHdr('BMix ~ Fitting data')

  pio::pioStr("Total number of runs: ", nrow(grid), suffix = '\n\n')

  cat(sprintf("%10s  | %7s |  %6s | %11s | %10s | %10s\n",
              "Run #", "Samp. #", "Binom.", "Beta-Binom.", "Conv.", "ICL" ))
  cat("------------------------------------------------------------------------------")

  fits = NULL
  for(i in 1:nrow(grid))
  {
    # Loop untill it computes without errors
    success = FALSE
    repeat {
      tryCatch({
        cat(sprintf("\n%10s  | %7s |  %6s | %11s | ", paste0(i, '/', nrow(grid)), grid$Sample[i], grid$B[i], grid$BB[i]))

        # cat('\n',
        #     paste0('#',i,'.'),
        #     "K =", grid$B[i], "Binomials + K =", grid$BB[i], "Beta-Binomials: ")

        # try the EM
        fit = bmixfit_EM(data, K = c(grid$B[i], grid$BB[i]), epsilon = epsilon, use_entropy = entropy)

        # if you get here, it will exit
        success = TRUE
        if(success) break;

      },
      error = function(e)
      {
        # cat("\n\tIntercepted error (retrying)\n")
        # print(e)
        cat(crayon::red("error (forcing restart of this computation)"))
      }
      )
    }

    if(fit$status.MLE.error){
      cat(crayon::red(sprintf("%10s", "MLE error")), '| ')
    }
    else cat(crayon::green(sprintf("%10s", "OK")), '| ')

    grid$ICL[i] = fit$ICL

    if(fit$ICL < best.score)
    {
      cat(crayon::green(fit$ICL), '*')
      best.score = fit$ICL
    }
    else cat(fit$ICL)

    fits = append(fits, list(fit))
  }

  best = which.min(grid$ICL)
  best = fits[[best]]
  best$grid.model.selection = grid

  cat("\n\n *** Best with ICL: ", min(grid$ICL), " \n\n")
  print(best)
  best
}
