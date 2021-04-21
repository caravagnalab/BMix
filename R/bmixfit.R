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
#' @param score The score for model selection, any of `NLL`, `BIC` or `ICL`.
#' @param silent If `FALSE`, does not print outputs.
#'
#' @return An object of class \code{bmix} that represents a fit mixture of this package.
#'
#' @import pio
#' @import cli
#' @import crayon
#' @import VGAM
#' @importFrom stats4 coef
#' @importFrom graphics hist
#' @importFrom stats pnorm dbinom dnorm kmeans median pnorm runif shapiro.test
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
#' print(x)
bmixfit = function(data,
                   K.Binomials = 1:2,
                   K.BetaBinomials = 0,
                   epsilon = 1e-8,
                   samples = 2,
                   score = 'ICL',
                   silent = FALSE,
                   description = "My BMix model")
{
  cli::cli_h1("BMix fit")
  cat('\n')

  B_grid = expand.grid(
    Sample = 1:samples,
    B = K.Binomials,
    BB = 0,
    stringsAsFactors = FALSE
  )

  BB_grid = expand.grid(
    Sample = 1:samples,
    B = 0,
    BB = K.BetaBinomials,
    stringsAsFactors = FALSE
  )

  grid = dplyr::bind_rows(B_grid, BB_grid)
  grid = grid[grid$B > 0 | grid$BB > 0, , drop = FALSE]

  # grid = expand.grid(Sample = 1:samples, B = K.Binomials, BB = K.BetaBinomials,  stringsAsFactors = FALSE)
  # grid = grid[ grid$B + grid$BB > 0, , drop = FALSE]
  # grid = grid[ !(grid$B > 0 & grid$BB > 0), , drop = FALSE]

  # best.score = .Machine$integer.max

  if (!silent)
  {
    cli::cli_alert_info(
      "Binomials k_B = {.field {K.Binomials}}, Beta-Binomials k_BB = {.field {K.BetaBinomials}}; {.value {nrow(grid)}} fits to run."
    )

    #
    # pio::pioStr("Total number of runs: ", nrow(grid), suffix = '\n\n')
    #
    # cat(sprintf("%10s  | %7s |  %6s | %11s | %10s | %10s\n",
    #             "Run #", "Samp. #", "Binom.", "Beta-Binom.", "Conv.", "ICL" ))
    # cat("------------------------------------------------------------------------------")
  }

  # grid$epsilon = epsilon
  # grid$entropy = entropy

  inputs = grid[, 2:ncol(grid)]
  inputs = apply(inputs, 1, function(x)
    list(
      data = data,
      B = x['B'],
      BB = x['BB'],
      # entropy = entropy,
      epsilon = epsilon
    ))

  TIME = as.POSIXct(Sys.time(), format = "%H:%M:%S")

  results = easypar::run(FUN = runner,
                         PARAMS = inputs,
                         parallel = FALSE)

  if(length(results) == 0)
    stop("All tasks returned error - cannot analyse this with BMix.")

  # Report timing to screen
  TIME = difftime(as.POSIXct(Sys.time(), format = "%H:%M:%S"), TIME, units = "mins")

  cat('\n')
  cli::cli_alert_info(paste(
    crayon::bold("Bmix best fit"),
    'completed in',
    round(TIME, 2),
    'mins'
  ))
  cat('\n')

  best = results[[which.min(sapply(results, function(x) x[[score]]))]]
  best$description = description
  best$score = score
  best$grid.model.selection = cbind(grid,
                                    `NLL` = sapply(results, function(x) x$NLL),
                                    `BIC` = sapply(results, function(x) x$BIC),
                                    `ICL` = sapply(results, function(x) x$ICL))

  print(best)

  # for(i in 1:nrow(grid))
  # {
  #   # # Loop untill it computes without errors
  #   # success = FALSE
  #   # repeat {
  #   #   tryCatch({
  #   #
  #   #     if(!silent)
  #   #       cat(sprintf("\n%10s  | %7s |  %6s | %11s | ", paste0(i, '/', nrow(grid)), grid$Sample[i], grid$B[i], grid$BB[i]))
  #   #
  #   #     # try the EM
  #   #     fit = bmixfit_EM(data, K = c(grid$B[i], grid$BB[i]), epsilon = epsilon, use_entropy = entropy)
  #   #
  #   #     # if you get here, it will exit
  #   #     success = TRUE
  #   #     if(success) break;
  #   #
  #   #   },
  #   #   error = function(e)
  #   #   {
  #   #     # cat("\n\tIntercepted error (retrying)\n")
  #   #     # print(e)
  #   #     if(!silent) cat(crayon::red("error (forcing restart of this computation)"))
  #   #   }
  #   #   )
  #   # }
  #   #
  #   # if(!silent)
  #   # {
  #   #   if(fit$status.MLE.error){
  #   #     cat(crayon::red(sprintf("%10s", "MLE error")), '| ')
  #   #   }
  #   #   else cat(crayon::green(sprintf("%10s", "OK")), '| ')
  #   # }
  #
  #   grid$ICL[i] = fit$ICL
  #
  #   if(fit$ICL < best.score)
  #   {
  #     if(!silent)
  #       cat(crayon::green(fit$ICL), '*')
  #     best.score = fit$ICL
  #   }
  #   else cat(fit$ICL)
  #
  #   fits = append(fits, list(fit))
  # }
  #
  # best = which.min(grid$ICL)
  # best = fits[[best]]
  # best$grid.model.selection = grid
  #
  # if(!silent)
  # {
  #   cat("\n\n *** Best model: ", min(grid$ICL), " \n\n")
  #   print(best)
  # }

  return(best)
}

runner =  function(data, B, BB, epsilon) {
  # Loop until it computes without errors
  success = FALSE
  fit = NULL

  todo_repetitions = 15

  while (!success)
  {
    tryCatch({
      # Count the attempt
      todo_repetitions = todo_repetitions - 1

      if (todo_repetitions == 0)
        success = TRUE
      else
      {
        # try the EM
        fit = bmixfit_EM(
          data,
          K = c(B, BB),
          epsilon = epsilon
        )

        # if you get here, it will exit
        success = TRUE
      }
    },
    error = function(e)
    {
      cli::cli_alert_danger("Error interpected, forcing restart of this computation.")
      print(e)
    })
  }

  if (todo_repetitions == 0) {
    cli::cli_alert_danger("Too many errors for this runner - propagating an error!")

    stop(base::simpleError("Too many errors for this runner - propagating an error!"))
  }

  return(fit)
}
