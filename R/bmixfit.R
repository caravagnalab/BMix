
#' Title
#'
#' @param data
#' @param K.Binomials
#' @param K.BetaBinomials
#' @param epsilon
#' @param samples
#'
#' @return
#' @export
#'
#' @examples
bmixfit = function(
  data,
  K.Binomials = 0:2,
  K.BetaBinomials = 0:2,
  epsilon = 1e-8,
  samples = 1,
  entropy = TRUE
)
{
  grid = expand.grid(Sample = 1:samples, B = K.Binomials, BB = K.BetaBinomials,  stringsAsFactors = FALSE)
  grid = grid[ grid$B + grid$BB > 0, , drop = FALSE]
  grid$ICL = NA

  best.score = .Machine$integer.max

  cat("\n\tTotal number of runs: ", nrow(grid), '\n\n')

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
        fit = bmixfit_EM(data, K = c(grid$B[i], grid$BB[i]), epsilon = epsilon, entropy = entropy)

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
