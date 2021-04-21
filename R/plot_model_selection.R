#' Plot the grid of model selection for the mixture.
#'
#' @description
#'
#' This functions plots the score of each combination of parameters
#' for this mixture.
#'
#' @param x An object of class \code{bmix} that represents a fit.
#'
#' @return A ggplot object.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @export
#'
#' @examples
#' # Simple dataset
#' data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)
#'
#' # BMix fit
#' x = bmixfit(data, K.Binomials = 1:3, K.BetaBinomials = 1:3)
#'
#' plot_model_selection(x)
plot_model_selection = function(x)
{
  bof = function(x, score){
    x$grid.model.selection %>%
      as_tibble() %>%
      dplyr::select(Sample, B, BB, score) %>%
      group_by(B, BB) %>%
      summarise(best = min(!! sym(score))) %>%
      ungroup() %>%
      mutate(score = !!score)
  }

  ms_map = bind_rows(
      bof(x, "NLL"),
      bof(x, "ICL"),
      bof(x, "BIC")
      )

  ms_map = ms_map %>%
    filter(B > 0) %>%
    mutate(K = B, model = 'Binomial') %>%
    bind_rows(
      ms_map %>%
        filter(BB > 0) %>%
        mutate(K = BB, model = 'Beta-Binomial')
    )

  blable = ifelse(x$K[1] == 0,
                  paste0(x$K[2], " Beta-Binomial(s)"),
                  paste0(x$K[1], " Binomial(s)")
                  )

  ms_map = ms_map %>%
    group_by(model, K) %>%
    mutate(
      fit = ifelse(x$K[1] == B & x$K[2] == BB & score == !!score, "Best", NA)
    )

  ggplot(ms_map) +
    geom_bar(
      aes(x = as.character(K), y = best, fill = as.character(score), color = fit),
      stat = 'identity',
      position = 'dodge',
      size = 1
    ) +
    facet_grid(score~model) +
    guides(fill = guide_legend('Score'))+
    labs(
      x = 'Clusters',
      y = "Score",
      title = "BMix model selection",
      subtitle =
        paste0(
          "Best with ", x$score, ": ", blable)
        ) +
    BMix:::my_ggplot_theme() +
    scale_color_manual(
      values = c(`Best` = 'black', `NA` = 'white'),
    ) +
    guides(color = FALSE)


  # ggplot(ms_map,
  #        aes(x = B, y = BB, fill = best)) +
  #   geom_tile(aes(width = .9, height = .9)) +
  #   facet_wrap(~score, ncol = 1) +
  #   scale_fill_distiller(palette = 'Spectral', direction = -1) +
  #   scale_x_discrete(limits = unique(ms_map$B) %>% sort) +
  #   scale_y_discrete(limits = unique(ms_map$BB) %>% sort) +
  #   labs(
  #     x = 'Binomials',
  #     y = "Beta-Binomials",
  #     title = "BMix model selection",
  #     subtitle =
  #       paste(
  #         "Best score:", ifelse(x$use_entropy, "ICL = BIC + entropy",
  #                              "BIC")
  #       )
  #   ) +
  #   BMix:::my_ggplot_theme() +
  #   guides(fill = guide_colorbar("Score", barwidth = 6))
}
