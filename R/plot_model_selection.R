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
plot_model_selection = function(x)
{
  # Model selection heatmap
  ms_map = x$grid.model.selection %>%
    as_tibble() %>%
    group_by(B, BB) %>%
    summarise(best = min(ICL)) %>%
    ungroup()

  ggplot(ms_map,
         aes(x = B, y = BB, fill = best)) +
    geom_tile(aes(width = .9, height = .9)) +
    scale_fill_distiller(palette = 'Spectral', direction = -1) +
    scale_x_discrete(limits = unique(ms_map$B) %>% sort) +
    scale_y_discrete(limits = unique(ms_map$BB) %>% sort) +
    labs(
      x = 'Binomials',
      y = "Beta-Binomials",
      title = "BMix model selection",
      subtitle =
        paste(
          "Best score:", ifelse(x$use_entropy, "ICL = BIC + entropy",
                               "BIC")
        )
    ) +
    my_ggplot_theme() +
    guides(fill = guide_colorbar("Score", barwidth = 6))
}
