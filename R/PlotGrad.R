#' Plot Gradiente profile
#'
#' Plot the LC gradient profile.
#'
#' @param x data frame, gradient information previously imported with GradInput function.
#'
#' @return The graph of gradient profile.
#'
#' @examples
#' \dontrun{PlotGrad(x)}
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 geom_area
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_rect
#'
#' @export
#' @author Estevan Bruginski \email{estevan.bruginski@ufpr.br}
#' Universidade Federal do Paraná
#' License: MIT + file LICENSE
#'
PlotGrad <- function(x) {
  #create the group composition
  comp <- c(rep("A", nrow(x)), rep("B", nrow(x)));

  #select the phase A
  phase.A <- cbind(x[, 1], x[, 3]);

  phase.B <- cbind(x[, 1], x[, 4]);

  # merge the phases A and B
  phase.AB <- rbind(phase.A, phase.B);

  # group the phase A and B
  phase.grp <- data.frame(phase.AB, comp);

  # graph
  p <-
    ggplot2::ggplot(phase.grp,
                    ggplot2::aes(x = phase.grp[, 1] , y = phase.grp[, 2],  fill = phase.grp[, 3])) +
    ggplot2::labs(x = "Time (min)", y = "Composition (%)", fill = "") +
    ggplot2::geom_area(alpha = 0.7) +
    ggplot2::theme(text = ggplot2::element_text(size = 16), panel.border = ggplot2::element_rect(fill = .1, colour = "black")) +
    ggplot2::scale_x_continuous(breaks = round(seq(min(phase.grp[, 1]), max(phase.grp[, 1]),
                                                   by = 5), 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = round(seq(min(10), max(100), by = 10), 1),
                                expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = c("#3c8dbc", "#9F0607"))


  return(p)
}
