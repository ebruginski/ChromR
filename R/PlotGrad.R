#' Plot Gradiente profile
#'
#' Plot the LC gradient profile.
#'
#' @param grad data frame, gradient information previously imported with GradInput function.
#'
#' @return The graph of gradient profile.
#'
#' @example PlotGrad(grad)
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_area
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#'
#' @export
PlotGrad <- function(grad){
  #create the group composition
  comp <- c(rep("A", nrow(grad)),rep("B", nrow(grad)));
  #select the phase A
  phase.A <- cbind(grad[,1], grad[,3]);
  phase.B <- cbind(grad[,1], grad[,4]);
  #merge the phases A and B
  phase.AB <- rbind(phase.A,phase.B);
  #group the phase A and B
  phase.grp <- data.frame(phase.AB,comp);

  grad.plot <- ggplot2::ggplot(phase.grp) +
    ggplot2::aes(x = phase.grp[,1] , y = phase.grp[,2], fill = phase.grp[,3]) +
    ggplot2::geom_area(size = 10L, alpha = 0.7) +
    ggplot2::scale_fill_manual(values=c("#332382", "#CD0000")) +
    ggplot2::labs(x = "Time", y = " ", fill = " ") +
    ggplot2::theme_minimal(base_line_size = 1.1) +
    ggplot2::theme(legend.position = "top", axis.text=ggplot2::element_text(size=14), axis.title.x = element_text(size=18));
  return(grad.plot);
}
