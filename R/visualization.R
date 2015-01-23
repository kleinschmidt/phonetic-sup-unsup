library(ggplot2)

#' Helper function to view classification curves
#'
#' @param .data - Data frame with at least vot, respP, bvotCond, and supCond
#' (like what is returned by load_and_parse()
plot_class_curve <- function(.data, ...) {
  ggplot(.data, aes(x=vot, y=respP, color=bvotCond, linetype=supCond), ...) +
    geom_line(stat='summary', fun.y=mean) +
    scale_x_continuous('VOT (ms)') +
    scale_y_continuous('Prob. /p/ response')
}
