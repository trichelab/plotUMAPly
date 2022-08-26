#' compute the number of traces that could be laid out in a plotly plot
#' 
#' @param m   the total number of columns (dimensions) to support
#' @param d   number of dimensions to plot simultaneously (3) 
#' @param g   the number of groupings to support (1)
#' 
#' @return    how many traces this implies 
#' 
#' @examples
#'
#' numTraces(m=3, d=3, g=1) # 6     (i.e. xyz, yzx, zxy, xzy, zyx, yxz)
#' numTraces(m=6, d=3, g=3) # 360
#' numTraces(m=4, d=3, g=8) # 192
#' numTraces(m=8, d=3, g=4) # 1344
#' 
#' @export
numTraces <- function(m, d=3, g=1) {

  choose(m, d) * factorial(d) * g

}
