#' add 3D scatterplot traces to a plotly plot, each corresponding to a dropdown
#' 
#' @param cols        the names of the grouping columns in the data
#' @param dimcols     the names of the dimensions columns in the data
#' @param shared      an R data.frame or a crosstalk::SharedData object 
#'
#' @return            a plotly plot object with at least one 3D plot trace
#' 
#' @seealso           plotdfly
#' @seealso           choosePalette
#' @seealso           mergePalettes
#'
#' @import            crosstalk
#' @import            plotly
#'
#' @export
#'
addTraces <- function(cols, dimcols, shared) {

  names(cols) <- cols
  if (!is(shared, "SharedData")) shared <- SharedData$new(shared)
  stopifnot(all(cols %in% names(shared$data())))

  # generate and merge color palettes 
  pals <- lapply(cols, function(i) choosePalette(shared$data()[[i]], shuf=TRUE))
  pal <- mergePalettes(pals)

  # create the plot
  p <- plotly_empty()
  for (column in cols) { 
    group <- factor(shared$data()[[column]])
    label <- paste0(rownames(shared$data()), " (", group, ")")
    p <- add_trace(p, 
                   data = shared,
                   x = shared$data()[[dimcols[1]]],
                   y = shared$data()[[dimcols[2]]],
                   z = shared$data()[[dimcols[3]]],
                   mode = "markers",
                   hoverinfo = "text", 
                   type = "scatter3d",
                   text = label, 
                   color = group,
                   colors = pal)
  } 
  return(p)

}
