#' helper function for plotfly
#' 
#' Most of the time, we don't want tick marks on the axes of our 3D plots. 
#' This function solves that problem so we don't have to think about it.
#' 
#' @param i       the axis (required; usually 1, 2, or 3) 
#' @param dimcols the dimension column names from the user (or their data.frame)
#' 
#' @return        a list (see Details) 
#' 
#' @details 
#' The returned list has elements 'title' (a character string), and also
#' 'showticklabels', 'zeroline', 'showline', and 'showgrid' (zeroline is TRUE).
#' This is then passed to plotly to generate a clean set of axis stylings.
#' 
#' @export
cleanAxis <- function(i, dimcols) {

  res <- list(title=dimcols[i], 
              showticklabels = FALSE,
              zeroline = FALSE,
              showline = FALSE,
              showgrid = FALSE)
  return(res)

}
