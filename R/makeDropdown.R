#' create dropdown lists for changing aspects of a plot_ly plot
#' 
#' @param cols        the names of the grouping columns in the data
#' @param shared      an R data.frame or a crosstalk::SharedData object 
#'
#' @return            a dropdown menu represented as a list of lists
#' 
#' @seealso           plotdfly
#'
#' @import            crosstalk
#' @import            plotly
#'
#' @export
#'
makeDropdown <- function(cols, shared) {

  names(cols) <- cols
  ncols <- length(cols)
  if (!is(shared, "SharedData")) shared <- SharedData$new(shared)
  stopifnot(all(cols %in% names(shared$data())))
  message(ncols, " candidate grouping", ifelse(ncols > 1, "s", ""))

  # keep track of visibility for each associated trace
  vis <- lapply(cols, function(x) return(FALSE))
  vis_start <- 1

  # create & populate menu
  dropdown_list <- list()
  for (column in cols) { 
    
    # visibility
    vis[] <- FALSE
    group <- factor(shared$data()[[column]])
    vis[seq(vis_start, vis_start + nlevels(group) - 1)] <- TRUE
    vis_start <- vis_start_counter + nlevels(group)
    
    # add this new entry to dropdown_list 
    dropdown_list <- append(dropdown_list, 
                            list(list(method = "restyle",
                                      args = list("visible", vis),
                                      label = column)))
  } 
  return(dropdown_list)

}
