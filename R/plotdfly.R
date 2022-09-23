#' barebones dropdown maker for a data.frame (useful to base other methods on)
#' 
#' @param   rd          an R data.frame
#' @param   dimcols     the dimension columns in the data frame (if NULL, 1:3)
#' @param   groupcols   the names of grouping/coloring columns in the data frame
#' @param   dimlabel    stub for the dimension axis labels ("dim") 
#' @param   verbose     be verbose? (TRUE) 
#' @param   ...         parameters to pass to getPalette()
#'
#' @return              a plot_ly object
#' 
#' @seealso getPalette 
#' 
#' @examples
#' if (FALSE) { 
#'   library(singlet)
#'   get_pbmc3k_data() %>% NormalizeData %>% RunNMF %>% AnnotateNMF -> pbmc3k
#'   covs <- data.frame(cell_type = as.character(pbmc3k@meta.data$cell_type))
#'   lineages <- c(
#'                 "Naive CD4 T"="lymphoid", 
#'                 "Memory CD4 T"="lymphoid",
#'                 "CD8 T"="lymphoid",
#'                 "B"="lymphoid",
#'                 "NK"="lymphoid",
#'                 "Platelet"="myeloid",
#'                 "CD14+ Mono"="myeloid",
#'                 "FCGR3A+ Mono"="myeloid",
#'                 "DC"="myeloid" # arguably
#'                )
#'   covs$lineage <- lineages[covs$cell_type]
#'   rownames(covs) <- colnames(pbmc3k)
#'   embeddings <- pbmc3k@reductions$nmf@cell.embeddings
#'   stopifnot(identical(rownames(covs), rownames(embeddings))
#'   subembed <- embeddings[, c("NMF_11", "NMF_7", "NMF_15")]
#'   rd <- cbind(subembed, covs)
#'   plotdfly(rd, groupcols=c("cell_type","lineage"))
#' }
#'
#' @import  plotly 
#' 
#' @export 
#' 
plotdfly <- function(rd, 
                     dimcols = NULL,
                     groupcols = c("group"), 
                     dimlabel = "dim",
                     verbose = TRUE, 
                     ...) {

  # check type (switch to S3?)
  stopifnot(is(rd, "data.frame"))

  # check that all dims are available or at least room for them
  if (!is.null(dimcols)) stopifnot(all(dimcols %in% names(rd))) 
  else stopifnot(ncol(rd) >= length(groupcols) + 3)
 
  # check switchable bits 
  color_by_list <- checkcols(groupcols, rd)
  rd$plot_color <- NA_character_

  # generate as many palettes as necessary (static=TRUE may be desirable)
  pals <- lapply(color_by_list, function(i) choosePalette(rd[,i], shuffle=TRUE))

  # an empty plot
  p <- plot_ly()

  # list of booleans for trace visibilitytoggle
  visibility_list <- lapply(color_by_list, function(x) return(FALSE))

  # counter for tracking where visibility needs to change 
  vis_start_counter <- 1
  dropdown_list <- list()

  # add entries to dropdown menu
  for (grouping in names(color_by_list)) { 
    
    # get name of dropdown item 
    color_by <- color_by_list[grouping]

    # set current trace as group and label
    rd$group <- factor(rd[, color_by])
    rd$plot_label <- paste0(rownames(rd), "(", rd$group, ")")
    
    # create option for selection dropdown: set up visibility_list
    visibility_list[] <- FALSE
    clevels <- nlevels(rd$group)
    idx <- seq(vis_start_counter, vis_start_counter + clevels - 1)
    visibility_list[idx] <- TRUE

    # increment the trace counter 
    vis_start_counter <- vis_start_counter + nlevels(rd$group)
    
    # add a palette of appropriate cardinality
    pal <- pals[[grouping]]
    names(pal) <- levels(rd$group)
    
    # build dropdown item
    dropdown_entry <- list(
      method = 'restyle',
      args = list("visible", visibility_list),
      label = grouping
    )

    # add item to dropdown_list
    dropdown_list <- append(dropdown_list, list(dropdown_entry))
    
    #create trace, only 1st trace starts visible
    p <- add_trace(p, 
                   rd,
                   mode = "markers",
                   x = rd$X,
                   y = rd$Y,
                   z = rd$Z,
                   text = rd$plot_label, 
                   hoverinfo = "text", 
                   type = "scatter3d",
                   color = rd$group,
                   colors = pal)
    # }}}
  } 
  

  # do not show menu if single metadata option
  axes <- lapply(list(x=1, y=2, z=3), cleanAxis, embed = dimlabel)
  if (length(dropdown_list) > 1) {
    names(dropdown_list) <- color_by_list
    p <- layout(p, updatemenus = list(list(y = 0.9, buttons = dropdown_list)))
  } else {
    p <- layout(p)
  }

  config(p, displayModeBar = FALSE)

}



# helper function 
checkcols <- function(cols, rd, what="grouping", verbose=TRUE) { 

  stopifnot(all(cols %in% names(rd))) 
  ncols <- length(cols)
  if (verbose) message(ncols, " candidate ", what, ifelse(ncols > 1, "s", ""))
  names(cols) <- cols # for debugging
  return(cols) 

}
