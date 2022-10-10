#' barebones dropdown maker for a data.frame turned into a plot_ly 3D scatter
#' 
#' @param   rd          an R data.frame
#' @param   dimcols     the dimension columns in the data frame (if NULL, 1:3)
#' @param   groupcols   the names of grouping/coloring columns in the data frame
#' @param   ...         parameters to pass to getPalette()
#'
#' @return              a plot_ly object
#' 
#' @examples
#'
#' if (FALSE) { 
#'
#'   library(singlet)
#'   get_pbmc3k_data() %>% NormalizeData %>% RunNMF %>% AnnotateNMF -> pbmc3k
#'   covs <- data.frame(cell_type = as.character(pbmc3k@meta.data$cell_type))
#'   lineages <- c("Naive CD4 T"="lymphoid", 
#'                 "Memory CD4 T"="lymphoid",
#'                 "CD8 T"="lymphoid",
#'                 "B"="lymphoid",
#'                 "NK"="lymphoid",
#'                 "Platelet"="myeloid",
#'                 "CD14+ Mono"="myeloid",
#'                 "FCGR3A+ Mono"="myeloid",
#'                 "DC"="myeloid") # arguably
#'   covs$lineage <- lineages[covs$cell_type]
#'   rownames(covs) <- colnames(pbmc3k)
#'   embeddings <- pbmc3k@reductions$nmf@cell.embeddings
#'   stopifnot(identical(rownames(covs), rownames(embeddings)))
#'   subembed <- embeddings[, c("NMF_11", "NMF_7", "NMF_15")]
#'   rd <- cbind(subembed, covs)
#'   plotdfly(rd, groupcols=c("cell_type","lineage"))
#'
#' }
#'
#' @import  crosstalk
#' @import  plotly 
#' 
#' @export 
#' 
plotdfly <- function(rd, dimcols = NULL, groupcols = c("group"), ...) {

  # check type (switch to S3?)
  stopifnot(is(rd, "data.frame"))

  # check that all dims are available or at least room for them
  if (!is.null(dimcols)) stopifnot(all(dimcols %in% names(rd))) 
  else stopifnot(ncol(rd) >= length(groupcols) + 3)
  if (is.null(dimcols)) dimcols <- names(rd)[1:3]
  if (is.null(dimcols)) dimcols <- c("dim1", "dim2", "dim3")

  # make a shared data object
  shared <- SharedData$new(rd)
  dropdown <- makeDropdown(groupcols, shared=shared)
  updatemenus <- list(list(y = 0.9, buttons=dropdown))
  axes <- lapply(list(x=1, y=2, z=3), cleanAxis, dimcols=dimcols)
  p <- addTraces(groupcols, dimcols=dimcols, shared=shared)

  # suppress if only one grouping
  if (length(groupcols) == 1) layout(p, scene = axes)
  else layout(p, updatemenus = updatemenus, scene = axes)

}
