#' specifically for plot_ly multi-grouping plots, merge multiple palettes
#' 
#' After generating (or retrieving cached) palettes of the appropriate size, 
#' it turns out that one must merge them for plot_ly to use multiple groups. 
#' Instead of generating an enormous and possibly convergent palette, it is 
#' usually better to generate individual grouping-specific palettes, and then
#' merge them. This function handles the latter and resolves any collisions. 
#' Ties (same name in different groupings) are broken by using whichever color
#' was seen first for that name. In extreme cases this may result in poor 
#' divergence of colors for duplicated names, so try not to have any. 
#'
#' @param pals          a list of palettes to merge 
#' @param ...           not currently used
#' 
#'
#' @examples
#'
#' data("defaultPalettes")
#' pals <- defaultPalettes[c(1, 10, 100)]
#' mergePalettes(pals)
#'
#' @seealso choosePalette
#' @seealso getPalette
#' 
#' @export
#'
mergePalettes <- function(pals, ...) { 

  unames <- Reduce(base::union, lapply(pals, names))
  merged <- do.call(c, unname(pals))
  if (any(duplicated(names(merged)))) {
    message("Name collisions detected in group levels. Colors may look funky.")
  }
  merged[unames]

}
