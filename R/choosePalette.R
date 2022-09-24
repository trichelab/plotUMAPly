#' choose a predefined palette with a factor's worth of levels, or create one 
#' 
#' The Polychrome package is exceptionally useful, but it is not particularly 
#' fast or widely used. The plotUMAPly package includes a set of pre-generated
#' palettes that are widely spread out in CIE L*u*v color space, and the option
#' to generate new ones if needs be (e.g. for more than 100 factor levels, or
#' if a user wants to generate a colorblindness-safe palette; the latter may be
#' added to defaultPalettes or a similar data object if demand justifies this). 
#'
#' @param x             an integer (colors) or factor-like grouping with levels
#' @param static        use a saved static palette if available? (TRUE) 
#' @param shuffle       shuffle the default color palette (if static)? (FALSE)
#' @param seed          if static == FALSE, seed colors to use? (see Details)
#' @param ...           params passed to Polychrome::createPalette if !static
#' 
#' @details
#' Polychrome can take time to generate a palette since it samples ~50K colors.
#' This function speeds up that process by caching and optionally shuffling. 
#' The default seedcolors are FF0000, 0000FF, 00FF00 (red, blue, green) and
#' the default prefix for pre-generated color palettes is "color". Everything
#' else for the pre-generated palettes is Polychrome::createPalette defaults.
#' Pre-generated palettes for up to 100 levels are included in this package. 
#' Parameters that may be of interest when static != FALSE include `target`, 
#' which specifies a type of colorblindness to accommodate in a given palette.
#' Please see Polychrome::createPalette for more information on these. 
#'
#' The default palettes were created as follows: 
#'
#' library(parallel)
#' library(Polychrome)
#' sz <- seq_len(100)
#' names(sz) <- paste0("paletteOfSize", sz)
#' defaultPalettes <- mclapply(sz, choosePalette, static=FALSE, prefix="color")
#' 
#' Even when parallelized, this takes a few minutes, since Polychrome samples a 
#' large space of colors for each size. These are the same palettes available by
#'
#'   data("defaultPalettes")
#'
#' which also happens to be where choosePalette retrieves cached palettes from.
#'
#' @seealso Polychrome::createPalette
#' @seealso getPalette
#'
#' @export
#'
choosePalette <- function(x, static=TRUE, shuffle=FALSE, seed=c("#ff0000", "#0000ff", "#00ff00"), ...) { 

  if (length(x) == 1) {
    hasNames <- FALSE
    K <- as(x, "integer")
  } else {
    hasNames <- TRUE
    K <- nlevels(factor(x))
  }

  if (static & K <= 100) {
    pal <- getPalette(K, ...) 
  } else { 
    set.seed(K)
    message("Loading Polychrome. ", appendLF=FALSE)
    require(Polychrome)
    message("Generating a palette with ",K," color", ifelse(K > 1, "s", ""),".")
    pal <- createPalette(K, seed=seed, ...)[seq_len(K)] # weird bug for K < 4
  }

  if (shuffle) pal <- pal[sample(seq_along(pal), K)] 
  if (hasNames) names(pal) <- levels(x)
  return(pal)

}
