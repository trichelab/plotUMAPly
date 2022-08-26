#' choose a predefined palette with a factor's worth of levels, or create one 
#' 
#' The Polychrome package is exceptionally useful, but it is not particularly 
#' fast or widely used. The plotUMAPly package includes a set of pre-generated
#' palettes that are widely spread out in CIE L*u*v color space, and the option
#' to generate new ones if needs be (e.g. for more than 100 factor levels, or
#' if a user wants to generate a colorblindness-safe palette; the latter may be
#' added to defaultPalettes or a similar data object if demand justifies this). 
#'
#' @param x             a factor (will be converted to one if it isn't)
#' @param static        use a saved static palette if available? (TRUE) 
#' @param shuffle       shuffle the default color palette (if static)? (FALSE)
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
#'   library(Polychrome)
#'   seed <- c("#ff0000", "#0000ff", "#00ff00") 
#'   newpal <- function(x) createPalette(x, seedcolors=seed, prefix="color")
#'   defaultPalettes <- lapply(1:100, newpal)
#' 
#' These are the same palettes available via
#'
#'   data("defaultPalettes", package="plotUMAPly")
#'
#' which also happens to be where choosePalette retrieves cached palettes from.
#'
#' @export
choosePalette <- function(x, static=TRUE, shuffle=FALSE, ...) { 

  x <- factor(x)
  K <- nlevels(x)

  if (!static | K > 100) {

    message("Loading Polychrome...")
    require(Polychrome)
    message("Generating a new palette with ", K, " colors...")
    pal <- createPalette(K, ...) 

  } else { 

    message("Loading pre-generated palette with ", K, " colors...")
    if (!exists("defaultPalettes")) {
      data("defaultPalettes", package="plotUMAPly")
    }
    pal <- defaultPalettes[[K]]

  }
 
  if (shuffle) {
    pal <- pal[sample(seq_along(pal), K)] 
  }

  names(pal) <- levels(x)
  return(pal)

}
