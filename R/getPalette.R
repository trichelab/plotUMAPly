#' choose a predefined palette with K levels, for any K up to 100
#' 
#' The Polychrome package is exceptionally useful, but it is not particularly 
#' fast or widely used. getPalette() makes its most useful functionality fast.
#'
#' @param K             how many colors we need
#' @param ...           not currently used
#' 
#' @details
#' Polychrome can take time to generate a palette since it samples ~50K colors.
#' This function speeds up that process by pulling appropriate 
#' The default seedcolors are FF0000, 0000FF, 00FF00 (red, blue, green) and
#' the default prefix for pre-generated color palettes is "color". Everything
#' else for the pre-generated palettes is Polychrome::createPalette defaults.
#' Pre-generated palettes for up to 100 levels are included in this package. 
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
#' The choosePalette() function calls out to getPalette if static=TRUE.
#'
#' @seealso mergePalettes
#' @seealso choosePalette
#' 
#' @export
getPalette <- function(K, shuffle=FALSE, ...) { 

  if (!exists("defaultPalettes")) data("defaultPalettes")
  stopifnot(K <= length(defaultPalettes))
  pal <- defaultPalettes[[K]]
  return(pal)

}
